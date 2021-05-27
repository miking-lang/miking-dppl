-- CorePPL compiler, targeting the RootPPL framework

include "../coreppl/coreppl.mc"
-- include "../models/crbd.mc"

include "rootppl.mc"

include "mexpr/ast-builder.mc"

include "assoc-seq.mc"
include "name.mc"

include "c/ast.mc"
include "c/pprint.mc"
include "c/compile.mc"

-----------------------------------------
-- BASE LANGUAGE FRAGMENT FOR COMPILER --
-----------------------------------------

lang MExprPPLRootPPLCompile = MExprPPL + RootPPL + MExprCCompile

  -- Compiler internals
  syn CExpr =
  | CEAlloc {} -- Allocation placeholder
  | CEResample {} -- Indicates resample locations

  sem printCExpr (env: PprintEnv) =
  | CEAlloc {} -> (env, "<<<CEAlloc>>>")
  | CEResample {} -> (env, "<<<CEResample>>>")

  sem sfold_CExpr_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CEAlloc _ -> acc
  | CEResample _ -> acc

  sem smap_CExpr_CExpr (f: CExpr -> CExpr) =
  | CEAlloc _ & t -> t
  | CEResample _ & t -> t

  -- ANF override
  -- Ensures argument to assume is not lifted by ANF (first-class distributions
  -- not supported in RootPPL)
  sem normalize (k : Expr -> Expr) =
  | TmAssume ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmAssume { t with dist = TmDist { td with dist = dist } }))
      dist

  -- Extensions
  sem compileDist =
  | DBernoulli { p = p } -> CDBern { p = compileExpr p }
  | DBeta { a = a, b = b } -> CDBeta { a = compileExpr a, b = compileExpr b }
  | DCategorical { p = p } -> CDCategorical { p = compileExpr p }
  | DMultinomial { n = n, p = p } ->
    CDMultinomial { n = compileExpr n, p = compileExpr p }
  | DDirichlet { a = a } -> CDDirichlet { a = compileExpr a }
  | DExponential { rate = rate } -> CDExp { rate = compileExpr rate }
  | DEmpirical { samples = samples } ->
    CDEmpirical { samples = compileExpr samples }

  -- Extensions
  sem compileExpr =
  | TmAssume _ -> error "Assume without direct distribution"
  | TmAssume { dist = TmDist { dist = dist }} ->
    CESample { dist = compileDist dist }
  | TmWeight { weight = weight } -> CEWeight { weight = compileExpr weight }
  | TmResample _ -> CEResample {}

  -- Allocation
  sem alloc (name: Name) =
  -- | CTyVar { id = tyName } & ty ->
  | CTyPtr { ty = CTyStruct { id = Some ident, mem = None() } & ty } & ptrTy ->
    let allocName = nameSym "alloc" in
    [
      -- Placeholder definition of pointer to allocated struct
      { ty = ptrTy
      , id = Some name
      , init = Some (CIExpr { expr = CEAlloc {} })
      }
    ]
  | _ -> error "Incorrect type in alloc"

end

----------------------
-- ROOTPPL COMPILER --
----------------------

let printCompiledRPProg = use MExprPPLRootPPLCompile in
  lam rpprog: RPProg.
    -- TODO Should really cCompilerNames be used here?
    printRPProg cCompilerNames rpprog

-- Compiler entry point.
let rootPPLCompileH: [(Name,Type)] -> [Name] -> Expr -> RPProg =
  use MExprPPLRootPPLCompile in
  lam typeEnv: [(Name,Type)].
  lam globalNames: [Name].
  lam prog: Expr.

    -------------------------
    -- COMPONENT FUNCTIONS --
    -------------------------

    -- Stackframe type, containing relevant names and types
    type StackFrame = {
      -- * Each stack frame begins with (not explicitly included) a BBLOCk int
      -- return address.
      -- * Parameters for current function
      params: [(Name,CType)],
      -- * Return type (pointer to return type is passed as last argument if
      -- function does not return void)
      ret: CType,
      -- * Locals pointing directly to allocated data.
      localAllocs: [(Name,CType)],
      -- * Accumulated locals that traverse block boundaries
      locals: [(Name,CType)]
    } in

    let emptySF: CType -> StackFrame =
      lam ret. { params = [], localAllocs = [], locals = [], ret = ret}
    in

    -- Accumulator used when determining stack frames
    type AccStackFrames = {
      -- Current stack frame
      sf: StackFrame,
      -- Names defined in current block
      defs: [(Name,CType)],
      -- Names defined in previous blocks for a function
      prevDefs: [(Name,CType)],
      -- Indicates whether or not a list of CStmt contains a BBLOCK split
      hasSplit: Bool
    } in

    let emptyAccSF: StackFrame -> AccStackFrames = lam sf: StackFrame.
      { sf = sf, defs = [], prevDefs = [], hasSplit = false }
    in

    -- Add a local alloc to accumulator
    let addLocalAlloc: AccStackFrames -> (Name,CType) -> AccStackFrames =
      lam acc: AccStackFrames. lam e: (Name,CType).
        -- Guaranteed to not already exist, so no check needed here
        {acc with sf = {acc.sf with localAllocs = snoc acc.sf.localAllocs e}}
    in

    -- Add all local variables used in an expression that traverse BBLOCK
    -- boundaries to the accumulator
    recursive let addLocals: AccStackFrames -> CExpr -> AccStackFrames =
      lam acc: AccStackFrames. lam expr: CExpr.
        let lookup = assocSeqLookup {eq = nameEq} in
        let sf = acc.sf in
        match expr with CEVar { id = id } then
          match lookup id sf.params with Some _ then acc
          else match lookup id sf.locals with Some _ then acc
          else match lookup id sf.localAllocs with Some _ then acc
          else match lookup id acc.prevDefs with Some ty then
            {acc with sf = {sf with locals = snoc sf.locals (id,ty)}}
          else acc
        else sfold_CExpr_CExpr addLocals acc expr
    in

    -- Mark the end of a BBLOCK in the accumulator
    let nextBBlock: AccStackFrames -> AccStackFrames = lam acc: AccStackFrames.
      {{{ acc with defs = [] }
              with prevDefs = concat acc.defs acc.prevDefs }
              with hasSplit = true }
    in

    -- Given an initial accumulator (see above), get the locals used in a
    -- function (= list of statements)
    recursive let getLocals: AccStackFrames -> [CStmt] -> AccStackFrames =
      lam acc: AccStackFrames. lam stmts: [CStmt].
        let acc = {acc with hasSplit = false} in
        foldl (lam acc: AccStackFrames. lam stmt: Stmt.

          -- Add def, if applicable
          let acc =
            match stmt with CSDef { ty = ty, id = Some name }
            then { acc with defs = cons (name,ty) acc.defs } else acc
          in

          -- Always add pointers to allocated structs to locals, since these
          -- might implicitly traverse block boundaries through other
          -- variables.
          let acc =
            match stmt with CSDef {
              ty = ty,
              id = Some id,
              init = Some (CIExpr { expr = CEAlloc {} })
            } then addLocalAlloc acc (id,ty) else acc
          in

          -- Add used locals from prevDef
          let acc =
            match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then
              sfold_CStmt_CExpr addLocals acc stmt
            else match stmt with CSIf { cond = cond } then
              addLocals acc cond
            else error "Unsupported term when adding locals"
          in

          -- Block split
          match stmt with CSDef { init = Some (CIExpr { expr = CEApp _ }) }
                        | CSExpr { expr = CEApp _ }
                        | CSExpr { expr = CEResample _ } then
            nextBBlock acc

          -- If statement, requires special handling
          else match stmt with CSIf { thn = thn, els = els } then
            let accThn = getLocals acc thn in
            let accEls = getLocals {acc with sf = accThn.sf} els in
            let acc = {acc with sf = accEls.sf} in
            if or (accThn.hasSplit) (accEls.hasSplit) then nextBBlock acc
            else acc

          -- Rest
          else match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then acc

          else error "Unsupported term in getLocals"

        ) acc stmts
    in

    -- Recurse over all top-level definitions and determine stack frames for
    -- functions
    let getStackFrames: [CTop] -> [(Name,StackFrame)] =
      lam tops: [CTop].
        foldl (lam acc: [(Name,StackFrame)]. lam top: CTop.
          match top
          with CTFun { ret = ret, id = id, params = params, body = body } then

            -- Initialize accumulator for new function
            let sf =
              { (emptySF ret) with params = map (lam t. (t.1,t.0)) params } in
            let lAcc = emptyAccSF sf in

            -- Get locals that traverse BBLOCK boundaries in function
            let lAcc = getLocals lAcc body in

            -- Record stack frame
            snoc acc (id,lAcc.sf)

          else acc) [] tops
    in

    -- Get stack frame for initialization code
    let getInitStackFrame: [CStmt] -> CType -> StackFrame =
      lam stmts: [CStmt]. lam ret: CType.
        let acc = getLocals (emptyAccSF (emptySF ret)) stmts in
        acc.sf
    in

    -- Type used for indicating what action to take at endpoints.
    -- Collapse => Collapse stack
    -- Block => Jump to block. Block name can be either predetermined (Some _)
    -- or generated at the endpoint (None _)
    type Next in
    con Collapse: () -> Next in
    con Block: Option Name -> Next in

    -- Accumulator used when splitting function into BBLOCKs
    type AccSplit = {
      -- Indicates what to do at an endpoint
      next: Next,
      -- Indicates if a split has occured at the end of current block
      hasSplit: Bool,
      -- Accumulated block
      block: [CStmt],
      -- Stack frame for the current function
      sf: StackFrame,
      -- Bindings from names to stack frames
      sfs: [(Name,StackFrame)],
      -- Accumulated set of top-level definitions
      tops: [CTop]
    } in
    let emptyAccSplit =
      { next = Collapse (), hasSplit = false, block = [], sf = emptySF
      , sfs = [], tops = [] }
    in

    -- Stackframe lookup
    let getStackFrame: Name -> [(Name,StackFrame)] -> StackFrame =
      lam id: Name. lam sfs: [(Name,StackFrame)].
        match assocSeqLookup {eq=nameEq} id sfs with Some sf then sf
        else "Stack frame does not exist in getStackFrame"
    in

    -- C statement for setting the PC to a name
    let setPCFromExpr: CExpr -> CExpr = lam expr.
      (CSExpr { expr = (CEBinOp { op = COAssign {}, lhs = CEPC {}
                                , rhs = expr })})
    in

    -- C statement for setting the PC to a name
    let setPC: Name -> CExpr = lam name.
      setPCFromExpr (CEBBlockName { name = name })
    in


    -- Used when reaching endpoints, ensures the accumulator has a name for the
    -- next block.
    let initNextName: AccSplit -> AccSplit = lam acc: AccSplit.
      match acc.next with Block (Some name) then acc
      else match acc.next with Block (None ()) then
        {acc with next = Block (Some (nameSym "lazy"))}
      else error "Error in initNextName"
    in

    -- Get the name for the next block from accumulator
    let getNextName: AccSplit -> Name = lam acc: AccSplit.
      match acc.next with Block (Some name) then name
      else error "Error in getNextName"
    in

    -- Name for return adress variable
    let nameRetAddr = nameSym "ra" in
    -- Name for return value pointer
    let nameRetLoc = nameSym "retLoc" in

    -- Various compiler names
    -- TODO(dlunde,2021-05-25): Must be guaranteed unique somehow
    let nameBblocksArr = nameSym "bblocksArr" in
    let nameBblockCall = nameSym "BBLOCK_CALL" in
    let nameDataPointer = nameSym "DATA_POINTER" in
    let nameNull = nameSym "NULL" in

    let callBBlock = lam name: Name.
      -- BBLOCK_CALL(name)
      CSExpr { expr = CEApp {
        fun = nameBblockCall, args = [CEVar {id = name}, CEVar {id = nameNull}]
      } }
    in

    let callFromExpr = lam expr: CExpr.
        -- BBLOCK_CALL(DATA_POINTER(bblocksArr)[PC])
        CSExpr { expr = CEApp {
          fun = nameBblockCall, args = [
            CEBinOp {
              op = COSubScript {},
              lhs = CEApp {
                fun = nameDataPointer,
                args = [ CEVar { id = nameBblocksArr}]
              },
              rhs = expr
            },
            CEVar { id = nameNull }
          ] }
        }
    in

    -- Stack pointer expression
    let stackPtr = CEMember { lhs = CEPState {}, id = "stackPtr" } in

    -- Unwraps pointer type one step
    let tyDeref: CType -> CType = lam ty: CType.
      match ty with CTyPtr { ty = ty } then ty
      else error "Non-pointer in tyDeref"
    in

    let derefExpr: CExpr -> CExpr = lam expr.
      CEUnOp { op = CODeref {}, arg = expr}
    in

    -- Fins used variables in a list of statements
    let usedVars: [CStmt] -> [Name] = lam stmts.
      recursive let rec = lam acc: [Name]. lam expr: CExpr.
        match expr with CEVar { id = id } then cons id acc
        else sfold_CExpr_CExpr rec acc expr
      in foldl (sfold_CStmt_CExpr rec) [] stmts
    in

    -- Construct a BBLOCK from the currently accumulated statements
    let createBlock: StackFrame -> Name -> AccSplit -> AccSplit =
      lam sf: StackFrame. lam name: Name. lam acc: AccSplit.

        -- Used for avoiding some unnecessary code
        let uses = usedVars acc.block in

        type Acc = { stmts: [CStmt], pos: CExpr } in

        let f: Acc -> Name -> CType -> Bool -> Acc =
          lam lAcc: Acc. lam n: Name. lam ty: Type. lam ptr: Bool.
            let pos = CEBinOp {
              op = COSub {},
              lhs = lAcc.pos,
              rhs = CESizeOfType { ty = if ptr then ty else tyDeref ty }
            } in
            let ty = if ptr then CTyPtr { ty = ty } else ty in
            if any (nameEq n) uses then
              let stmt = CSDef {
                ty = ty,
                id = Some n,
                init = Some (CIExpr {
                  expr =
                    CECast { ty = ty, rhs = pos }
                })
              }
              in {{ lAcc with stmts = cons stmt lAcc.stmts } with pos = pos }
            else { lAcc with pos = pos }
        in

        let lAcc: Acc = { stmts = [], pos = stackPtr } in

        let lAcc =
          foldr (lam t. lam lAcc. f lAcc t.0 t.1 true) lAcc sf.locals in
        let lAcc =
          foldr (lam t. lam lAcc. f lAcc t.0 t.1 false) lAcc sf.localAllocs in
        let lAcc =
          match sf.ret with CTyVoid _ then lAcc
          else f lAcc nameRetLoc (CTyPtr { ty = sf.ret }) true in
        let lAcc =
          foldr (lam t. lam lAcc. f lAcc t.0 t.1 true) lAcc sf.params in
        let lAcc = f lAcc nameRetAddr (CTyInt {}) true in

        let bb = CTBBlock { id = name, body = concat lAcc.stmts acc.block } in
        {acc with tops = snoc acc.tops bb}
    in

    let frameTypes: StackFrame -> [CType] = lam sf: StackFrme.
      join [
        [CTyInt {}],
        map (lam t. t.1) sf.params,
        match sf.ret with ! CTyVoid _ then [CTyPtr { ty = sf.ret }] else [],
        map (lam t. tyDeref t.1) sf.localAllocs,
        map (lam t. t.1) sf.locals
      ]
    in

    -- Generate code for collapsing a stack frame.
    let modStackPtr: CBinOp -> StackFrame -> CStmt =
      lam op: CBinOp. lam sf: StackFrame.
        let f: CExpr -> CType -> CExpr = lam acc. lam ty.
          CEBinOp {
            op = op,
            lhs = acc,
            rhs = CESizeOfType { ty = ty }
          }
        in
        let rhs = foldl f stackPtr (frameTypes sf) in
        CSExpr { expr = CEBinOp { op = COAssign {}, lhs = stackPtr, rhs = rhs} }
    in

    let decrementStackPtr = modStackPtr (COSub {}) in
    let incrementStackPtr = modStackPtr (COAdd {}) in

    -- Generate code for calling a BBLOCK
    let constructCall:
        StackFrame -> Name -> [CExpr] -> Option CExpr -> Name -> [CStmt] =
      lam sf: StackFrame.
      lam fun: Name.
      lam args: [CExpr].
      lam ret: Option CExpr.
      lam ra: CExpr.

        type Acc = { stmts: [CStmt], pos: CExpr } in

        let f: Acc -> CExpr -> CType -> Acc =
          lam acc: Acc. lam e: CExpr. lam ty: Type.
            let stmt = CSExpr { expr = CEBinOp {
              op = COAssign {},
              lhs = CEUnOp {
                op = CODeref {},
                arg = CECast {
                  ty = CTyPtr { ty = ty },
                  rhs = acc.pos
                }
              },
              rhs = e
            }} in
            let pos = CEBinOp {
              op = COAdd {},
              lhs = acc.pos,
              rhs = CESizeOfType { ty = ty }
            } in
            {{ acc with stmts = snoc acc.stmts stmt } with pos = pos }
        in

        let acc: Acc = { stmts = [], pos = stackPtr } in

        -- Set return address
        -- *((int*) stackPtr) = ra;
        let acc = f acc ra (CTyInt {}) in

        -- *((tyarg1*) (stackPtr + sizeof(tyarg1))) = arg1;
        -- *((tyarg2*) (stackPtr + sizeof(tyarg1) + sizeof(tyarg2))) = arg2;
        -- ...
        let args = zipWith (lam p. lam a. (a,p.1)) sf.params args in
        let acc = foldl (lam acc. lam a. f acc a.0 a.1) acc args in

        -- *((rettype**) (stackPtr + ... + sizeof(rettype*))) = &retLoc;
        let acc =
          match ret with Some expr then
            f acc (CEUnOp { op = COAddrOf {}, arg = expr})
              (CTyPtr { ty = sf.ret })
          else acc in

        -- stackPtr = stackPtr +
        --   sizeof(int) + // RA
        --   sizeof(tyarg1) + sizeof(tyarg2) + ... + // Arguments
        --   sizeof(rettype*) +
        --   sizeof(local1) + sizeof(local2) + ... // Locals
        --
        let incr = incrementStackPtr sf in

        -- BBLOCK_CALL(.);
        let call = callBBlock fun in

        join [acc.stmts, [incr, call]]
    in

    let constructCallFromStmt: StackFrame -> CStmt -> Name -> [Cstmt] =
      lam sf: StackFrame.
      lam stmt: CStmt.
      lam ra: CExpr.
        let app: { fun: Name, args: [CExpr] } =
          match stmt with CSDef { init = Some (CIExpr { expr = CEApp app }) }
             | CSExpr { expr = CEBinOp { op = COAssign {}, rhs = CEApp app } }
             | CSExpr { expr = CEApp app }
          then app else error "Non-application in constructCallFromStmt"
        in
        let ret: Option CExpr =
          match stmt with
            CSExpr { expr = CEBinOp {
              op = COAssign {}, lhs = expr, rhs = CEApp _ }
            } then
            match sf.ret with ! CTyVoid _ then Some expr
            else error "Error in constructCallFromStmt"
          else None ()
        in
        constructCall sf app.fun app.args ret ra
    in

    -- Split a function (= list of statements) into BBLOCKs
    recursive let splitFunBody: AccSplit -> [CStmt] -> AccSplit =
      lam acc: AccSplit. lam stmts: [CStmt].

        match stmts with [stmt] ++ stmts then

          -- Function application
          match stmt
          with CSDef { init = Some (CIExpr { expr = CEApp app }) }
             | CSExpr { expr = CEBinOp { op = COAssign {}, rhs = CEApp app } }
             | CSExpr { expr = CEApp app }
          then
            let acc = {acc with hasSplit = true} in
            let funStackFrame = getStackFrame app.fun acc.sfs in
            match (stmts, acc.next) with ([], Block _) then
              -- Special handling of this call avoids an empty BBLOCK
              let acc = initNextName acc in
              let ra = getNextName acc in
              let call = constructCallFromStmt funStackFrame stmt
                           (CEBBlockName { name = ra }) in
              let block = concat acc.block call in
              {acc with block = block}
            else
              -- CASE: Not last statement, but end of block
              let accNext = {acc with block = []} in
              let accNext = splitFunBody accNext stmts in
              let ra = nameSym "bblock" in
              let accNext = createBlock acc.sf ra accNext in

              let call = constructCallFromStmt funStackFrame stmt
                           (CEBBlockName { name = ra }) in
              let block = concat acc.block call in
              {accNext with block = block}

          -- Resample
          else match stmt with CSExpr { expr = CEResample _ } then
            let acc = {acc with hasSplit = true} in
            match stmts with [] then
              -- CASE: Resample as last statment _and_ end of block
              match acc.next with Collapse _ then
                let collapse = decrementStackPtr acc.sf in
                let block = snoc acc.block collapse in
                {acc with block =
                   snoc block (setPCFromExpr
                                 (derefExpr (CEVar { id = nameRetAddr }))) }
              else match acc.next with Block _ then
                -- Set PC to next block
                let acc = initNextName acc in
                let name = getNextName acc in
                {acc with block = snoc acc.block (setPC name)}
              else never
            else
              -- CASE: Not last statement, but end of block
              let accNext = {acc with block = []} in
              let accNext = splitFunBody accNext stmts in
              let name = nameSym "bblock" in
              let accNext = createBlock acc.sf name accNext in
              -- Simply set PC to next block
              {accNext with block = snoc acc.block (setPC name)}

          -- Write to return value
          else match stmt with CSRet { val = Some val } then
            -- *(retLoc) = val
            let stmt = CSExpr { expr = CEBinOp {
              op = COAssign {},
              lhs = derefExpr (derefExpr (CEVar { id = nameRetLoc })),
              rhs = val
            }}
            in splitFunBody {acc with block = snoc acc.block stmt} stmts

          -- Not a function application, resample, or return. Just accumulate
          -- and continue
          else match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then
            splitFunBody {acc with block = snoc acc.block stmt} stmts

          -- If statement
          else match stmt with CSIf { cond = cond, thn = thn, els = els } then
            let next = match stmts with [] then acc.next else Block (None ()) in
            let accThn = splitFunBody {{{acc with block = []}
                                             with hasSplit = false}
                                             with next = next} thn in
            let accEls = splitFunBody {{accThn with block = []}
                                               with hasSplit = false} els in

            -- No split in branches, just continue
            if and (not accThn.hasSplit) (not accEls.hasSplit) then
              let stmt =
                CSIf { cond = cond, thn = accThn.block, els = accEls.block }
              in
              splitFunBody {acc with block = snoc acc.block stmt} stmts

            -- At least one split in branches
            else
              let update = lam acc: AccSplit.
                match accEls.next with Block (Some name) then
                  if acc.hasSplit then acc
                  else {acc with block = snoc acc.block (callBBlock name)}
                else acc
              in
              let accThn = update accThn in
              let accEls = update accEls in
              let stmt =
                CSIf { cond = cond, thn = accThn.block, els = accEls.block } in
              let block = snoc acc.block stmt in

              -- Create new block for stmts if needed
              let accStmts = {{ accEls with hasSplit = true }
                                       with next = acc.next } in
              match stmts with [] then {accStmts with block = block}
              else
                let accStmts = {accStmts with block = []} in
                let accStmts = splitFunBody accStmts stmts in
                let name = getNextName accEls in
                let accStmts = createBlock acc.sf name accStmts in
                {accStmts with block = block}

          else error "Not supported in splitFunBody"

        -- End of block without split
        else match stmts with [] then

          match acc.next with Collapse _ then
            let collapse = decrementStackPtr acc.sf in
            let block = snoc acc.block collapse in
            {acc with block =
               snoc block
                 (callFromExpr (derefExpr (CEVar { id = nameRetAddr }))) }

          else match acc.next with Block _ then
            if acc.hasSplit then
              let acc = initNextName acc in
              let name = getNextName acc in
              {acc with block = snoc acc.block (callBBlock name)}
            else acc
          else never

        else never
    in

    -- Split a function
    let splitFunction: AccSplit -> CTop -> AccSplit =
      lam acc: AccSplit. lam top: CTop.
        match top with CTFun { id = id, body = body } then
          let acc = {{{ acc with next = Collapse ()}
                            with block = []}
                            with sf = getStackFrame id acc.sfs} in
          let acc = splitFunBody acc body in
          createBlock acc.sf id acc
        else error "Non-CTFun in splitFunction"
    in

    -- Name for the initial BBLOCK
    let nameInit: Name = nameSym "init" in

    -- Split the init function
    let splitInit: AccSplit -> StackFrame -> [CStmt] -> CTop =
      lam acc: AccSplit. lam sf: StackFrame. lam stmts: [CStmt].
        let acc = {{{ acc with next = Collapse ()}
                          with block = []}
                          with sf = sf} in
        let acc = splitFunBody acc stmts in
        match createBlock acc.sf nameInit acc with { tops = tops } then tops
        else never
    in

    -- Iterate over top-level definitions and split functions into BBLOCKs
    -- NOTE(dlunde,2021-05-21): Currently, _all_ functions are split. Later on,
    -- we only want to split where necessary.
    let splitFunctions: AccSplit -> [CTop] -> AccSplit =
      lam acc: AccSplit. lam tops: [CTop].
        foldl (lam acc: AccSplit. lam top: CTop.

          -- Split functions
          match top with CTFun _ then splitFunction acc top

          -- Remove all function declarations, this is handled separately in any
          -- case.
          else match top with CTDef { ty = CTyFun _ } then acc

          -- Leave everything else intact
          else {acc with tops = snoc acc.tops top}
        ) acc tops
    in

    -- Type for names with global scope
    type Globals = {
      globalAllocs: [(Name,Ctype)],
      globals: [(Name,CType)]
    } in

    let emptyGlobals = { globalAllocs = [], globals = [] } in

    -- Retrieve global allocations from a list of C statments
    let findGlobals: [Name] -> [CStmt] -> Globals =
      lam names: [Name].
      lam stmts: [CStmt].
        foldl (lam acc: Globals. lam stmt: CStmt.

          -- Global struct allocation
          match stmt with CSDef {
            ty = ty,
            id = Some name,
            init = Some (CIExpr { expr = CEAlloc {} })
          } then
            if any (nameEq name) names then
              { acc with globalAllocs = snoc acc.globalAllocs (name,ty) }
            else acc

          -- Global non-struct allocation
          else match stmt with CSDef { ty = ty, id = Some name } then
            if any (nameEq name) names then
              { acc with globals = snoc acc.globals (name,ty) }
            else acc

          else acc

        ) emptyGlobals stmts
    in

    -- Used for removing def statements, since allocation for the corresponding
    -- variables is handled elswhere.
    let stripDefs: Globals -> StackFrame -> CStmt -> [CStmt] =
      lam globals: Globals.
      lam sf: StackFrame.
      lam stmt: CStmt.

        recursive let rec = lam stmt: CStmt.

          -- Always remove cealloc
          match stmt with CSDef { init = Some (CIExpr { expr = CEAlloc {} }) }
          then []

          -- Replace def if local or global
          else match stmt with CSDef { id = Some id, init = init } then
            let g = any (lam g. nameEq id g.0) globals.globals in
            let l = any (lam l. nameEq id l.0) sf.locals in
            if or g l then
              match init with Some init then
                match init with CIExpr { expr = expr } then
                  [CSExpr { expr = CEBinOp {
                    op = COAssign {},
                    lhs = CEVar { id = id },
                    rhs = expr
                  }}]
                else match init with None () then
                  error "Non-CIExpr initializer in stripDefs"
                else never
              else []
            else [stmt]

          -- Recurse
          else [sreplace_CStmt_CStmt rec stmt]

        in rec stmt
    in

    let replaceVar: Globals -> StackFrame -> CExpr -> CExpr =
      lam globals: Globals.
      lam sf: StackFrame.
      lam expr: CExpr.
        let isPtr: Name -> Bool = lam id: Name.
          if any (lam g. nameEq id g.0) sf.params then true
          else if any (lam g. nameEq id g.0) sf.locals then true
          else false
        in
        recursive let rec = lam expr: CExpr.
          match expr with CEVar { id = id } then

            -- Stored directly in PSTATE
            -- TODO Guarantee unique names somehow, rhs of member is string.
            -- Perhaps just have an array in PSTATE containing all global data?
            if any (lam g. nameEq id g.0) globals.globals then
            CEMember { lhs = CEPState {}, id = nameGetStr id }

            -- Stored in PSTATE but must be accessed through pointer
            -- TODO Guarantee unique names somehow, rhs of member is string.
            else if any (lam g. nameEq id g.0) globals.globalAllocs then
            CEUnOp { op = COAddrOf {}
                   , arg = CEMember { lhs = CEPState {}, id = nameGetStr id } }

            else if isPtr id then derefExpr expr

            -- Leave other variables
            else expr

          else smap_CExpr_CExpr rec expr
        in rec expr
    in

    -----------
    -- START --
    -----------

    -- Run base C compiler
    match compile typeEnv prog with (env, types, tops, inits) then

    -- Compute stack frames for each function and the init code
    match getStackFrames tops with sfs then
    let retTy: CType = compileType env (ty prog) in
    let initSF: StackFrame = getInitStackFrame inits retTy in

    -- Helper function for getting the stack frame of a top.
    let topStackFrame: CTop -> StackFrame =
      lam top.
        let id = match top with CTFun { id = id } then id
                 else error "Invalid top in topStackFrame" in
        getStackFrame id sfs
    in

    -- Compute globally scoped variables
    let globals: Globals = findGlobals globalNames inits in
    let f = lam g:[(Name,CType)].
      filter (lam l. not (any (lam r. nameEq l.0 r.0) g)) in
    let initSF: StackFrame =
      {{ initSF with localAllocs = f globals.globalAllocs initSF.localAllocs }
                with locals = f globals.globals initSF.locals } in

    -- Replace defs
    let tops: [CTop] = map (lam top.
      let sf = topStackFrame top in
      sreplace_CTop_CStmt (stripDefs globals sf) top
    ) tops in
    let inits: [CStmt] = join (map (stripDefs globals initSF) inits) in

    -- Replace global variable uses
    let tops: [CTop] = map (lam top.
      let sf = topStackFrame top in
      smap_CTop_CExpr (replaceVar globals sf) top
    ) tops in
    let inits: [CStmt] =
      map (smap_CStmt_CExpr (replaceVar globals initSF)) inits in

    -- Split functions into BBLOCKs
    let accSplit = { emptyAccSplit with sfs = sfs } in
    match splitFunctions accSplit tops with { tops = tops } then
    let tops = concat tops (splitInit accSplit initSF inits) in

    let stackSize = 1000 in

    -- PSTATE.stackPtr = PSTATE.stack
    let initStackPtr = CSExpr { expr = CEBinOp {
      op = COAssign {},
      lhs = stackPtr,
      rhs = CEMember { lhs = CEPState {}, id = "stack" }
    }} in
    let initRet =
      match retTy with ! CTyVoid _ then
        Some (CEMember { lhs = CEPState {}, id = "ret" })
      else None () in
    let nameEnd = nameSym "end" in
    let initCall =
      constructCall initSF nameInit
        [] initRet (CEBBlockName { name = nameEnd }) in
    let startBBlock = CTBBlock {
      id = nameSym "start",
      body = join [
        [initStackPtr],
        initCall
      ]
    } in
    let endBBlock = CTBBlock {
      id = nameEnd,
      body = [setPCFromExpr (CEInt { i = negi 1 })]
    } in

    let progStateMem = join [
      [
        ( CTyArray { ty = CTyChar {}, size = Some (CEInt { i = stackSize })}
        , Some "stack" ),
        ( CTyPtr { ty = CTyChar {} }, Some "stackPtr" ),
        ( retTy, Some "ret" )
      ],
      map (lam t. (tyDeref t.1, Some (nameGetStr t.0))) globals.globalAllocs,
      map (lam t. (t.1, Some (nameGetStr t.0))) globals.globals
    ] in

    RPProg {
      includes = ["\"inference/smc/smc.cuh\""],
      pStateTy = CTyStruct { id = None (), mem = Some progStateMem },
      types = types,
      tops = join [[startBBlock, endBBlock], tops]
    }

    else never
    else never
    else never

-- Get the names of all globally accessible non-function data.
let findGlobalNames: Expr -> [Name] = use MExprPPLRootPPLCompile in
  lam expr: Expr.
    recursive let rec = lam acc: [Name]. lam expr: Expr.
      match expr with TmLet { ident = ident, body = ! TmLam _, inexpr = inexpr }
      then rec (cons ident acc) inexpr
      else match expr with
        TmLet { inexpr = inexpr }
        | TmRecLets { inexpr = inexpr }
        | TmType { inexpr = inexpr }
        | TmConDef { inexpr = inexpr }
      then rec acc inexpr
      else acc
    in rec [] expr

let rootPPLCompile: Expr -> RPProg = use MExprPPLRootPPLCompile in lam prog.

  -- print (expr2str prog); print "\n\n";

  -- Symbolize with empty environment
  let prog: Expr = symbolizeExpr symEnvEmpty prog in

  -- print "--- AFTER SYMBOLIZE ---\n";
  -- dprint prog; print "\n\n";

  -- Find global non-function definitions
  let globals: [Name] = findGlobalNames prog in

  -- print "--- AFTER FIND GLOBALS ---\n";
  -- dprint prog; print "\n\n";

  -- dprint globals; print "\n\n";

  -- Type annotate
  let prog: Expr = typeAnnot prog in

  -- print "--- AFTER TYPE ANNOTATE ---\n";
  -- dprint prog; print "\n\n";

  -- ANF transformation
  let prog: Expr = normalizeTerm prog in

  -- print "--- AFTER ANF ---\n";
  -- dprint prog; print "\n\n";

  -- Type lift
  match typeLift prog with (env, prog) then

    -- print (expr2str prog); print "\n\n";

    -- Run C compiler
    let rpprog: RPProg = rootPPLCompileH env globals prog in

    -- print (printCompiledRPProg rpprog);
    rpprog

  else never

mexpr
use MExprPPLRootPPLCompile in

-- rootPPLCompile crbd;

()
