-- CorePPL compiler, targeting the RootPPL framework
-- TODO(dlunde,2021-05-04): Out of date

include "../coreppl/coreppl.mc"

include "../models/crbd.mc"

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
  | CEBBlockName { name: Name } -- Block names

  sem printCExpr (env: PprintEnv) =
  | CEAlloc {} -> (env, "<<<CEAlloc>>>")
  | CEResample {} -> (env, "<<<CEResample>>>")
  | CEBBlockName { name = name } ->
    match pprintEnvGetStr env name with (env,name) then
      (env, join ["###", name, "###"])
    else never

  sem sfold_CExpr_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CEAlloc {} -> acc
  | CEResample {} -> acc
  | CEBBlockName _ -> acc

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
  | DBern { p = p } -> CDBern { p = compileExpr p }
  | DBeta { a = a, b = b } -> CDBeta { a = compileExpr a, b = compileExpr b }
  | DCategorical { p = p } -> CDCategorical { p = compileExpr p }
  | DMultinomial { n = n, p = p } ->
    CDMultinomial { n = compileExpr n, p = compileExpr p }
  | DDirichlet { a = a } -> CDDirichlet { a = compileExpr a }
  | DExp { rate = rate } -> CDExp { rate = compileExpr rate }
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
let rootPPLCompile: [(Name,Type)] -> Expr -> RPProg =
  use MExprPPLRootPPLCompile in
  lam typeEnv: [(Name,Type)].
  lam prog: Expr.

    -- Stackframe type, containing relevant names and types
    type StackFrame = {
      params: [(Name,CType)],
      locals: [(Name,CType)],
      ret: CType
    } in

    -- Accumualator used when determining stack frames
    type AccStackFrames = {
      -- Parameters for current function
      params: [(Name,CType)],
      -- Names defined in current block
      defs: [(Name,CType)],
      -- Names defined in previous blocks for a function
      prevDefs: [(Name,CType)],
      -- Accumulated locals that traverse block boundaries
      locals: [(Name,CType)],
      -- Return value indicating whether or not a list of CStmt contains a
      -- BBLOCK split
      hasSplit: Bool,
      -- Accumulated bindings from names to stack framse
      sfs: [(Name,StackFrame)]
    } in
    let emptyAcc: AccStackFrames =
      { params = [], defs = [], prevDefs = []
      , locals = [], hasSplit = false, sfs = [] }
    in

    -- Add all local variables used in an expression that traverse BBLOCK
    -- boundaries to the accumulator
    recursive let addLocals: AccStackFrames -> CExpr -> AccStackFrames =
      lam acc: AccStackFrames. lam expr: CExpr.
        let lookup = assocSeqLookup {eq = nameEq} in
        match expr with CEVar { id = id } then
          match lookup id acc.params with Some _ then acc
          else match lookup id acc.locals with Some _ then acc
          else match lookup id acc.prevDefs with Some ty then
            {acc with locals = cons (id,ty) acc.locals}
          else acc
        else sfold_CExpr_CExpr addLocals acc expr
    in

    -- Mark the end of a BBLOCK in the accumulator
    let nextBBlock: AccStackFrames -> AccStackFrames = lam acc: AccStackFrames.
      {{{ acc with defs = []}
              with prevDefs = concat acc.defs acc.prevDefs}
              with hasSplit = true}
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

          -- Add locals
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

          else match stmt with CSIf { thn = thn, els = els } then
            let accThn = getLocals acc thn in
            let accEls = getLocals {acc with locals = accThn.locals} els in
            let acc = {acc with locals = accEls.locals} in
            if or (accThn.hasSplit) (accEls.hasSplit) then nextBBlock acc
            else acc

          else match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then acc

          else error "Unsupported term in getLocals"

        ) acc stmts
    in

    -- Recurse over all top-level definitions and determine stack frames for
    -- functions
    let getStackFrames: AccStackFrames -> [CTop] -> AccStackFrames =
      lam tops: [CTop].
        foldl (lam acc: AccStackFrames. lam top: CTop.
          match top
          with CTFun { ret = ret, id = id, params = params, body = body } then

            -- Initialize accumulator for new function
            let acc = {{{{{ acc with params = map (lam t. (t.1,t.0)) params }
                                with defs = [] }
                                with prevDefs = [] }
                                with locals = [] }
                                with hasSplit = false } in

            -- Get locals that traverse BBLOCK boundaries in function
            let acc = getLocals acc body in

            -- Record stack frame
            {acc with sfs =
               snoc acc.sfs
                 (id, {params = acc.params, locals = acc.locals, ret = ret})}

          else acc) emptyAcc tops
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
      -- Bindings from names to stack frames
      sfs: [(Name,StackFrame)],
      -- Accumulated set of top-level definitions
      tops: [CTop]
    } in
    let emptyAccSplit: [(Name,StackFrame)] -> AccSplit = lam sfs.
      { next = Collapse (), hasSplit = false, block = [], sfs = sfs, tops = [] }
    in

    -- Construct a BBLOCK from the currently accumulated statements
    let createBlock: Name -> AccSplit -> AccSplit =
      lam name: Name. lam acc: AccSplit.
        let bb = CTBBlock { id = name, body = acc.block } in
        {acc with tops = snoc acc.tops bb}
    in

    -- C statement for setting the PC to a name
    let setPC: Name -> CExpr = lam name.
      (CSExpr { expr = (CEBinOp { op = COAssign {}, lhs = CEPC {}
                                , rhs = CEBBlockName { name = name }})})
    in

    let initNextName: AccSplit -> AccSplit = lam acc: AccSplit.
      match acc.next with Block (Some name) then acc
      else match acc.next with Block (None ()) then
        {acc with next = Block (Some (nameSym "lazy"))}
      else error "Error in initNextName"
    in

    let getNextName: AccSplit -> Name = lam acc: AccSplit.
      match acc.next with Block (Some name) then name
      else error "Error in getNextName"
    in

    -- Split a function (= list of statements) into BBLOCKs
    recursive let splitFunBody: AccSplit -> [CStmt] -> AccSplit =
      lam acc: AccSplit. lam stmts: [CStmt].

        match stmts with [stmt] ++ stmts then

          -- Function application
          match stmt with CSDef { init = Some (CIExpr { expr = CEApp _ }) }
                        | CSExpr { expr = CEApp _ } then
            --------- TODO TEMPORARY -------
            let block = snoc acc.block stmt in
            --------------------------------
            let acc = {acc with hasSplit = true} in
            match stmts with [] then
              -- CASE: Function application as last statment _and_ end of block
              match acc.next with Collapse _ then
                -- Tail call, build new and collapse current stack frame
                -- (tail-call optimization)
                -- TODO Collapse + build stack frame
                {acc with block = snoc block (setPC (nameSym "tailcall"))}
              else match acc.next with Block _ then
                -- Regular call, build new stack frame with ra given by acc.next
                let acc = initNextName acc in
                let name = getNextName acc in
                -- TODO Build stack frame
                {acc with block = snoc block (setPC name)}
                -----------
              else never
            else
              -- CASE: Not last statement, but end of block
              let accNext = {acc with block = []} in
              let accNext = splitFunBody accNext stmts in
              let name = nameSym "bblock" in
              let accNext = createBlock name accNext in
              -------- TODO Build new stack frame
              let block = snoc block (setPC name) in
              --------
              {accNext with block = block}

          -- Resample
          else match stmt with CSExpr { expr = CEResample _ } then
            let acc = {acc with hasSplit = true} in
            match stmts with [] then
              -- CASE: Resample as last statment _and_ end of block
              match acc.next with Collapse _ then
                -- TODO Collapse current stack frame and set PC to RA
                {acc with block = snoc acc.block (setPC (nameSym "collapse"))}
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
              let accNext = createBlock name accNext in
              -- Simply set PC to next block
              {accNext with block = snoc acc.block (setPC name)}

          -- Not a function application or resample, just accumulate and
          -- continue
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
              let branchFin =
                match accEls.next with Collapse _ then
                  -- TODO Collapse stack frame
                  setPC (nameSym "collapse")
                else match accEls.next with Block (Some name) then
                  -- Set PC to correct BBLOCK
                  setPC name
                else error "Impossible Error in splitFunBody"
              in
              let update = lam acc: AccSplit.
                if acc.hasSplit then acc
                else {acc with block = snoc acc.block branchFin}
              in
              let accThn = update accThn in
              let accEls = update accEls in
              let stmt =
                CSIf { cond = cond, thn = accThn.block, els = accEls.block } in
              let block = snoc acc.block stmt in

              -- Create new block for stmts if needed
              let accStmts = {{accEls with hasSplit = true}
                                      with next = acc.next} in
              match stmts with [] then {accStmts with block = block}
              else
                let accStmts = {accStmts with block = []} in
                let accStmts = splitFunBody accStmts stmts in
                let name = getNextName accEls in
                let accStmts = createBlock name accStmts in
                {accStmts with block = block}

          else error "Not supported in splitFunBody"

        -- End of block without split
        else match stmts with [] then
          if acc.hasSplit then
            match acc.next with Collapse _ then
              -- TODO Collapse stack
              {acc with block = snoc acc.block (setPC (nameSym "collapse"))}
            else match acc.next with Block _ then
              let acc = initNextName acc in
              let name = getNextName acc in
              -- Call next block here instead (we do not want to resample)
              {acc with block = snoc acc.block (setPC name)}
              ------------
            else never
          else acc

        else never
    in

    -- Split a function
    let splitFunction: AccSplit -> CTop -> AccSplit =
      lam acc: AccSplit. lam top: CTop.
        match top with CTFun { id = id, body = body } then
          let acc = {{acc with next = Collapse ()} with block = []} in
          let acc = splitFunBody acc body in
          createBlock id acc
        else error "Non-CTFun in splitFunction"
    in

    -- Iterate over top-level definitions and split functions into BBLOCKs
    -- NOTE(dlunde,2021-05-21): Currently, _all_ functions are split. Further
    -- on, we only want to split where necessary.
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

    match compile typeEnv prog with (types, tops, inits) then
    match getStackFrames tops with { sfs = sfs } then
    match splitFunctions (emptyAccSplit sfs) tops with { tops = tops } then

      -- TODO Do everthing for inits as well

      let initbb = CTBBlock { id = nameSym "init", body = inits } in
      RPProg {
        includes = _includes,
        pStateTy = CTyVoid {},
        tops = join [types, tops, [initbb]]
      }

    else never
    else never
    else never

mexpr
use MExprPPLRootPPLCompile in

let compile: Expr -> RPProg = lam prog.

  print (expr2str prog); print "\n\n";

  -- Symbolize with empty environment
  let prog = symbolizeExpr symEnvEmpty prog in

  -- Type annotate
  let prog = typeAnnot prog in

  -- ANF transformation
  let prog = normalizeTerm prog in

  -- Type lift
  match typeLift prog with (env, prog) then

    -- print (expr2str prog); print "\n\n";

    -- Run C compiler
    let rpprog = rootPPLCompile env prog in

    -- print (printCompiledRPProg rpprog);
    rpprog

  else never

in

compile crbd;

()
