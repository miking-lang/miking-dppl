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
  | CTyVar { id = tyName } & ty ->
    let allocName = nameSym "alloc" in
    [
      -- Placeholder definition of pointer to allocated struct
      { ty = ty
      , id = Some name
      , init = Some (CIExpr { expr = CEAlloc {} })
      }
    ]
  | _ -> error "Not a CTyVar in alloc"

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

    type StackFrame = {
      params: [(Name,CType)],
      locals: [(Name,CType)],
      ret: CType
    } in

    type AccSF = {
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

    let emptyAcc: AccSF =
      { params = [], defs = [], prevDefs = []
      , locals = [], hasSplit = false, sfs = [] }
    in

    recursive let addLocals: AccSF -> CExpr -> AccSF =
      lam acc: AccSF. lam expr: CExpr.
        let lookup = assocSeqLookup {eq = nameEq} in
        match expr with CEVar { id = id } then
          match lookup id acc.params with Some _ then acc
          else match lookup id acc.locals with Some _ then acc
          else match lookup id acc.prevDefs with Some ty then
            {acc with locals = cons (id,ty) acc.locals}
          else acc
        else sfold_CExpr_CExpr addLocals acc expr
    in

    let nextBBlock: AccSF -> AccSF = lam acc: AccSF.
      {{{ acc with defs = []}
              with prevDefs = concat acc.defs acc.prevDefs}
              with hasSplit = true}
    in

    recursive let getLocals: AccSF -> [CStmt] -> AccSF =
      lam acc: AccSF. lam stmts: [CStmt].
      let acc = {acc with hasSplit = false} in
      foldl (lam acc: AccSF. lam stmt: Stmt.
        match stmt with CSDef { ty = ty, id = Some name, init = init } then
          let acc = sfold_CStmt_CExpr addLocals acc stmt in
          let acc = { acc with defs = cons (name,ty) acc.defs } in
          match init with Some (CIExpr { expr = CEApp _ }) then
            nextBBlock acc
          else acc

        else match stmt with CSIf { cond = cond, thn = thn, els = els } then
          let acc = addLocals acc cond in
          let thnAcc = getLocals acc thn in
          let elsAcc = getLocals {acc with locals = thnAcc.locals} els in
          let acc = {acc with locals = elsAcc.locals} in
          if or (thnAcc.hasSplit) (elsAcc.hasSplit) then nextBBlock acc
          else acc

        else match stmt with CSExpr { expr = expr } then
          let acc = sfold_CStmt_CExpr addLocals acc stmt in
          match expr with CEApp _ | CEResample _ then nextBBlock acc
          else acc

        else match stmt with CSRet _ then sfold_CStmt_CExpr addLocals acc stmt
        else match stmt with CSNop _ then acc
        else error "Not supported in getLocals"

      ) acc stmts
    in

    let getStackFrames: AccSF -> [CTop] -> AccSF =
      lam tops: [CTop].
        foldl (lam acc: AccSF. lam top: CTop.
          match top
          with CTFun { ret = ret, id = id, params = params, body = body } then

            -- Initialize accumulator for new function
            let acc = {{{{{ acc with params = map (lam t. (t.1,t.0)) params }
                                with defs = [] }
                                with prevDefs = [] }
                                with locals = [] }
                                with hasSplit = false } in

            -- Split function into BBLOCKs
            let acc = getLocals acc body in

            -- Record stack frame
            {acc with sfs =
               snoc acc.sfs
                 (id, {params = acc.params, locals = acc.locals, ret = ret})}

          else acc) emptyAcc tops
    in

    type Next in
    -- Collapse stack at endpoint
    con Collapse: () -> Next in
    -- Jump to block at endpoint (block name can be generated a priori)
    con Block: Option Name -> Next in

    type AccBB = {
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

    let emptyAccBB: [(Name,StackFrame)] -> AccBB = lam sfs.
      { next = Collapse (), hasSplit = false, block = [], sfs = sfs, tops = [] }
    in

    let createBlock: Name -> AccBB -> AccBB =
      lam name: Name. lam acc: AccBB.
        let bb = CTBBlock { id = name, body = acc.block } in
        {acc with tops = cons bb acc.tops}
    in

    let setPC: Name -> CExpr = lam name.
      (CSExpr { expr = (CEBinOp { op = COAssign {}, lhs = CEPC {}
                                , rhs = CEBBlockName { name = name }})})
    in

    recursive let splitFunBody: AccBB -> [CStmt] -> AccBB =
      lam acc: AccBB. lam stmts: [CStmt].
        match stmts with [stmt] ++ stmts then
          -- Function application or resample
          match stmt with CSDef { init = Some (CIExpr { expr = CEApp _ }) }
                        | CSExpr { expr = CEApp _ }
                        | CSExpr { expr = CEResample _ } then
            -- print "Split case\n\n";
            let block = snoc acc.block stmt in
            let acc = {acc with hasSplit = true} in
            match stmts with [] then
              -- CASE: Last statment _and_ end of block
              -- If next = Collapse () -> Tail call
              --   1. Build new stack frame with return address from current stack frame
              --   2. Also collapse previous stack frame in some way (tail-call optimization)
              -- If next = Block _ -> set return address for call = name
              match acc.next with Collapse _ then
                {acc with block = snoc block (setPC (nameSym "rafromcollapsed"))}
              else match acc.next with Block (Some name) then
                {acc with block = snoc block (setPC name)}
              else match acc.next with Block (None ()) then
                let name = nameSym "bblocklazy" in
                {{acc with block = snoc block (setPC name)}
                      with next = Block (Some name)}
              else never
            else
              -- CASE: Not last statement, but end of block
              -- 1. Generate name for and build next block
              -- 2. Set return address for call = name
              let accNext = {acc with block = []} in
              let accNext = splitFunBody accNext stmts in
              let name = nameSym "bblock" in
              let accNext = createBlock name accNext in
              -------- TODO Temporary, return addr should be pushed on stack instead
              let block = snoc block (setPC name) in
              --------
              {accNext with block = block}

          -- Not a function application or resample, just continue recursion
          else match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then
            -- print "Easy case\n\n";
            splitFunBody {acc with block = snoc acc.block stmt} stmts

          -- If statements
          else match stmt with CSIf { cond = cond, thn = thn, els = els } then
            -- print "If case\n\n";
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
              -- print "There is at least one split";
              let branchFin =
                match accEls.next with Collapse _ then
                  setPC (nameSym "rafromcollapse")
                else match accEls.next with Block (Some name) then
                  setPC name
                else error "Impossible Error in splitFunBody"
              in
              let accThn =
                if accThn.hasSplit then accThn
                else {accThn with block = snoc accThn.block branchFin}
              in
              let accEls =
                if accEls.hasSplit then accEls
                else {accEls with block = snoc accEls.block branchFin}
              in
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
                let name =
                  match accEls.next with Block (Some name) then name
                  else error "Impossible Error in splitFunBody" in
                let accStmts = createBlock name accStmts in
                {accStmts with block = block}

          else error "Not supported in splitFunBody"

        else match stmts with [] then
          -- print "End case\n\n";
          if acc.hasSplit then
            match acc.next with Collapse _ then
              -- Collapse stack
              {acc with block = snoc acc.block (setPC (nameSym "rafromcollapsed"))}
            else match acc.next with Block (Some name) then
              -- Call next block here instead (we do not want to resample)
              {acc with block = snoc acc.block (setPC name)}
            else match acc.next with Block (None ()) then
              let name = nameSym "bblocklazy" in
              {{acc with block = snoc acc.block (setPC name)}
                    with next = Block (Some name)}
            else never
          else acc

        else never
    in

    let transformTops: AccBB -> [CTop] -> AccBB =
      lam acc: AccBB. lam tops: [CTop].
      foldl (lam acc: AccBB. lam top: CTop.
        match top with CTFun { id = id, body = body } then
          let acc = {{acc with next = Collapse ()} with block = []} in
          let acc = splitFunBody acc body in
          let acc =
            if acc.hasSplit then acc else
              {acc with block = snoc acc.block (setPC (nameSym "rafromcollapsed"))}
          in
          createBlock id acc
        else {acc with tops = snoc acc.tops top} -- TODO
      ) acc tops
    in

    match compile typeEnv prog with (types, tops, inits) then
    match getStackFrames tops with { sfs = sfs } then
    match transformTops (emptyAccBB sfs) tops with { tops = tops } then

      -- dprint sfs; print "\n";
      -- TODO Run getLocals on inits as well

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

  -- print (expr2str prog); print "\n\n";

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

    print (printCompiledRPProg rpprog);
    rpprog

  else never

in

compile crbd;

()
