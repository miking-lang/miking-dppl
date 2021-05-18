-- CorePPL compiler, targeting the RootPPL framework
-- TODO(dlunde,2021-05-04): Out of date

-- NOTE
--
-- #include <stdio.h>

-- struct Ty {
--   int a;
--   char b;
-- };

-- int main() {
--   char stack[1000];

--   int test = 1;
--   printf("Size: %zu\n", sizeof(struct Ty));

--   struct Ty *ptr = (struct Ty *) stack;
--   ptr->a = 32;
--   ptr->b = 'a';

--   struct Ty *ptr2 = (struct Ty *) (stack + sizeof(struct Ty));
--   ptr2->a = 31;
--   ptr2->b = 'b';

--   printf("ptr->a = %d, ptr->b = %c\n", ptr->a, ptr->b);
--   printf("ptr2->a = %d, ptr2->b = %c\n", ptr2->a, ptr2->b);

-- }

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
  | CEAlloc {}
  | CEResample {}
  | CEBBlockName { name: Name }

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

  -- Extension
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

  -- Extension
  sem compileExpr =
  | TmAssume _ -> error "Assume without direct distribution"
  | TmAssume { dist = TmDist { dist = dist }} ->
    CESample { dist = compileDist dist }
  | TmWeight { weight = weight } -> CEWeight { weight = compileExpr weight }
  | TmResample _ -> CEResample {}


  -- Allocation
  -- Type is
  -- `Name -> CType -> [{ ty: CType, id: Option Name, init: Option CInit }]`
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
    printRPProg cCompilerNames rpprog

-- Compiler entry point.
let rootPPLCompile: [(Name,Type)] -> Expr -> RPProg =
  use MExprPPLRootPPLCompile in
  lam typeEnv: [(Name,Type)].
  lam prog: Expr.

    type StackFrame = {
      params: [(Name,CType)],
      locals: [(Name,CType)]
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
      lam acc: AccSF. lam tops: [CTop].
        foldl (lam acc: AccSF. lam top: CTop.
          match top
          with CTFun { id = id, params = params, body = body } then

            -- Initialize accumulator for function
            let acc = {{{{{ acc with params = map (lam t. (t.1,t.0)) params }
                                with defs = [] }
                                with prevDefs = [] }
                                with locals = [] }
                                with hasSplit = false } in

            -- Split function into BBLOCKs
            let acc = getLocals acc body in

            -- Record stack frame
            {acc with sfs =
               snoc acc.sfs (id, {params = acc.params, locals = acc.locals})}

          else acc) emptyAcc tops
    in

    type Next
    con Collapse: () -> Next
    con Block: Option Name -> Next

    type AccBB = {
      -- What should happen when we reach an endpoint
      next: Next,
      -- Accumulated block
      block: [CStmt],
      -- Bindings from names to stack frames
      sfs: [(Name,StackFrame)],
      -- Accumulated set of top-level definitions
      tops: [CTop]
    } in

    let emptyAccBB: [(Name,StackFrame)] -> AccBB = lam sfs.
      { next = Collapse (), name = None ()
      , nextBlockIndex = 0, block = [], sfs = sfs, tops = [] }
    in

    let createBlock Name -> AccBB -> AccBB =
      lam name: Name. lam acc: AccBB.
        let bb = CTBBlock { id = name, body = acc.block } in
        {acc with tops = cons bb acc.tops}
    in

    recursive let splitFunBody: AccBB -> [CStmt] -> AccBB =
      lam acc: AccBB. lam stmts: [CStmt].
        match stmts with [stmt] ++ stmts then

          match stmt with CSDef { init = init } then
            match init with Some (CIExpr { expr = CEApp _ }) then
              let blocks = snoc acc.block stmt in
              match stmts with [] then
                -- TODO Handle `Next` somehow here
                {acc with blocks = blocks}
              else
                let acc = {acc with block = []} in
                let acc = splitFunBody acc stmts in
                let name = nameSym "bblock" in
                -------- TODO Temporary, should be pushed on stack
                let blocks = snoc acc.block
                  (CEBinOp { op = COAssign {}, lhs = CEPC {}
                           , rhs = CEBBlockName { name = name }})
                in
                --------
                let acc = createBlock name acc in
                {acc with blocks = blocks}
            else

          else match stmt with CSIf { cond = cond, thn = thn, els = els } then
            -- We need to check for splits within branches before going down branches
            -- sfold_CStmt_CStmt should be useful here

        else match stmts with [] then
          -- TODO Look at `Next`, take action depending?
        else never
    in

    let transformTops: AccBB -> [CTop] -> AccBB =
      lam acc: AccBB. lam tops: [CTop].
      foldl (lam acc: AccBB. lam top: CTop.
        match top with CTFun { id = id, body = body } then
          let acc = {{acc with next = Collapse ()} with block = []} in
          let acc = splitFunBody acc body in
          createBlock id acc
        else acc -- TODO
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

    print (expr2str prog); print "\n\n";

    -- Run C compiler
    let rpprog = rootPPLCompile env prog in

    print (printCompiledRPProg rpprog);
    rpprog

  else never

in

compile crbd;

()
