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

include "c/ast.mc"
include "c/pprint.mc"
include "c/compile.mc"

lang Compiler = MExprPPL + RootPPL + MExprCCompile

  ------------------
  -- ANF OVERRIDE --
  ------------------
  -- Ensures argument to assume is not lifted by ANF (first-class distributions
  -- not supported in RootPPL)

  sem normalize (k : Expr -> Expr) =
  | TmAssume ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmAssume { t with dist = TmDist { td with dist = dist } }))
      dist


  -----------------------------------------
  -- ADDITIONS TO EXPRESSION COMPILATION --
  -----------------------------------------

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

  sem compileExpr =
  | TmAssume _ -> error "Assume without direct distribution"
  | TmAssume { dist = TmDist { dist = dist }} ->
    CESample { dist = compileDist dist }
  | TmWeight { weight = weight } -> CEWeight { weight = compileExpr weight }


  ---------------
  -- C COMPILE --
  ---------------
  -- Because of the rather involved method of allocating stuff, we need to defer allocations performed by alloc to a later stage

  -- Internal compiler extension to C initializers
  syn CExpr =
  | CEAlloc {}
  sem printCExpr (env: PprintEnv) =
  | CEAlloc {} -> (env, "CEAlloc")

  -- Name -> CType -> [{ ty: CType, id: Option Name, init: Option CInit }]
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

  sem printCompiledRPProg =
  | rpprog -> printRPProg cCompilerNames rpprog

  sem rootPPLCompile (typeEnv: [(Name,Type)]) =
  | prog ->
    match compile typeEnv prog with (types, tops, inits) then
      -- Temporary
      let initbb = CTBBlock { id = nameSym "init", body = inits } in
      RPProg { includes = _includes, pStateTy = CTyVoid {}, tops = join [types, tops, [initbb]] }
    else never


end

mexpr
use Compiler in

let compile: Expr -> RPProg = lam prog.

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
