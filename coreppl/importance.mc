

-- include "ocaml/compile.mc"
include "compile-mcore.mc"
include "seq.mc"
include "coreppl.mc"

lang MExprPPLImportance = MExprPPL

  sem transformImpSeq =
  | TmAssume r ->
      (app_ (app_ (var_ "betaSample") (float_ 2.)) (float_ 2.))

--app_TmConst {val=CInt {val=7} , ty=r.ty, info=r.info}
  | TmObserve r -> TmConst {val=CInt {val=8} , ty=r.ty, info=r.info}
  | TmDist r -> distReplace r.ty r.info r.dist
  | expr -> smap_Expr_Expr transformImpSeq expr

  sem distReplace (ty:Ty) (info:Info) =
  | DUniform r -> TmConst {val=CInt {val=7} , ty=ty, info=info}
  | expr -> TmConst {val=CInt {val=100} , ty=ty, info=info}

end



let importanceSamplingInference = lam options. lam ast.
  use MExprPPLImportance in
  let ast = transformImpSeq ast in
  -- Print (optional) the transformed MCore program
  (if options.printMCore then
    printLn (expr2str ast)
  else ());
  -- Compile and run the code
  compileRunMCore ast
