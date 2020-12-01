-- CorePPL AST

include "mexpr/ast.mc"

lang CorePPL = MExprAst

  syn Expr =
  | TmWeight { arg: Expr }
  | TmSampleExp { a: Expr }
  | TmSampleBern { p: Expr }
  | TmResample {}

end

