-- PPLCore AST

include "mexpr/ast.mc"

lang PPLCore = MExprAst

  syn Expr =
  | TmWeight { arg: Expr }
  | TmSampleExp { a: Expr }
  | TmSampleBern { p: Expr }
  | TmResample {}

end

