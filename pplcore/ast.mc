-- PPLCore AST

include "mexpr/ast.mc"

lang PPLCoreAst = MExprAst

  syn Expr =
  | TmWeight { arg: Expr }
  | TmSampleExp { a: Expr }
  | TmSampleBern { p: Expr }
  | TmResample {}

end

