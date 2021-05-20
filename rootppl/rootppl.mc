-- Sketch of CUDA language fragment

include "option.mc"
include "c/ast.mc"

lang RootPPL = CTopAst + CStmtAst

  syn CTop =

    -- Global data declarations
    | TBBlockData { ty: CType,
                    id: Name }

    -- Basic block helpers (essentially regular functions with special syntax)
    | TBBlockHelper { ty     : CType,
                      id     : Name,
                      params : [(CType,Name)],
                      body   : [Stmt] }

    -- Basic blocks
    | TBBlock { id   : Name,
                body : [Stmt] }

  -- TODO(dlunde,2021-05-04)
  syn CExpr =
    | TmWeight  { arg: Expr }
    | TmPush    { arg: Expr }
    | TmPop     { arg: Expr }
    | TmSetPC   { arg: Expr }
    | TmPCCall  { arg: Expr }

end

mexpr
use RootPPL in

()
