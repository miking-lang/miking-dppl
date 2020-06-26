-- Sketch of CUDA language fragment

include "option.mc"
include "c/ast.mc"

lang SMCCUDAAst = CStmtAst

  syn Prog =
    | Prog { tops: [Top] }

  syn Top =

    -- Global data declarations
    | TBBlockData { ty     : Type,
                    var    : String }

    -- Basic block helpers (regular functions?)
    | TBBlockHelper { ty     : Type,
                      id     : String,
                      params : [{ ty: Type, var: String }],
                      body   : Stmt }

    -- Basic blocks (no params allowed)
    | TBBlock { id   : String,
                body : Stmt }

  syn Expr =
    | TmWeight  { arg: Expr }
    | TmPush    { arg: Expr }
    | TmPop     { arg: Expr }
    | TmSetPC   { arg: Expr }
    | TmPCCall  { arg: Expr }

  sem codeGen =
    | Prog tops -> "Hello, World!"

end

mexpr
use SMCCUDAAst in

let prog = PProg{ tops = [] } in

printLn (codeGen prog)
