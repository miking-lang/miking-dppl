-- Sketch of CUDA language fragment

include "option.mc"

-- TODO Reuse things from MExpr
-- TODO Factor out smaller fragments and add them directly to the Miking
-- framework

lang SMCCUDAAst

  syn Prog =
    | PProg { tops: [Top] }

  syn Top =

    -- Global data declarations
    | TBBlockData { ty     : Type,
                    var    : String }

    -- Basic block helpers (regular functions?)
    | TBBlockHelper { ty     : Type,
                      fun    : String,
                      params : [{ ty: Type, var: String }],
                      body   : [Stmt] }

    -- Basic blocks (no params allowed)
    | TBBlock { ty     : Type, -- TODO Type needed? Can we return things?
                fun    : String,
             -- params : [{ ty: Type, var: String }],
                body   : [Stmt] }

  syn Type =
    | TyInt   {}
    | TyFloat {}
    | TyVoid  {}
    | TyPtr   { ty: Type }
    -- TODO Structs? Arrays? Enums? Unions?

  syn Stmt =
    | SIf      { cond: Expr, thn: Stmt, els: Stmt }
    | SWhile   { cond: Expr, body: Stmt }
    | SRet     { val: Option }
    | SAssg    { var: String, val: Expr }
    | SAppAssg { var: String, fun: String, args: [Expr] }
    | SWeight  { arg: Expr }
    | SPush    { arg: Expr }
    | SPop     { arg: Expr }

  syn Expr =
    | VVar   { var: String }
    | VInt   { val: Int }
    | VFloat { val: Float }
    | VPtr   { adr: Int }

  sem outputProg =
    | PProg tops -> "Hello, World!"

end

mexpr
use SMCCUDAAst in

let prog = PProg{ tops = [] } in

printLn (outputProg prog)
