-- Sketch of CUDA language fragment

include "option.mc"

-- TODO Some things can probably be reused from mexpr/ast.mc here
-- TODO The fragment should probably not be named CUDA

lang CUDA

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
    -- TODO Add type for C structs?

  syn Stmt =
    | SIf      { cond: Val, thn: Stmt, els: Stmt }
    | SWhile   { cond: Val, body: Stmt }
    | SRet     { val: Option }
    | SAssg    { var: String, val: Val }
    | SAppAssg { var: String, fun: String, args: [Val] }
    | SWeight  { arg: Val }
    | SPush    { arg: Val }
    | SPop     { arg: Val }

  syn Val =
    | VVar   { var: String }
    | VInt   { val: Int }
    | VFloat { val: Float }
    | VPtr   { adr: Int }

  sem outputProg =
    | PProg tops -> "Hello, World!"

end

mexpr
use CUDA in

let prog = PProg{ tops = [] } in

printLn (outputProg prog)
