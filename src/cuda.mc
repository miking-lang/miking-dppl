-- Sketch of CUDA language fragment

-- Standard lib includes
include "option.mc"

-- Local includes
include "pplcore.mc"

-- TODO Some things can probably be reused from mexpr/ast.mc here

-- TODO The fragment should probably not be named CUDA
lang CUDA

  syn Prog =
    | PProg { tops: [Top] }

  syn Top =
    | TVarDecl { ty     : Type,
                 var    : String }
    | TFunDecl { ty     : Type,
                 fun    : String,
                 params : [{ ty: Type, var: String }] }
    | TFun     { ty     : Type,
                 fun    : String,
                 params : [{ ty: Type, var: String }],
                 body   : [Stmt] }

    -- TODO Should probably replace function definitions altogether with
    -- TBBlock and TBBlockHelper?
    | TBBlock {}

  syn Type =
    | TyInt {}
    | TyPtr { ty: Type }

  syn Stmt =
    | SIf    { cond: Expr, thn: Stmt, els: Stmt }
    | SWhile { cond: Expr, body: Stmt }
    | SRet   { expr: Option }
    | SAssg  { var: String, expr: Expr }
--  | SApp   { fun: String, args: [Expr] }
    | SBlock { stmts: [Stmt] }

  syn Expr =
    | EApp { fun: String, args: [Expr] }
    | EVar { var: String }
    | EInt { val: Int }

    -- TODO Fill in needed stuff from framework, e.g. stack handling
    -- TODO We might want to move EWeight to Stmt?
    | EWeight { arg: Expr }

  sem outputProg =
    | PProg tops -> "Hello, World!"

end

mexpr
use CUDA in

let prog = PProg{ tops = [] } in

printLn (outputProg prog)
