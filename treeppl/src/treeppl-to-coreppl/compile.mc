/-
TreePPL Compiler and Unit Tests
===============================

Viktor Senderov, Daniel Lundén, and Viktor Palmkvist (2022)

Unit tests available via:
  
  mi compile models/treeppl-to-coreppl/compile.mc --test

-/

include "treeppl-ast.mc"
include "mexpr/ast.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"
include "mexpr/eq.mc" -- for eqExpr
include "sys.mc"

include "../../../coreppl/src/coreppl-to-mexpr/compile.mc"
include "../../../coreppl/src/coreppl-to-rootppl/compile.mc"
include "../../../coreppl/src/coreppl.mc"
include "../../../coreppl/src/parser.mc"

lang TreePPLCompile = TreePPLAst + MExprPPL + RecLetsAst 
  sem compile: Expr -> FileTppl -> Expr
  sem compile (input: Expr) =

  | FileTppl x -> 
      let invocation = match findMap mInvocation x.decl with Some x
        then x
        else printLn "You need a model function"; never
      in 
      TmRecLets {
        bindings = map compileTpplDecl x.decl,
        inexpr = bind_ input invocation,
        ty = tyunknown_, 
        info = x.info
      }

  sem mInvocation: DeclTppl -> Option Expr
  sem mInvocation = 

  | _ -> None ()

  | FunDeclTppl (x & {model = Some _}) ->
    let invar = TmVar { 
        ident = x.name.v,
        info = x.name.i,
        ty = tyunknown_,
        frozen = false
      } in

    let f = lam f. lam arg.
      TmApp {
        lhs = f,
        rhs = parseArgument arg,
        ty = tyunknown_,
        info = x.info  
      }
    in
    Some (foldl f invar x.args)
  
  sem parseArgument: {name:{v:Name, i:Info}, ty:TypeTppl} -> Expr
  sem parseArgument =  

  | x -> TmVar {
      ident = x.name.v,
      ty = tyunknown_,
      info = x.name.i,
      frozen = false 
    } 

  sem compileTpplDecl: DeclTppl -> RecLetBinding
  sem compileTpplDecl = 

  | FunDeclTppl f -> {
        ident = f.name.v,
        tyBody = tyunknown_,
        body = 
          foldr (lam f. lam e. f e) unit_ (concat (map compileFunArg f.args) (map compileStmtTppl f.body)),          
        info = f.info
      }   

  sem compileFunArg: {name:{v:Name, i:Info}, ty:TypeTppl} -> (Expr -> Expr)
  
  sem compileFunArg =
  | arg ->
    lam cont. 
    TmLam {
      ident = arg.name.v,
      tyIdent = compileTypeTppl arg.ty,
      body = cont,
      ty = tyunknown_,
      info = arg.name.i
    }

  sem compileTypeTppl: TypeTppl -> Type

  sem compileTypeTppl =
  | TypeTppl x -> TyCon {
    ident = x.name.v,
    info = x.name.i
  }

  | AtomicRealTypeTppl x -> TyFloat {
    info = x.info
  }

  | AtomicBoolTypeTppl x -> TyBool {
    info = x.info
  }

  sem compileStmtTppl: StmtTppl -> (Expr -> Expr)

  sem compileStmtTppl = 

  | AssumeStmtTppl a -> 
  lam cont. TmLet {
    ident = a.randomVar.v, 
    tyBody = tyunknown_, 
    body = TmAssume {
      dist = compileExprTppl a.dist,
      ty = tyunknown_,
      info = a.info
    },
    inexpr = cont,
    ty = tyunknown_,
    info = a.info
  }
  
  | AssignStmtTppl a ->
  lam cont. TmLet {
    ident = a.var.v, 
    tyBody = tyunknown_, 
    body =  compileExprTppl a.val,
    inexpr = cont,
    ty = tyunknown_,
    info = a.info
  }

  | WeightStmtTppl a ->
  lam cont. TmLet {
    ident = nameNoSym "foo", 
    tyBody = tyunknown_, 
    body =  TmWeight {
      weight = compileExprTppl a.value,
      ty = tyunknown_,
      info = a.info
    },
    inexpr = cont,
    ty = tyunknown_,
    info = a.info
  }

  /--
  To avoid code duplication.
  Intuition: compiling with continuations
  Instead of 
    a; cont 
  we do
    let c = lam x:(). cont in
    a; c()
  
  Here c corresponds to contF.
  --/
  -- TODO for Daniel: have C compiler handle f()
  | IfStmtTppl a ->
  lam cont.
  let contName = nameSym "if-cont" in
  let contF = lam_ "" tyint_ cont in -- continuation function
  let cont: Expr = withInfo a.info (app_ (nvar_ contName) (int_ 0)) in
    TmLet {
      ident = contName,
      body = contF,
      tyBody = tyunknown_,
      ty = tyunknown_,
      info = a.info,
      inexpr = TmMatch {
        target = compileExprTppl a.condition,
        pat    = ptrue_,
        thn    = foldr (lam f. lam e. f e) cont (map compileStmtTppl a.ifTrueStmts),		 
        els    = foldr (lam f. lam e. f e) cont (map compileStmtTppl a.ifFalseStmts),	 
        ty     = tyunknown_,
        info   = a.info
      }
    }

  | ReturnStmtTppl r ->
  lam cont. match r.return with Some x then compileExprTppl x else unit_

  sem compileExprTppl: ExprTppl -> Expr

  sem compileExprTppl =

  | BernoulliExprTppl d ->
    TmDist {
      dist = DBernoulli {
        p = compileExprTppl d.prob
      },
      ty = tyunknown_,
      info = d.info
    }

  | VariableExprTppl v ->
    TmVar {
      ident = v.ident.v,
      ty = tyunknown_,
      info = v.info,
      frozen = false 
    }

  | RealExprTppl r ->
    use FloatAst in
    TmConst {
      val = CFloat { val = r.val.v },
      ty = tyunknown_,
      info = r.info
    }

end

-- Version of parseMCoreFile needed to get input data into the program
let parseMCoreFile = lam filename.
  use BootParser in
    parseMCoreFile
      {defaultBootParserParseMCoreFileArg with eliminateDeadCode = false}
        filename

-- Parses a TreePPL file
let parseTreePPLFile = lam filename.
  let content = readFile filename in
    use TreePPLAst in (parseTreePPLExn filename content)

-- Compiles a TreePPL program and input to a CorePPL string

let compileTreePPLToString = lam input:Expr. lam program:FileTppl.
  use TreePPLCompile in
    let corePplAst: Expr = compile input program in
      (mexprPPLToString corePplAst)

-- Compiles a TreePPL program and input to a CorePPL AST
let compileTreePPL = lam input:Expr. lam program:FileTppl.
  use TreePPLCompile in
    let corePplAst: Expr = compile input program in corePplAst

mexpr

-- test the flip example, TODO should iterate through the files instead
let testTpplProgram = parseTreePPLFile "models/flip/flip.tppl" in
let testInput = parseMCoreFile "models/flip/data.mc" in
let testMCoreProgram = parseMCorePPLFile "models/flip/flip.mc" in

-- Doesn't work TODO
-- use MExprEq in
-- utest compileTreePPL testInput testTpplProgram with testMCoreProgram using eqExpr in
-- instead do a silly thing:
use MExprPPL in
utest compileTreePPLToString testInput testTpplProgram with (mexprPPLToString testMCoreProgram) using eqString in

-- test the flip example, should iterate through the files instead
let testTpplProgram = parseTreePPLFile "models/if/if.tppl" in
let testInput = parseMCoreFile "models/if/data.mc" in
let testMCoreProgram = parseMCorePPLFile "models/if/if.mc" in

-- Doesn't work TODO 
-- use MExprEq in
-- utest compileTreePPL testInput testTpplProgram with testMCoreProgram using eqExpr in
-- instead do a silly thing:
use MExprPPL in
utest compileTreePPLToString testInput testTpplProgram with (mexprPPLToString testMCoreProgram) using eqString in

()

