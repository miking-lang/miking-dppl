/- 
TreePPL Compiler
Authors: Viktor S and Viktor P
Dates: 2022

Hello!

This is the TreePPL compiler (part of miking-dppl).

Here is the compilation pipeline:

example.tppl 
  --parse--> (part of Viktor's Tool)
TreePPL AST 
  --analysis--> (part of this compiler)
Side effects and enriched TreePPL AST 
  --compile TPPL--> (part of this compiler)
CorePPL AST + Variant Record Projection (another syn Expr...) 
  --type checker--> 
CorePPL AST + Variant Record Projection with types 
  --desugar--> 
CorePPL AST 
  --CorePPL compiler-->
 RootPPL CU file 
  --rootppl-->
RootPPL program 

To run this compiler (when it's ready), try:

  mi compile --typecheck --keep-dead-code compiler.mc
  mi compile --keep-dead-code compiler.mc
  boot eval compiler

TODO try entr to autocompile on save.

Some inspiration is drawn from the CorePPL C compiler.
-/

include "treeppl-ast.mc"
include "mexpr/ast.mc"
include "mexpr/boot-parser.mc"

include "../../coreppl/coreppl.mc" -- TODO is this the best way?

lang TreePPLCompile = TreePPLAst + CorePPL + RecLetsAst
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
      
      /-
      1. find which function is the model function f
      2. fold over arguments of model function, so that the inexpr
      becomes f(data.arg1, data.arg2....) 
      3. always add #include "data.mc" to all compiled programs
      -/
      -- this should be the application of whichever function
                      -- is the model function on the input data

  sem mInvocation: Decl -> Option Expr
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
        -- TODO Write a function that maps TreePPL types to CorePPL
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

  sem compileStmtTppl: StmtTppl -> (Expr -> Expr)

  sem compileStmtTppl = 
  | AssumeStmtTppl a ->
  lam cont. TmLet {
    ident = a.randomVar.v, 
    tyBody = tyunknown_, 
    body = TmAssume {
      dist = compileExprTppl a.dist,
      --dist = unit_,
      ty = tyunknown_,
      info = a.info
    },
    inexpr = cont,
    ty = tyunknown_,
    info = a.info
  }
               
  | ReturnStmtTppl r ->
  lam cont. compileExprTppl r.return
  
  -- unit_ -- Expr

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
  
end
   
mexpr

match argv with ![_, _, _] then
  printLn "-- Error, arguments";
  printLn "-- Correct: tpplc program.tppl data.mc";
  exit 0
else match argv with [_, filename, data] in
use BootParser in
--let input = bootParserParseMCoreFile_ data in
let input = parseMCoreFile {defaultBootParserParseMCoreFileArg with eliminateDeadCode = false}
  data in
--dprint data;
--dprint input;
let content = readFile filename in
use TreePPLAst in
match parseTreePPLExn filename content with  file in
--();
-- match file.decl with file in ();
-- dprint file

use TreePPLCompile in
-- use BernoulliDist in
  let corePplAst = compile input file in
  use MExprPPL in 
  print (mexprPPLToString corePplAst)
