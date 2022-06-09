/-
TreePPL Compiler
Authors: Viktor S, Viktor P, Daniel L
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

To try this compiler run:

  mi compile src/tpplc.mc
  ./tpplc program.tppl data.mc

You can install install the compiler with `make install` from the `miking-dppl` directory.
  
Some inspiration is drawn from the CorePPL C compiler.
-/

include "treeppl-ast.mc"
include "mexpr/ast.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"

include "../../coreppl/src/coreppl-to-rootppl/compile.mc"
include "../../coreppl/src/coreppl.mc" -- TODO is this the best way?

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
      weight = compileExprTppl a.val,
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
   
mexpr

-- helper to parse a Miking src with input data
let bootParse = lam data.
  use BootParser in
    parseMCoreFile {defaultBootParserParseMCoreFileArg with eliminateDeadCode = false} data
in

-- helper to parse a TreePPL src with the program
let treeParse = lam filename.
  let content = readFile filename in
    use TreePPLAst in
      (parseTreePPLExn filename content)
in

-- helper to test
let testString = lam input. lam file.
  use TreePPLCompile in
    let corePplAst: Expr = compile input file in
  (mexprPPLToString corePplAst)
in

-- test the flip example
let testCode = treeParse "models/flip/flip.tppl" in
let testInput = bootParse "models/flip/data.mc" in
utest testString testInput testCode  with strJoin "\n" [
  "recursive",
  "  let flip =",
  "    lam p: Float.",
  "      let e =",
  "        assume",
  "          (Bernoulli",
  "             p)",
  "      in",
  "      e",
  "in",
  "let p =",
  "  0.5",
  "in",
  "flip",
  "  p"
] using eqString in

match argv with ![_, _, _] then
  printLn "-- Error, arguments";
  printLn "-- Correct: tpplc program.tppl data.mc";
  exit 0
else match argv with [_, filename, data] in
use BootParser in
let input = parseMCoreFile {defaultBootParserParseMCoreFileArg with eliminateDeadCode = false}
  data in
let content = readFile filename in
use TreePPLAst in
match parseTreePPLExn filename content with  file in

use TreePPLCompile in
  let corePplAst: Expr = compile input file in
  --  TODO(vsenderov,2022-05-10): Maybe parse from the command line  
  let outfile = "out.cu" in
  let options = {default with method = "rootppl-smc"} in
  let prog: Expr = typeCheck corePplAst in
  --let prog = corePplAst in
  writeFile outfile (printCompiledRPProg (rootPPLCompile options prog));
  -- mexprCompile backend 
  print (join ["RootPPL output written.\n",
   "To get an executable, compile with \n\n  rootppl --stack-size 10000 ", outfile, "\n\n"]);
  use MExprPPL in 
  print (concat (mexprPPLToString corePplAst) "\n\n")
