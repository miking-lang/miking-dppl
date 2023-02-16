/-
TreePPL Compiler and Unit Tests
===============================

Viktor Senderov, Daniel Lundén, and Viktor Palmkvist (2022)

Unit tests available via:

  mi compile models/treeppl-to-coreppl/compile.mc --test

-/

include "../treeppl-ast.mc"
include "mexpr/ast.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"

include "sys.mc"

include "../../../coreppl/src/coreppl-to-mexpr/compile.mc"
include "../../../coreppl/src/coreppl-to-mexpr/runtimes.mc"
include "../../../coreppl/src/coreppl-to-rootppl/compile.mc"
include "../../../coreppl/src/coreppl.mc"
include "../../../coreppl/src/parser.mc"

-- Version of parseMCoreFile needed to get input data into the program
let parseMCoreFile = lam filename.
  use BootParser in
    parseMCoreFile
      {defaultBootParserParseMCoreFileArg with eliminateDeadCode = false}
        filename

lang TreePPLCompile = TreePPLAst + MExprPPL + RecLetsAst + Externals + MExprSym
-- TODO If this works it should go to externals
  sem constructExternalMap : Expr -> Map String Name
  sem constructExternalMap =
  | expr -> constructExternalMapH (mapEmpty cmpString) expr
  sem constructExternalMapH : Map String Name -> Expr -> Map String Name
  sem constructExternalMapH acc =
  | TmExt t -> constructExternalMapH (mapInsert (nameGetStr t.ident) t.ident acc) t.inexpr
  | expr -> sfold_Expr_Expr constructExternalMapH acc expr

  -- smap_Expr_Expr, sfold_Expr_Expr explained in recursion cookbook
  sem filterExternalMap: Set String -> Expr -> Expr
  sem filterExternalMap ids =
  | TmExt t ->
    let inexpr = filterExternalMap ids t.inexpr in
    --if (nameGetStr t.ident) in ids then TmExt {t with inexpr = inexpr}
    match setMem (nameGetStr t.ident) ids with true then TmExt {t with inexpr = inexpr}
    else inexpr
  | expr -> smap_Expr_Expr (filterExternalMap ids) expr
  | TmLet t -> filterExternalMap ids t.inexpr -- strips all the lets

  -- a type with useful information passed down from compile
  type TpplCompileContext = {
    logName: Name,
    expName: Name
  }

  sem compile: Expr -> FileTppl -> Expr

  sem compile (input: Expr) =
  | FileTppl x ->
    let externals = parseMCoreFile "src/externals/ext.mc" in
    let exts = setOfSeq cmpString ["externalLog", "externalExp"] in
    let externals = filterExternalMap exts externals in  -- strip everything but needed stuff from externals
    let externals = symbolize externals in
    let externalMap = constructExternalMap externals in
    let compileContext: TpplCompileContext = {
      logName = mapFindExn "externalLog" externalMap,
      expName = mapFindExn "externalExp" externalMap
    } in
    let input = bind_ externals input in
    --dprint x;
    let invocation = match findMap mInvocation x.decl with Some x
      then x
      else printLn "You need a model function"; never
    in
    bind_ input (TmRecLets {
      bindings = map (compileTpplDecl compileContext) x.decl,
      inexpr = invocation,
      ty = tyunknown_,
      info = x.info
    })

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

  sem compileTpplDecl: TpplCompileContext -> DeclTppl -> RecLetBinding
  sem compileTpplDecl (context: TpplCompileContext) =

  | FunDeclTppl f -> {
      ident = f.name.v,
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
      body =
        foldr (lam f. lam e. f e) unit_ (concat (map compileFunArg f.args) (map (compileStmtTppl context) f.body)),
      info = f.info
    }

  sem compileFunArg: {name:{v:Name, i:Info}, ty:TypeTppl} -> (Expr -> Expr)

  sem compileFunArg =
  | arg ->
    lam cont.
    TmLam {
      ident = arg.name.v,
      tyIdent = tyunknown_,
      tyAnnot = compileTypeTppl arg.ty,
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

  sem compileStmtTppl: TpplCompileContext -> StmtTppl -> (Expr -> Expr)

  sem compileStmtTppl (context: TpplCompileContext) =

  | AssumeStmtTppl a ->
    lam cont. TmLet {
      ident = a.randomVar.v,
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
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
      tyAnnot = tyunknown_,
      body =  compileExprTppl a.val,
      inexpr = cont,
      ty = tyunknown_,
      info = a.info
    }

  | WeightStmtTppl a ->
    lam cont.

    let cExpr: Expr = (compileExprTppl a.value) in
    let logExpr: Expr = withInfo a.info (app_ (nvar_ context.logName) cExpr) in
    let tmp = TmLet {
      ident = nameNoSym "foo",
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
      body =  TmWeight {
        weight = logExpr,
        --weight = cExpr,
        ty = tyunknown_,
        info = a.info
      },
      inexpr = cont,
      ty = tyunknown_,
      info = a.info
    } in
    --printLn (mexprPPLToString tmp);
    tmp

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
    let contName = nameSym "ifCont" in
    let contF = lam_ "" tyint_ cont in -- continuation function
    let cont: Expr = withInfo a.info (app_ (nvar_ contName) (int_ 0)) in
    TmLet {
      ident = contName,
      body = contF,
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
      ty = tyunknown_,
      info = a.info,
      inexpr = TmMatch {
        target = compileExprTppl a.condition,
        pat    = ptrue_,
        thn    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifTrueStmts),
        els    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifFalseStmts),
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



-- Parses a TreePPL file
let parseTreePPLFile = lam filename.
  let content = readFile filename in
    use TreePPLAst in (parseTreePPLExn filename content)

-- Compiles a TreePPL program and input to a CorePPL string

let compileTreePPLToString = lam input:Expr. lam program:FileTppl.
  use TreePPLCompile in
    let corePplAst: Expr = compile input program in
      use MExprPPL in
        (mexprPPLToString corePplAst)

-- Compiles a TreePPL program and input to a CorePPL AST
let compileTreePPL = lam input:Expr. lam program:FileTppl.
  use TreePPLCompile in
    let corePplAst: Expr = compile input program in corePplAst

mexpr

-- test the flip example, TODO should iterate through the files instead
let testTpplProgram = parseTreePPLFile "models/flip/flip.tppl" in
let testInput = parseMCoreFile "models/flip/data.mc" in
let testMCoreProgram = parseMCorePPLFileNoDeadCodeElimination "models/flip/flip.mc" in


-- Doesn't work TODO
use MExprPPL in
utest compileTreePPL testInput testTpplProgram with testMCoreProgram using eqExpr in

-- test the if example, should iterate through the files instead
let testTpplProgram = parseTreePPLFile "models/if/if.tppl" in
let testInput = parseMCoreFile "models/if/data.mc" in
let testMCoreProgram = parseMCorePPLFileNoDeadCodeElimination "models/if/if.mc" in

--debug pretty printing
--use TreePPLCompile in
--printLn (mexprToString testMCoreProgram);

--use MExprPPL in
utest compileTreePPL testInput testTpplProgram with testMCoreProgram using eqExpr in

-- test the externals example, should iterate through the files instead
let testTpplProgram = parseTreePPLFile "models/externals/externals.tppl" in
let testInput = parseMCoreFile "models/externals/data.mc" in
let testMCoreProgram = parseMCorePPLFileNoDeadCodeElimination "models/externals/externals.mc" in

--use MExprPPL in
utest compileTreePPL testInput testTpplProgram with testMCoreProgram using eqExpr in

-- If you want to print out the strings, use the following:
-- use MExprPPL in
--utest compileTreePPLToString testInput testTpplProgram with (mexprPPLToString testMCoreProgram) using eqString in

()
