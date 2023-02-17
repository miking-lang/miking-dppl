/-
TreePPL Compiler and Unit Tests
===============================

Viktor Senderov, Daniel Lundén, and Viktor Palmkvist (2022)

Unit tests available via:

  mi compile models/treeppl-to-coreppl/compile.mc --test

-/

include "../treeppl-ast.mc"
include "../src-location.mc"
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

-- Pattern match over an algebraic type where each constructor carries
-- a record, pulling out the field with the given name. Additionally,
-- only pull out that field if the type agrees with `ty`. This last
-- part is probably not optimal, but seems the easiest for now.
lang ProjMatchAst = Ast
  syn Expr =
  | TmProjMatch {info : Info, target : Expr, field : SID, ty : Type, env : TCEnv}

  sem infoTm =
  | TmProjMatch x -> x.info

  sem tyTm =
  | TmProjMatch x -> x.ty

  sem withInfo info =
  | TmProjMatch x -> TmProjMatch {x with info = info}

  sem withType ty =
  | TmProjMatch x -> TmProjMatch {x with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmProjMatch x ->
    match f acc x.target with (acc, target) in
    (acc, TmProjMatch {x with target = target})
end

lang ProjMatchPprint = PrettyPrint + ProjMatchAst
  sem isAtomic =
  | TmProjMatch _ -> false

  sem pprintCode indent env =
  | TmProjMatch x ->
    match printParen indent env x.target with (env, target) in
    (env, join [target, ".", pprintLabelString x.field])
end

lang ProjMatchTypeCheck = TypeCheck + ProjMatchAst
  sem typeCheckExpr env =
  | TmProjMatch x ->
    let target = typeCheckExpr env x.target in
    let ty = newvar env.currentLvl x.info in
    TmProjMatch {x with target = target, ty = ty, env = env}
end

lang PullName = AppTypeAst + ConTypeAst
  sem pullName =
  | TyApp x -> pullName x.lhs
  | TyCon x -> Some x.ident
  | _ -> None ()
end

lang PullNameFromConstructor = PullName + FunTypeAst + AllTypeAst
  sem pullName =
  | TyArrow x -> pullName x.to
  | TyAll x -> pullName x.ty
end

lang LowerProjMatch = ProjMatchAst + MatchAst + DataPat + RecordPat + RecordTypeAst + FunTypeAst + Eq + Unify + Generalize
  sem lowerProj : Expr -> Expr
  sem lowerProj =
  | t -> smap_Expr_Expr lowerProj t
  | TmProjMatch x ->
    let targetTyName = match (use PullName in pullName) (tyTm x.target)
      with Some ty then ty
      else errorSingle [infoTm x.target] (join ["Could not infer this to be a variant type (found ", type2str (tyTm x.target), ")"])
    in
    let expectedResultTy = x.ty in
    let filterConstructorNames : (Name, Type) -> Option (Name, Type) = lam pair.
      match pair with (conName, conTy) in
      let conTyName = match use PullNameFromConstructor in pullName conTy
        with Some tyName then tyName
        else error "Every constructor should end in a type that is named" in
      -- TODO(vipa, 2023-01-30): We would actually like to unify and
      -- then proceed *if it succeeds*, but we can't fail a
      -- unification without ending the program presently
      if nameEq targetTyName conTyName then
        match inst x.info x.env.currentLvl conTy with TyArrow arr in
        -- TODO(vipa, 2022-12-20): I'm not entirely sure we want to run
        -- arbitrary type-checking stuff after the actual typechecking
        -- phase, but I believe these uses are at least correct, and
        -- should not leak any new types into the generated code.
        unify [infoTm x.target] arr.to (tyTm x.target);
        match arr.from with recTy & TyRecord rec then
          match mapLookup x.field rec.fields with Some fieldTy then
            if eqType fieldTy expectedResultTy
            then Some (conName, recTy)
            else None ()
          else None ()
        else None ()
      else None ()
    in
    -- OPT(vipa, 2022-12-20): This potentially repeats a lot of work
    let relevantConstructors = mapOption filterConstructorNames (mapBindings x.env.conEnv) in

    -- TODO(vipa, 2022-12-20): Move these to ast-builder
    let inpcon_ = lam i. lam n. lam p. withInfoPat i (npcon_ n p) in
    let invar_ = lam i. lam n. withInfo i (nvar_ n) in
    let imatch_ = lam i. lam s. lam p. lam t. lam e. withInfo i (match_ s p t e) in
    let inpvar_ = lam i. lam n. withInfoPat i (npvar_ n) in

    let iSidRecordProj_ = lam i. lam target. lam sid. lam recordTy.
      let varName = nameSym "x" in
      let pat = PatRecord
        { bindings = mapInsert sid (inpvar_ i varName) (mapEmpty cmpSID)
        , info = i
        , ty = recordTy
        } in
      imatch_ i target pat (invar_ i varName) never_ in
    let match_ = imatch_ x.info in
    let npcon_ = inpcon_ x.info in
    let nvar_ = invar_ x.info in
    let npvar_ = inpvar_ x.info in
    let varName = nameSym "target" in
    let var = invar_ x.info varName in
    let wrap : Expr -> (Name, Type) -> Expr = lam next. lam pair.
      match pair with (conName, recTy) in
      let xName = nameSym "x" in
      match_ var (npcon_ conName (npvar_ xName))
        (iSidRecordProj_ x.info (nvar_ xName) x.field recTy)
        next
    in
    bind_
      (nulet_ varName x.target)
      (foldl wrap never_ relevantConstructors)
end

lang TreePPLCompile = TreePPLAst + MExprPPL + RecLetsAst + Externals + MExprSym + FloatAst + ProjMatchAst + Resample

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

  sem isemi_: Expr -> Expr -> Expr
  sem isemi_ l =
  | r ->
    let info = match infoTm r with info & Info _
      then info
      else infoTm l
    in withInfo info (semi_ l r)

  sem compile: Expr -> FileTppl -> Expr

  sem compile (input: Expr) =
  | FileTppl x ->
    let externals = parseMCoreFile (concat tpplSrcLoc "/src/externals/ext.mc") in
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
      else printLn "You need a model function!"; exit 1
    in
    let types = map (compileTpplTypeDecl compileContext) x.decl in
    let typeNames = mapOption (lam x. x.0) types in
    let constructors = join (map (lam x. x.1) types) in
    let bindType = lam inexpr. lam name.
      TmType {
        ident = name.v,
        params = [],
        tyIdent = tyWithInfo name.i (tyvariant_ []),
        inexpr = inexpr,
        ty = tyunknown_,
        info = name.i  -- NOTE(vipa, 2022-12-22): This makes `type T in e` just point to `T`, which might not be desirable
      }
    in
    let bindCon = lam inexpr. lam pair.
      TmConDef {
        ident = pair.0 .v,
        tyIdent = pair.1,
        inexpr = inexpr,
        ty = tyunknown_,
        info = pair.0 .i
      }
    in
    let input = foldl bindCon input constructors in
    let input = foldl bindType input typeNames in
    let complete = bind_ input (TmRecLets {
      bindings = mapOption (compileTpplFunction compileContext) x.decl,
      inexpr = invocation,
      ty = tyunknown_,
      info = x.info
    }) in
    let env = symEnvEmpty in
    symbolizeExpr ({env with varEnv = mapInsert "log" compileContext.logName (mapInsert "exp" compileContext.expName env.varEnv)}) complete

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

  sem compileTpplTypeDecl: TpplCompileContext -> DeclTppl -> (Option {v:Name, i:Info}, [({v:Name, i:Info}, Type)])
  sem compileTpplTypeDecl context =
  | TypeDeclTppl x ->
    let f = lam c. match c with Con c in
      let mkField = lam x. (x.name.v, compileTypeTppl x.ty) in
      let tycon = tyWithInfo x.name.i (ntycon_ x.name.v) in
      let record = tyWithInfo x.info (tyrecord_ (map mkField c.fields)) in
      (c.name, tyWithInfo x.info (tyarrow_ record tycon))
    in
    (Some x.name, map f x.cons)
  | _ -> (None (), [])

  sem compileTpplFunction: TpplCompileContext -> DeclTppl -> Option RecLetBinding
  sem compileTpplFunction (context: TpplCompileContext) =

  | FunDeclTppl f -> Some {
      ident = f.name.v,
      tyBody = tyunknown_,
      tyAnnot = tyWithInfo f.name.i tyunknown_,
      body =
        foldr (lam f. lam e. f e)
          (withInfo f.info unit_)
          (concat (map compileFunArg f.args) (map (compileStmtTppl context) f.body)),
      info = f.info
    }
  | TypeDeclTppl _ -> None ()

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

  | ExprStmtTppl x ->
    -- TODO(vipa, 2022-12-22): Info field for the entire semi?
    lam cont. isemi_ (compileExprTppl x.e) cont

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

  | ObserveStmtTppl x ->
    lam cont.
      let obs = TmObserve {
        info = x.info,
        value = compileExprTppl x.value,
        dist = compileExprTppl x.dist,
        ty = tyunknown_
      } in
      -- TODO(vipa, 2022-12-22): Info for semi?
      (isemi_ obs cont)

  | ResampleStmtTppl x ->
    lam cont.
      let res = TmResample { info = x.info, ty = tyunknown_ } in
      isemi_ res cont

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
    let logExpr: Expr = withInfo a.info (app_ (withInfo a.info (nvar_ context.logName)) cExpr) in
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

  | LogWeightStmtTppl a ->
    lam cont.

    let tmp = TmLet {
      ident = nameNoSym "foo",
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
      body =  TmWeight {
        weight = compileExprTppl a.value,
        ty = tyunknown_,
        info = a.info
      },
      inexpr = cont,
      ty = tyunknown_,
      info = a.info
    } in
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
        pat    = withInfoPat (get_ExprTppl_info a.condition) ptrue_,
        thn    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifTrueStmts),
        els    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifFalseStmts),
        ty     = tyunknown_,
        info   = a.info
      }
    }

  | ReturnStmtTppl r ->
    lam cont. match r.return with Some x then compileExprTppl x else withInfo r.info unit_

  | PrintStmtTppl x ->
    lam cont.
      let print = print_ (snoc_ (float2string_ (compileExprTppl x.real)) (char_ '\n')) in
      let flush = flushStdout_ unit_ in
      isemi_ (isemi_ print flush) cont

  sem compileExprTppl: ExprTppl -> Expr

  sem compileExprTppl =

  | ProjectionExprTppl x ->
    TmProjMatch {
      info = x.info,
      target = compileExprTppl x.target,
      field = stringToSid x.field.v,
      ty = tyunknown_,
      env = _tcEnvEmpty  -- TODO(vipa, 2022-12-21): This is technically supposed to be private
    }

  | FunCallExprTppl x ->
    let f = compileExprTppl x.f in
    let app = lam f. lam arg.
      TmApp {
        info = x.info,
        lhs = f,
        rhs = compileExprTppl arg,
        ty = tyunknown_
      } in
    foldl app f x.args

  | BernoulliExprTppl d ->
    TmDist {
      dist = DBernoulli {
        p = compileExprTppl d.prob
      },
      ty = tyunknown_,
      info = d.info
    }

  | GaussianExprTppl d ->
    TmDist {
      dist = DGaussian {
        mu = compileExprTppl d.mean,
        sigma = compileExprTppl d.stdDev
      },
      ty = tyunknown_,
      info = d.info
    }

  | PoissonExprTppl d ->
    TmDist {
      dist = DPoisson {
        lambda = compileExprTppl d.rate
      },
      ty = tyunknown_,
      info = d.info
    }

  | ExponentialExprTppl d ->
    TmDist {
      dist = DExponential {
        rate = compileExprTppl d.rate
      },
      ty = tyunknown_,
      info = d.info
    }

  | GammaExprTppl d ->
    TmDist {
      dist = DGamma {
        k = compileExprTppl d.shape,
        theta = compileExprTppl d.scale
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

  | IsExprTppl x ->
    TmMatch {
      info = x.info,
      target = compileExprTppl x.thing, -- and constructor
      pat = PatCon {
        info = x.constructor.i,
        ty = tyunknown_,
        ident = x.constructor.v,
        subpat = withInfoPat x.info pvarw_
      },
      thn = withInfo x.info true_,
      els = withInfo x.info false_,
      ty = tyunknown_
    }

  | ConstructorExprTppl x ->
    let mkField : {key : {v:String, i:Info}, value : Option ExprTppl} -> (SID, Expr) = lam field.
      let sid = stringToSid field.key.v in
      let val = optionMapOr (withInfo field.key.i (var_ field.key.v)) compileExprTppl field.value in
      (sid, val) in
    let fields = mapFromSeq cmpSID (map mkField x.fields) in
    let record = TmRecord { bindings = fields, ty = tyunknown_, info = x.info } in
    TmConApp { ident = x.name.v, body = record, ty = tyunknown_, info = x.info }

  -- TODO(vipa, 2022-12-22): We want to pick the right one depending
  -- on the type of the expressions, but that requires
  -- inference/type-checking. This seems like a problem that should
  -- appear in many languages, i.e., we want a good way of supporting
  -- it in MExpr. I guess a superset that includes some form of ad-hoc
  -- overleading?
  | AddExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CAddf ()
        },
        rhs = compileExprTppl x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl x.right,
      ty = tyunknown_
    }

  | SubExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CSubf ()
        },
        rhs = compileExprTppl x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl x.right,
      ty = tyunknown_
    }

  | MulExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CMulf ()
        },
        rhs = compileExprTppl x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl x.right,
      ty = tyunknown_
    }

  | DivExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CDivf ()
        },
        rhs = compileExprTppl x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl x.right,
      ty = tyunknown_
    }

  | LessEqExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CLeqf ()
        },
        rhs = compileExprTppl x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl x.right,
      ty = tyunknown_
    }

  | GreaterExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CGtf ()
        },
        rhs = compileExprTppl x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl x.right,
      ty = tyunknown_
    }

  | EqualExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CEqf ()
        },
        rhs = compileExprTppl x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl x.right,
      ty = tyunknown_
    }

  | RealExprTppl r ->
    TmConst {
      val = CFloat { val = r.val.v },
      ty = tyunknown_,
      info = r.info
    }

  | IntegerExprTppl r ->
    TmConst {
      val = CInt { val = r.val.v },
      ty = tyunknown_,
      info = r.info
    }

  | TrueExprTppl r ->
    TmConst {
      val = CBool { val = true },
      ty = tyunknown_,
      info = r.info
    }

  | FalseExprTppl r ->
    TmConst {
      val = CBool { val = false },
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
