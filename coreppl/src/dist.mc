
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/info.mc"
include "mexpr/eq.mc"
include "mexpr/type-check.mc"
include "mexpr/anf.mc"
include "mexpr/type-lift.mc"
include "mexpr/const-arity.mc"
include "mexpr/const-types.mc"

include "peval/peval.mc"

include "string.mc"
include "seq.mc"
include "utest.mc"

lang Dist = PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift + PEval +
            TyConst + ConstPrettyPrint + ConstArity
  syn Expr =
  | TmDist { dist: Dist,
             ty: Type,
             info: Info }

  syn Type =
  | TyDist { info : Info,
             ty   : Type }

  syn Dist =
  -- Intentionally left blank

  sem infoTm =
  | TmDist t -> t.info

  sem tyTm =
  | TmDist t -> t.ty

  sem withInfo (info: Info) =
  | TmDist t -> TmDist { t with info = info }

  sem withType (ty: Type) =
  | TmDist t -> TmDist { t with ty = ty }

  sem smapAccumLDist_Expr_Expr : all acc. (acc -> Expr -> (acc, Expr)) -> acc -> Dist -> (acc, Dist)
  sem smapAccumLDist_Expr_Expr f acc =
  | dist -> (acc, dist)

  sem smapDist_Expr_Expr : (Expr -> Expr) -> Dist -> Dist
  sem smapDist_Expr_Expr f =
  | p ->
    let res: ((), Dist) = smapAccumLDist_Expr_Expr (lam. lam a. ((), f a)) () p in
    res.1

  sem sfoldDist_Expr_Expr : all acc. (acc -> Expr -> acc) -> acc -> Dist -> acc
  sem sfoldDist_Expr_Expr f acc =
  | p ->
    let res: (acc, Dist) = smapAccumLDist_Expr_Expr (lam acc. lam a. (f acc a, a)) acc p in
    res.0

  sem smapAccumL_Expr_Expr f acc =
  | TmDist t ->
    match smapAccumLDist_Expr_Expr f acc t.dist with (acc,dist) in
    (acc, TmDist { t with dist = dist })

  sem infoTy =
  | TyDist t -> t.info

  sem tyWithInfo (info : Info) =
  | TyDist t -> TyDist {t with info = info}

  sem smapAccumL_Type_Type f acc =
  | TyDist t ->
    match f acc t.ty with (acc, ty) in
    (acc, TyDist {t with ty = ty})

  -- Pretty printing
  sem isAtomic =
  | TmDist _ -> false

  sem pprintDist (indent: Int) (env: PprintEnv) =
  -- Intentionally left blank

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmDist t -> pprintDist indent env t.dist

  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyDist t ->
    match getTypeStringCode indent env t.ty with (env, ty) then
      (env, join ["Dist(", ty, ")"])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  -- Intentionally left blank

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDist r ->
    match lhs with TmDist l then eqExprHDist env free l.dist r.dist else None ()

  sem eqTypeH (typeEnv : EqTypeEnv) (free : EqTypeFreeEnv) (lhs : Type) =
  | TyDist r ->
    match unwrapType lhs with TyDist l then
      eqTypeH typeEnv free l.ty r.ty
    else None ()

  sem cmpTypeH : (Type, Type) -> Int
  sem cmpTypeH =
  | (TyDist l, TyDist r) -> cmpType l.ty r.ty

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  -- Intentionally left blank

  sem symbolizeExpr (env: SymEnv) =
  | TmDist t ->
    TmDist {{ t with dist = symbolizeDist env t.dist }
                with ty = symbolizeType env t.ty }

  -- Type checking
  sem typeCheckDist : TCEnv -> Info -> Dist -> Type
  -- Intentionally left blank
  sem typeCheckExpr (env : TCEnv) =
  | TmDist t ->
    let dist = smapDist_Expr_Expr (typeCheckExpr env) t.dist in
    let innerTyDist = typeCheckDist env t.info dist in
    let innerTyDistVar = newvar env.currentLvl t.info in
    unify env [t.info] innerTyDistVar innerTyDist;
    TmDist {{ t with dist = dist }
                with ty = TyDist { info = t.info, ty = innerTyDistVar } }
  sem unifyBase: all u. Unifier u -> UnifyEnv -> (Type, Type) -> u
  sem unifyBase u env =
  | (TyDist t1, TyDist t2) -> unifyTypes u env (t1.ty, t2.ty)

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  -- Intentionally left blank

  sem normalize (k : Expr -> Expr) =
  | TmDist ({ dist = dist } & t) ->
    normalizeDist (lam dist. k (TmDist { t with dist = dist })) dist

  -- CPS
  sem cpsCont k =
  | TmLet ({ body = TmDist _ } & t) ->
    TmLet { t with inexpr = cpsCont k t.inexpr }

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  -- Intentionally left blank

  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmDist t ->
    match typeLiftDist env t.dist with (env, dist) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmDist {{ t with dist = dist }
                          with ty = ty })
      else never
    else never

  sem typeLiftType (env : TypeLiftEnv) =
  | TyDist t ->
    match typeLiftType env t.ty with (env, ty) then
      (env, TyDist {t with ty = ty})
    else never

  -- Partial evaluation
  sem pevalIsValue =
  | TmDist _ -> false

  sem pevalDistEval ctx k =
  -- Intentionally left blank

  sem pevalEval ctx k =
  | TmDist t ->
    pevalDistEval ctx (lam dist. k (TmDist { t with dist = dist }) ) t.dist

  -- Builtin operations on distributions
  syn Const =
  | CDistEmpiricalSamples {}
  | CDistEmpiricalDegenerate {}
  | CDistEmpiricalNormConst {}
  | CDistEmpiricalAcceptRate {}

  sem getConstStringCode (indent : Int) =
  | CDistEmpiricalSamples _ -> "distEmpiricalSamples"
  | CDistEmpiricalDegenerate _ -> "distEmpiricalDegenerate"
  | CDistEmpiricalNormConst _ -> "distEmpiricalNormConst"
  | CDistEmpiricalAcceptRate _ -> "distEmpiricalAcceptRate"

  sem _tydist =
  | a -> TyDist {info = NoInfo (), ty = a}

  sem tyConstBase d =
  | CDistEmpiricalSamples _ ->
    tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a"))
                  (tytuple_ [tyseq_ (tyvar_ "a"), tyseq_ tyfloat_]))
  | CDistEmpiricalDegenerate _ -> tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a")) tybool_)
  | CDistEmpiricalNormConst _ -> tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a")) tyfloat_)
  | CDistEmpiricalAcceptRate _ -> tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a")) tyfloat_)

  sem constArity =
  | CDistEmpiricalSamples _ -> 1
  | CDistEmpiricalDegenerate _ -> 1
  | CDistEmpiricalNormConst _ -> 1
  | CDistEmpiricalAcceptRate _ -> 1

end


lang UniformDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DUniform { a: Expr, b: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DUniform t ->
    match f acc t.a with (acc,a) in
    match f acc t.b with (acc,b) in
    (acc, DUniform {{t with a = a} with b = b})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DUniform t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a, t.b] with (env,args) then
      (env, join ["Uniform", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DUniform r ->
    match lhs with DUniform l then
      match eqExprH env free l.a r.a with Some free then
        eqExprH env free l.b r.b
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DUniform t -> DUniform {{ t with a = symbolizeExpr env t.a }
                                with b = symbolizeExpr env t.b }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DUniform t ->
    let float = TyFloat { info = info } in
    unify env [info, infoTm t.a] float (tyTm t.a);
    unify env [info, infoTm t.b] float (tyTm t.b);
    float

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DUniform ({ a = a, b = b } & t) ->
    normalizeName (lam a.
      normalizeName (lam b.
        k (DUniform {{ t with a = a } with b = b})) b) a

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DUniform ({ a = a, b = b } & t) ->
    match typeLiftExpr env a with (env, a) then
      match typeLiftExpr env b with (env, b) then
        (env, DUniform {{ t with a = a }
                            with b = b })
      else never
    else never

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DUniform t ->
    pevalBind ctx
      (lam a.
        pevalBind ctx
          (lam b. k (DUniform {t with a=a, b=b}))
          t.b)
      t.a

end





lang BernoulliDist = Dist + PrettyPrint + Eq + Sym + BoolTypeAst + FloatTypeAst

  syn Dist =
  | DBernoulli { p: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DBernoulli t ->
    match f acc t.p with (acc,p) in
    (acc, DBernoulli {t with p = p})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBernoulli t ->
    let i = pprintIncr indent in
    match printParen i env t.p with (env,p) then
      (env, join ["Bernoulli", pprintNewline i, p])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DBernoulli r ->
    match lhs with DBernoulli l then eqExprH env free l.p r.p else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DBernoulli t -> DBernoulli { t with p = symbolizeExpr env t.p }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DBernoulli t ->
    unify env [info, infoTm t.p] (TyFloat { info = info }) (tyTm t.p);
    TyBool { info = info }

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DBernoulli ({ p = p } & t) ->
    normalizeName (lam p. k (DBernoulli { t with p = p })) p

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DBernoulli ({ p = p } & t) ->
    match typeLiftExpr env p with (env, p) then
      (env, DBernoulli {t with p = p})
    else never

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DBernoulli t ->
    pevalBind ctx
      (lam p. k (DBernoulli {t with p=p}))
      t.p
end



lang PoissonDist = Dist + PrettyPrint + Eq + Sym + IntTypeAst + FloatTypeAst

  syn Dist =
  | DPoisson { lambda: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DPoisson t ->
    match f acc t.lambda with (acc,lambda) in
    (acc, DPoisson {t with lambda = lambda})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DPoisson t ->
    let i = pprintIncr indent in
    match printParen i env t.lambda with (env,lambda) then
      (env, join ["Poisson", pprintNewline i, lambda])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DPoisson r ->
    match lhs with DPoisson l then eqExprH env free l.lambda r.lambda else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DPoisson t -> DPoisson { t with lambda = symbolizeExpr env t.lambda }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DPoisson t ->
    unify env [info, infoTm t.lambda] (TyFloat { info = info }) (tyTm t.lambda);
    TyInt { info = info }

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DPoisson ({ lambda = lambda } & t) ->
    normalizeName (lam lambda. k (DPoisson { t with lambda = lambda })) lambda

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DPoisson ({ lambda = lambda } & t) ->
    match typeLiftExpr env lambda with (env, lambda) then
      (env, DPoisson {t with lambda = lambda})
    else never

  sem pevalDistEval ctx k =
  | DPoisson t ->
    pevalBind ctx
      (lam lambda. k (DPoisson {t with lambda=lambda}))
      t.lambda

end




lang BetaDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DBeta { a: Expr, b: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DBeta t ->
    match f acc t.a with (acc,a) in
    match f acc t.b with (acc,b) in
    (acc, DBeta {{t with a = a} with b = b})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBeta t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a, t.b] with (env,args) then
      (env, join ["Beta", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DBeta r ->
    match lhs with DBeta l then
      match eqExprH env free l.a r.a with Some free then
        eqExprH env free l.b r.b
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DBeta t -> DBeta {{ t with a = symbolizeExpr env t.a }
                          with b = symbolizeExpr env t.b }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DBeta t ->
    let float = TyFloat { info = info } in
    unify env [info, infoTm t.a] float (tyTm t.a);
    unify env [info, infoTm t.b] float (tyTm t.b);
    float

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DBeta ({ a = a, b = b } & t) ->
    normalizeName (lam a.
      normalizeName (lam b.
        k (DBeta {{ t with a = a } with b = b})) b) a

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DBeta ({ a = a, b = b } & t) ->
    match typeLiftExpr env a with (env, a) then
      match typeLiftExpr env b with (env, b) then
        (env, DBeta {{ t with a = a }
                         with b = b })
      else never
    else never

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DBeta t ->
    pevalBind ctx
      (lam a.
        pevalBind ctx
          (lam b. k (DBeta {t with a=a, b=b}))
          t.b)
      t.a

end




lang GammaDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DGamma { k: Expr, theta: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DGamma t ->
    match f acc t.k with (acc,k) in
    match f acc t.theta with (acc,theta) in
    (acc, DGamma {{t with k = k} with theta = theta})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DGamma t ->
    let i = pprintIncr indent in
    match printArgs i env [t.k, t.theta] with (env,args) then
      (env, join ["Gamma", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DGamma r ->
    match lhs with DGamma l then
      match eqExprH env free l.k r.k with Some free then
        eqExprH env free l.theta r.theta
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DGamma t -> DGamma {{ t with k = symbolizeExpr env t.k }
                            with theta = symbolizeExpr env t.theta }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DGamma t ->
    let float = TyFloat { info = info } in
    unify env [info, infoTm t.k] float (tyTm t.k);
    unify env [info, infoTm t.theta] float (tyTm t.theta);
    float

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DGamma ({ k = k2, theta = theta } & t) ->
    normalizeName (lam k2.
      normalizeName (lam theta.
       k (DGamma {{ t with k = k2 } with theta = theta})) theta) k2

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DGamma ({ k = k, theta = theta } & t) ->
    match typeLiftExpr env k with (env, k) then
      match typeLiftExpr env theta with (env, theta) then
        (env, DGamma {{ t with k = k }
                          with theta = theta })
      else never
    else never

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DGamma t ->
    pevalBind ctx
      (lam k2.
        pevalBind ctx
          (lam theta. k (DGamma {t with k=k2, theta=theta}))
          t.theta)
      t.k

end




-- DCategorical {p=p} is equivalent to DMultinomial {n=1, p=p}
lang CategoricalDist =
  Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- p has type [Float]: the list of probabilities
  | DCategorical { p: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DCategorical t ->
    match f acc t.p with (acc,p) in
    (acc, DCategorical {t with p = p})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DCategorical t ->
    let i = pprintIncr indent in
    match printArgs i env [t.p] with (env,p) then
      (env, join ["Categorical", pprintNewline i, p])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DCategorical { p = p2 } ->
    match lhs with DCategorical { p = p1 } then
      eqExprH env free p1 p2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DCategorical t ->
    DCategorical { t with p = symbolizeExpr env t.p }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DCategorical t ->
    let float = TyFloat { info = info } in
    let seq = TySeq { ty = TyFloat { info = info }, info = info } in
    unify env [info, infoTm t.p] seq (tyTm t.p);
    TyInt { info = info }

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DCategorical ({ p = p } & t) ->
    normalizeName (lam p. k (DCategorical {t with p = p})) p

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DCategorical ({ p = p } & t) ->
    match typeLiftExpr env p with (env, p) then
      (env, DCategorical {t with p = p})
    else never

  sem pevalDistEval ctx k =
  | DCategorical t ->
    pevalBind ctx
      (lam p. k (DCategorical {t with p=p}))
      t.p

end

lang MultinomialDist =
  Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- n has type Int: the number of trials
  -- p has type [Float]: the list of probabilities
  | DMultinomial { n: Expr, p: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DMultinomial t ->
    match f acc t.n with (acc,n) in
    match f acc t.p with (acc,p) in
    (acc, DMultinomial {{t with n = n} with p = p})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DMultinomial t ->
    let i = pprintIncr indent in
    match printArgs i env [t.n, t.p] with (env,args) then
      (env, join ["Multinomial", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DMultinomial { n = n2, p = p2 } ->
    match lhs with DMultinomial { n = n1, p = p1 } then
      match eqExprH env free n1 n2 with Some free then
        eqExprH env free p1 p2
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DMultinomial t ->
    DMultinomial {{ t with n = symbolizeExpr env t.n }
                      with p = symbolizeExpr env t.p }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DMultinomial t ->
    unify env [info, infoTm t.n] (TyInt { info = info }) (tyTm t.n);
    unify env [info, infoTm t.p]
      (TySeq { ty = TyFloat { info = info }, info = info }) (tyTm t.p);
    TySeq { ty = TyInt { info = info }, info = info }

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DMultinomial ({ n = n, p = p } & t) ->
    normalizeName (lam n.
      normalizeName (lam p.
        k (DMultinomial {{ t with n = n } with p = p})) p) n

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DMultinomial ({ n = n, p = p } & t) ->
    match typeLiftExpr env n with (env, n) then
      match typeLiftExpr env p with (env, p) then
        (env, DMultinomial {{ t with n = n }
                                with p = p })
      else never
    else never

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DMultinomial t ->
    pevalBind ctx
      (lam n.
        pevalBind ctx
          (lam p. k (DMultinomial {t with n=n, p=p}))
          t.p)
      t.n

end

lang DirichletDist = Dist + PrettyPrint + Eq + Sym + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- a has type [Float]: the list of concentration parameters
  | DDirichlet { a: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DDirichlet t ->
    match f acc t.a with (acc,a) in
    (acc, DDirichlet {t with a = a})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DDirichlet t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a] with (env,a) then
      (env, join ["Dirichlet", pprintNewline i, a])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DDirichlet { a = a2 } ->
    match lhs with DDirichlet { a = a1 } then
      eqExprH env free a1 a2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DDirichlet t ->
    DDirichlet { t with a = symbolizeExpr env t.a }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DDirichlet t ->
    let seqTy = TySeq { ty = TyFloat { info = info }, info = info } in
    unify env [info, infoTm t.a] seqTy (tyTm t.a); seqTy

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DDirichlet ({ a = a } & t) ->
    normalizeName (lam a. k (DDirichlet { t with a = a })) a

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DDirichlet ({ a = a } & t) ->
    match typeLiftExpr env a with (env, a) then
      (env, DDirichlet {t with a = a})
    else never

  sem pevalDistEval ctx k =
  | DDirichlet t ->
    pevalBind ctx
      (lam a. k (DDirichlet {t with a=a}))
      t.a

end

lang ExponentialDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DExponential { rate: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DExponential t ->
    match f acc t.rate with (acc,rate) in
    (acc, DExponential {t with rate = rate})

  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DExponential t ->
    let i = pprintIncr indent in
    match printParen i env t.rate with (env,rate) then
      (env, join ["Exponential", pprintNewline i, rate])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DExponential r ->
    match lhs with DExponential l then eqExprH env free l.rate r.rate else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DExponential t -> DExponential { t with rate = symbolizeExpr env t.rate }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DExponential t ->
    let float = TyFloat { info = info } in
    unify env [info, infoTm t.rate] float (tyTm t.rate); float

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DExponential ({ rate = rate } & t) ->
    normalizeName (lam rate. k (DExponential { t with rate = rate })) rate

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DExponential ({ rate = rate } & t) ->
    match typeLiftExpr env rate with (env, rate) then
      (env, DExponential {t with rate = rate})
    else never

  sem pevalDistEval ctx k =
  | DExponential t ->
    pevalBind ctx
      (lam rate. k (DExponential {t with rate=rate}))
      t.rate

end

lang EmpiricalDist =
  Dist + PrettyPrint + Eq + Sym + FloatTypeAst + RecordTypeAst + SeqTypeAst

  syn Dist =
  -- samples has type [(Float,a)]: A set of weighted samples over type a
  | DEmpirical { samples: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DEmpirical t ->
    match f acc t.samples with (acc,samples) in
    (acc, DEmpirical {t with samples = samples})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DEmpirical t ->
    let i = pprintIncr indent in
    match printParen i env t.samples with (env,samples) then
      (env, join ["Empirical", pprintNewline i, samples])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DEmpirical { samples = s2 } ->
    match lhs with DEmpirical { samples = s1 } then
      eqExprH env free s1 s2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DEmpirical t ->
    DEmpirical { t with samples = symbolizeExpr env t.samples }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DEmpirical t ->
    let resTy = newvar env.currentLvl info in
    let innerTy = tyWithInfo info (tytuple_ [TyFloat { info = info }, resTy]) in
    unify env [info, infoTm t.samples]
      (TySeq { ty = innerTy, info = info }) (tyTm t.samples);
    resTy

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DEmpirical ({ samples = samples } & t) ->
      normalizeName
        (lam samples. k (DEmpirical { t with samples = samples })) samples

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DEmpirical ({ samples = samples } & t) ->
    match typeLiftExpr env samples with (env, samples) then
      (env, DEmpirical {t with samples = samples})
    else never

  sem pevalDistEval ctx k =
  | DEmpirical t ->
    pevalBind ctx
      (lam samples. k (DEmpirical {t with samples=samples}))
      t.samples

end

lang GaussianDist =
  Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DGaussian { mu: Expr, sigma: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DGaussian t ->
    match f acc t.mu with (acc,mu) in
    match f acc t.sigma with (acc,sigma) in
    (acc, DGaussian {{t with mu = mu} with sigma = sigma})

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DGaussian t ->
    let i = pprintIncr indent in
    match printArgs i env [t.mu, t.sigma] with (env,args) then
      (env, join ["Gaussian", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DGaussian r ->
    match lhs with DGaussian l then
      match eqExprH env free l.mu r.mu with Some free then
        eqExprH env free l.sigma r.sigma
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DGaussian t -> DGaussian {{ t with mu = symbolizeExpr env t.mu }
                                  with sigma = symbolizeExpr env t.sigma }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DGaussian t ->
    let float = TyFloat { info = info } in
    unify env [info, infoTm t.mu] float (tyTm t.mu);
    unify env [info, infoTm t.mu] float (tyTm t.sigma); float

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DGaussian ({ mu = mu, sigma = sigma } & t) ->
    normalizeName (lam mu.
      normalizeName (lam sigma.
       k (DGaussian {{ t with mu = mu } with sigma = sigma})) sigma) mu

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DGaussian ({ mu = mu, sigma = sigma } & t) ->
    match typeLiftExpr env mu with (env, mu) then
      match typeLiftExpr env sigma with (env, sigma) then
        (env, DGaussian {{ t with mu = mu }
                          with sigma = sigma })
      else never
    else never

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DGaussian t ->
    pevalBind ctx
      (lam mu.
        pevalBind ctx
          (lam sigma. k (DGaussian {t with mu=mu, sigma=sigma}))
          t.sigma)
      t.mu

end

lang BinomialDist = Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + BoolTypeAst + FloatTypeAst

  syn Dist =
  | DBinomial { n: Expr, p: Expr }

  sem smapAccumLDist_Expr_Expr f acc =
  | DBinomial t ->
    match f acc t.n with (acc,n) in
    match f acc t.p with (acc,p) in
    (acc, DBinomial {{t with n = n} with p = p})

   -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBinomial t ->
    let i = pprintIncr indent in
    match printArgs i env [t.n, t.p] with (env,args) then
      (env, join ["Binomial", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DBinomial r ->
    match lhs with DBinomial l then
      match eqExprH env free l.n r.n with Some free then
        eqExprH env free l.p r.p
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DBinomial t -> DBinomial {{ t with n = symbolizeExpr env t.n }
                                  with p = symbolizeExpr env t.p }

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DBinomial t ->
    let int = TyInt { info = info } in
    let float = TyFloat { info = info } in
    unify env [info, infoTm t.n] int (tyTm t.n);
    unify env [info, infoTm t.p] float (tyTm t.p); int

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DBinomial ({ n = n, p = p } & t) ->
    normalizeName (lam n.
      normalizeName (lam p.
       k (DBinomial {{ t with n = n } with p = p})) p) n

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DBinomial ({ n = n, p = p } & t) ->
    match typeLiftExpr env n with (env, n) then
      match typeLiftExpr env p with (env, p) then
        (env, DBinomial {{ t with n = n }
                          with p = p })
      else never
    else never

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DBinomial t ->
    pevalBind ctx
      (lam n.
        pevalBind ctx
          (lam p. k (DBinomial {t with n=n, p=p}))
          t.p)
      t.n

end



lang WienerDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst
  syn Dist =
  | DWiener {}

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DWiener t -> (env, "Wiener ()")

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Dist) =
  | DWiener r ->
    match lhs with DWiener l then Some free
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DWiener t -> DWiener t

  -- Type Check
  sem typeCheckDist (env: TCEnv) (info: Info) =
  | DWiener t ->
    let tyfloat = TyFloat { info = info } in
    ityarrow_ info tyfloat tyfloat

  -- ANF
  sem normalizeDist (k : Dist -> Expr) =
  | DWiener t -> k (DWiener t)

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DWiener t -> (env, DWiener t)

  -- Partial evaluation
  sem pevalDistEval ctx k =
  | DWiener t -> k (DWiener t)
end

-----------------
-- AST BUILDER --
-----------------

let dist_ = use Dist in
  lam d. TmDist {dist = d, ty = tyunknown_, info = NoInfo ()}

let tydist_ = use Dist in
  lam ty. _tydist ty

let uniform_ = use UniformDist in
  lam a. lam b. dist_ (DUniform {a = a, b = b})

let bern_ = use BernoulliDist in
  lam p. dist_ (DBernoulli {p = p})

let poisson_ = use PoissonDist in
  lam lambda. dist_ (DPoisson {lambda = lambda})

let beta_ = use BetaDist in
  lam a. lam b. dist_ (DBeta {a = a, b = b})

let gamma_ = use GammaDist in
  lam k. lam theta. dist_ (DGamma {k = k, theta = theta})

let categorical_ = use CategoricalDist in
  lam p. dist_ (DCategorical {p = p})

let multinomial_ = use MultinomialDist in
  lam n. lam p. dist_ (DMultinomial {n = n, p = p})

let dirichlet_ = use DirichletDist in
  lam a. dist_ (DDirichlet {a = a})

let exp_ = use ExponentialDist in
  lam rate. dist_ (DExponential {rate = rate})

let empirical_ = use EmpiricalDist in
  lam lst. dist_ (DEmpirical {samples = lst})

let gaussian_ = use GaussianDist in
  lam mu. lam sigma. dist_ (DGaussian {mu = mu, sigma = sigma})

let binomial_ = use BinomialDist in
  lam n. lam p. dist_ (DBinomial {n = n, p = p})

let wiener_ = use WienerDist in dist_ (DWiener {})

---------------------------
-- LANGUAGE COMPOSITIONS --
---------------------------

lang DistAll =
  UniformDist + BernoulliDist + PoissonDist + BetaDist + GammaDist +
  CategoricalDist + MultinomialDist + DirichletDist +  ExponentialDist +
  EmpiricalDist + GaussianDist + BinomialDist + WienerDist
end

lang Test =
  DistAll + MExprAst + MExprPrettyPrint + MExprEq + MExprSym + MExprTypeCheck
  + MExprANF + MExprTypeLift
end

mexpr


use Test in

let tmUniform = uniform_ (float_ 1.0) (float_ 2.0) in
let tmBernoulli = bern_ (float_ 0.5) in
let tmPoisson = poisson_ (float_ 0.5) in
let tmBeta = beta_ (float_ 1.0) (float_ 2.0) in
let tmGamma = gamma_ (float_ 1.0) (float_ 2.0) in
let tmCategorical =
  categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]) in
let tmMultinomial =
  multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]) in
let tmExponential = exp_ (float_ 1.0) in
let tmEmpirical = empirical_ (seq_ [
    utuple_ [float_ 1.0, float_ 1.5],
    utuple_ [float_ 3.0, float_ 1.3]
  ]) in
let tmDirichlet = dirichlet_ (seq_ [float_ 1.3, float_ 1.3, float_ 1.5]) in
let tmGaussian = gaussian_ (float_ 0.0) (float_ 1.0) in
let tmBinomial = binomial_ (int_ 5) (float_ 0.5) in
let tmWiener = wiener_ in

------------------------
-- PRETTY-PRINT TESTS --
------------------------

utest mexprToString tmUniform with strJoin "\n" [
  "Uniform",
  "  1. 2."
] using eqString in

utest mexprToString tmBernoulli with strJoin "\n" [
  "Bernoulli",
  "  0.5"
] using eqString in

utest mexprToString tmPoisson with strJoin "\n" [
  "Poisson",
  "  0.5"
] using eqString in

utest mexprToString tmBeta with strJoin "\n" [
  "Beta",
  "  1. 2."
] using eqString in

utest mexprToString tmGamma with strJoin "\n" [
  "Gamma",
  "  1. 2."
] using eqString in

utest mexprToString tmCategorical with strJoin "\n" [
  "Categorical",
  "  [ 0.3, 0.2, 0.5 ]"
] using eqString in

utest mexprToString tmMultinomial with strJoin "\n" [
  "Multinomial",
  "  5 [ 0.3, 0.2, 0.5 ]"
] using eqString in

utest mexprToString tmExponential with strJoin "\n" [
  "Exponential",
  "  1."
] using eqString in

utest mexprToString tmEmpirical with strJoin "\n" [
  "Empirical",
  "  [ (1., 1.5),",
  "    (3., 1.3) ]"
] using eqString in

utest mexprToString tmDirichlet with strJoin "\n" [
  "Dirichlet",
  "  [ 1.3, 1.3, 1.5 ]"
] using eqString in

utest mexprToString tmGaussian with strJoin "\n" [
  "Gaussian",
  "  0. 1."
] using eqString in

utest mexprToString tmBinomial with strJoin "\n" [
  "Binomial",
  "  5 0.5"
] using eqString in

utest mexprToString tmWiener with "Wiener ()"
using eqString in

--------------------
-- EQUALITY TESTS --
--------------------

utest tmUniform with tmUniform using eqExpr in
utest eqExpr tmUniform (uniform_ (float_ 1.0) (float_ 1.0)) with false in

utest tmBernoulli with tmBernoulli using eqExpr in
utest eqExpr tmBernoulli (bern_ (float_ 0.4)) with false in

utest tmPoisson with tmPoisson using eqExpr in
utest eqExpr tmPoisson (poisson_ (float_ 0.4)) with false in

utest tmBeta with tmBeta using eqExpr in
utest eqExpr tmBeta (beta_ (float_ 1.0) (float_ 1.0)) with false in

utest tmGamma with tmGamma using eqExpr in
utest eqExpr tmGamma (gamma_ (float_ 1.0) (float_ 1.0)) with false in

utest tmCategorical with tmCategorical using eqExpr in
utest eqExpr tmCategorical
  (categorical_ (seq_ [float_ 0.2, float_ 0.2, float_ 0.5])) with false in
utest eqExpr tmCategorical
  (categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.6])) with false in

utest tmMultinomial with tmMultinomial using eqExpr in
utest eqExpr tmMultinomial
  (multinomial_ (int_ 4) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]))
with false in
utest eqExpr tmMultinomial
  (multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.3, float_ 0.5]))
with false in

utest tmExponential with tmExponential using eqExpr in
utest eqExpr tmExponential (exp_ (float_ 1.1)) with false in

utest tmEmpirical with tmEmpirical using eqExpr in
utest eqExpr tmEmpirical (empirical_ (seq_ [
    utuple_ [float_ 2.0, float_ 1.5],
    utuple_ [float_ 3.0, float_ 1.3]
  ]))
with false in
utest eqExpr tmEmpirical (empirical_ (seq_ [
    utuple_ [float_ 2.0, float_ 1.5],
    utuple_ [float_ 3.0, float_ 1.4]
  ]))
with false in

utest tmDirichlet with tmDirichlet using eqExpr in
utest eqExpr tmDirichlet
  (dirichlet_ (seq_ [float_ 1.2, float_ 0.5, float_ 1.5])) with false in

utest tmGaussian with tmGaussian using eqExpr in
utest eqExpr tmGaussian
  (gaussian_ (float_ 1.0) (float_ 1.0)) with false in

utest tmBinomial with tmBinomial using eqExpr in
utest eqExpr tmBinomial (binomial_ (int_ 4) (float_ 0.5)) with false in

utest tmWiener with tmWiener using eqExpr in
utest eqExpr tmWiener tmBinomial with false in

----------------------
-- SMAP/SFOLD TESTS --
----------------------

let tmVar = var_ "x" in
let mapVar = (lam. tmVar) in
let foldToSeq = lam a. lam e. cons e a in

utest smap_Expr_Expr mapVar tmUniform with uniform_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmUniform
with [ float_ 2.0, float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmBernoulli with bern_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBernoulli
with [ float_ 0.5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmPoisson with poisson_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmPoisson
with [ float_ 0.5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmBeta with beta_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBeta
with [ float_ 2.0, float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmGamma with gamma_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmGamma
with [ float_ 2.0, float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmCategorical with categorical_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmCategorical
with [ seq_ [float_ 0.3, float_ 0.2, float_ 0.5] ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmMultinomial
with multinomial_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmMultinomial
with [ seq_ [float_ 0.3, float_ 0.2, float_ 0.5], int_ 5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmExponential with exp_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmExponential
with [ float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmEmpirical with empirical_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmEmpirical
with [
  seq_ [ utuple_ [float_ 1.0, float_ 1.5], utuple_ [float_ 3.0, float_ 1.3] ]
] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmDirichlet with dirichlet_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmDirichlet
with [ seq_ [float_ 1.3, float_ 1.3, float_ 1.5] ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmGaussian with gaussian_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmGaussian
with [ float_ 1.0, float_ 0.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmBinomial with binomial_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBinomial
with [ float_ 0.5, int_ 5] using eqSeq eqExpr in

---------------------
-- SYMBOLIZE TESTS --
---------------------

utest symbolize tmUniform with tmUniform using eqExpr in
utest symbolize tmBernoulli with tmBernoulli using eqExpr in
utest symbolize tmPoisson with tmPoisson using eqExpr in
utest symbolize tmBeta with tmBeta using eqExpr in
utest symbolize tmGamma with tmGamma using eqExpr in
utest symbolize tmCategorical with tmCategorical using eqExpr in
utest symbolize tmMultinomial with tmMultinomial using eqExpr in
utest symbolize tmExponential with tmExponential using eqExpr in
utest symbolize tmEmpirical with tmEmpirical using eqExpr in
utest symbolize tmDirichlet with tmDirichlet using eqExpr in
utest symbolize tmGaussian with tmGaussian using eqExpr in
utest symbolize tmBinomial with tmBinomial using eqExpr in


-------------------------
-- TYPE-ANNOTATE TESTS --
-------------------------

utest tyTm (typeCheck tmUniform) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmBernoulli) with tydist_ tybool_ using eqType in
utest tyTm (typeCheck tmPoisson) with tydist_ tyint_ using eqType in
utest tyTm (typeCheck tmBeta) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmGamma) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmCategorical) with tydist_ tyint_ using eqType in
utest tyTm (typeCheck tmMultinomial) with tydist_ (tyseq_ tyint_) using eqType in
utest tyTm (typeCheck tmExponential) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmEmpirical) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmDirichlet) with tydist_ (tyseq_ tyfloat_) using eqType in
utest tyTm (typeCheck tmGaussian) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmBinomial) with tydist_ tyint_ using eqType in
utest tyTm (typeCheck tmWiener) with tydist_ (tyarrow_ tyfloat_ tyfloat_)
  using eqType
in

---------------
-- ANF TESTS --
---------------

let toStr = utestDefaultToString expr2str expr2str in

let _anf = compose normalizeTerm symbolize in

utest _anf tmUniform with bind_ (ulet_ "t" tmUniform) (var_ "t") using eqExpr in
utest _anf tmBernoulli with bind_ (ulet_ "t" tmBernoulli) (var_ "t") using eqExpr in
utest _anf tmPoisson with bind_ (ulet_ "t" tmPoisson) (var_ "t") using eqExpr in
utest _anf tmBeta with bind_ (ulet_ "t" tmBeta) (var_ "t") using eqExpr in
utest _anf tmGamma with bind_ (ulet_ "t" tmGamma) (var_ "t") using eqExpr in
utest _anf tmCategorical with bindall_ [
  ulet_ "t" (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]),
  ulet_ "t1" (categorical_ (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmMultinomial with bindall_ [
  ulet_ "t" (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]),
  ulet_ "t1" (multinomial_ (int_ 5) (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmExponential with bind_ (ulet_ "t" tmExponential) (var_ "t") using eqExpr in
utest _anf tmEmpirical with bindall_ [
  ulet_ "t" (utuple_ [float_ 1.0, float_ 1.5]),
  ulet_ "t1" (utuple_ [float_ 3.0, float_ 1.3]),
  ulet_ "t2" (seq_ [(var_ "t"), (var_ "t1")]),
  ulet_ "t3" (empirical_ (var_ "t2")),
  var_ "t3"
] using eqExpr else toStr in
-- print (mexprToString (_anf tmEmpirical)); print "\n";
utest _anf tmDirichlet with bindall_ [
  ulet_ "t" (seq_ [float_ 1.3, float_ 1.3, float_ 1.5]),
  ulet_ "t1" (dirichlet_ (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmGaussian with bind_ (ulet_ "t" tmGaussian) (var_ "t") using eqExpr in
utest _anf tmBinomial with bind_ (ulet_ "t" tmBinomial) (var_ "t") using eqExpr in
utest _anf tmWiener with bind_ (ulet_ "t" tmWiener) (var_ "t") using eqExpr in

---------------------
-- TYPE-LIFT TESTS --
---------------------

utest (typeLift tmUniform).1 with tmUniform using eqExpr in
utest (typeLift tmBernoulli).1 with tmBernoulli using eqExpr in
utest (typeLift tmPoisson).1 with tmPoisson using eqExpr in
utest (typeLift tmBeta).1 with tmBeta using eqExpr in
utest (typeLift tmGamma).1 with tmGamma using eqExpr in
utest (typeLift tmCategorical).1 with tmCategorical using eqExpr in
utest (typeLift tmMultinomial).1 with tmMultinomial using eqExpr in
utest (typeLift tmExponential).1 with tmExponential using eqExpr in
utest (typeLift tmEmpirical).1 with tmEmpirical using eqExpr in
utest (typeLift tmDirichlet).1 with tmDirichlet using eqExpr in
utest (typeLift tmGaussian).1 with tmGaussian using eqExpr in
utest (typeLift tmBinomial).1 with tmBinomial using eqExpr in
utest (typeLift tmWiener).1 with tmWiener using eqExpr in

()

