

include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/info.mc"
include "mexpr/eq.mc"
include "mexpr/type-annot.mc"
include "mexpr/anf.mc"
include "mexpr/type-lift.mc"

include "string.mc"
include "seq.mc"

lang Dist = PrettyPrint + Eq + Sym + TypeAnnot + ANF + TypeLift
  syn Expr =
  | TmDist { dist: Dist,
             ty: Type,
             info: Info }

  syn Type =
  | TyDist {info : Info,
            ty   : Type}

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

  sem smapDist_Expr_Expr (f: Expr -> a) =
  -- Intentionally left blank

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  -- Intentionally left blank

  sem smap_Expr_Expr (f: Expr -> a) =
  | TmDist t -> TmDist { t with dist = smapDist_Expr_Expr f t.dist }

  sem sfold_Expr_Expr (f: a -> b -> a) (acc: a) =
  | TmDist t -> sfoldDist_Expr_Expr f acc t.dist

  sem infoTy =
  | TyDist t -> t.info

  sem tyWithInfo (info : Info) =
  | TyDist t -> TyDist {t with info = info}

  sem smap_Type_Type (f: Type -> a) =
  | TyDist t -> TyDist { t with ty = f t.ty }

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
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  -- Intentionally left blank

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDist r ->
    match lhs with TmDist l then eqExprHDist env free l.dist r.dist else None ()


  sem eqTypeH (typeEnv : EqTypeEnv) (free : EqTypeFreeEnv) (lhs : Type) =
  | TyDist r ->
    match unwrapType typeEnv lhs with Some (TyDist l) then
      eqTypeH typeEnv free l.ty r.ty
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  -- Intentionally left blank

  sem symbolizeExpr (env: SymEnv) =
  | TmDist t ->
    TmDist {{ t with dist = symbolizeDist env t.dist }
                with ty = symbolizeType env t.ty }

  -- Type annotate
  sem compatibleTypeBase (tyEnv: TypeEnv) =
  | (TyDist t1, TyDist t2) ->
    match compatibleType tyEnv t1.ty t2.ty with Some t then
      Some (TyDist {t1 with ty = t})
    else None ()

  sem tyDist (env: TypeEnv) (info: Info) =
  -- Intentionally left blank

  sem typeAnnotDist (env: TypeEnv) =
  | dist -> smapDist_Expr_Expr (typeAnnotExpr env) dist

  sem typeAnnotExpr (env: TypeEnv) =
  | TmDist t ->
    let dist = typeAnnotDist env t.dist in
    let ty = TyDist { info = t.info, ty = tyDist env t.info dist } in
    TmDist {{ t with dist = dist }
                with ty = ty }

  -- ANF
  sem isValueDist =
  -- Intentionally left blank

  sem normalizeDist (k : Dist -> Expr) =
  -- Intentionally left blank

  sem isValue =
  | TmDist t -> isValueDist t.dist

  sem normalize (k : Expr -> Expr) =
  | TmDist ({ dist = dist } & t) ->
    normalizeDist (lam dist. k (TmDist { t with dist = dist })) dist

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

end


lang UniformDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DUniform { a: Expr, b: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DUniform t -> DUniform {{ t with a = f t.a }
                                with b = f t.b }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DUniform t -> f (f acc t.a) t.b

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DUniform t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a, t.b] with (env,args) then
      (env, join ["Uniform", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
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

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DUniform t ->
    let err = lam. infoErrorExit info "Type error uniform" in
    match tyTm t.a with TyFloat _ then
      match tyTm t.b with TyFloat _ then
        TyFloat { info = info }
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DUniform _ -> false

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

end





lang BernoulliDist = Dist + PrettyPrint + Eq + Sym + BoolTypeAst + FloatTypeAst

  syn Dist =
  | DBernoulli { p: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DBernoulli t -> DBernoulli { t with p = f t.p }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DBernoulli t -> f acc t.p

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBernoulli t ->
    let i = pprintIncr indent in
    match printParen i env t.p with (env,p) then
      (env, join ["Bernoulli", pprintNewline i, p])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DBernoulli r ->
    match lhs with DBernoulli l then eqExprH env free l.p r.p else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DBernoulli t -> DBernoulli { t with p = symbolizeExpr env t.p }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DBernoulli t ->
    match tyTm t.p with TyFloat _ then TyBool { info = info }
    else infoErrorExit info "Type error bern"

  -- ANF
  sem isValueDist =
  | DBernoulli _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DBernoulli ({ p = p } & t) ->
    normalizeName (lam p. k (DBernoulli { t with p = p })) p

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DBernoulli ({ p = p } & t) ->
    match typeLiftExpr env p with (env, p) then
      (env, DBernoulli {t with p = p})
    else never

end



lang PoissonDist = Dist + PrettyPrint + Eq + Sym + IntTypeAst + FloatTypeAst

  syn Dist =
  | DPoisson { lambda: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DPoisson t -> DPoisson { t with lambda = f t.lambda }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DPoisson t -> f acc t.lambda

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DPoisson t ->
    let i = pprintIncr indent in
    match printParen i env t.lambda with (env,lambda) then
      (env, join ["Poisson", pprintNewline i, lambda])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DPoisson r ->
    match lhs with DPoisson l then eqExprH env free l.lambda r.lambda else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DPoisson t -> DPoisson { t with lambda = symbolizeExpr env t.lambda }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DPoisson t ->
    match tyTm t.lambda with TyFloat _ then TyInt { info = info }
    else infoErrorExit info "Type error Poisson"

  -- ANF
  sem isValueDist =
  | DPoisson _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DPoisson ({ lambda = lambda } & t) ->
    normalizeName (lam lambda. k (DPoisson { t with lambda = lambda })) lambda

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DPoisson ({ lambda = lambda } & t) ->
    match typeLiftExpr env lambda with (env, lambda) then
      (env, DPoisson {t with lambda = lambda})
    else never

end




lang BetaDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DBeta { a: Expr, b: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DBeta t -> DBeta {{ t with a = f t.a }
                          with b = f t.b }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DBeta t -> f (f acc t.a) t.b

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBeta t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a, t.b] with (env,args) then
      (env, join ["Beta", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
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

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DBeta t ->
    let err = lam. infoErrorExit info "Type error beta" in
    match tyTm t.a with TyFloat _ then
      match tyTm t.b with TyFloat _ then
        TyFloat { info = info }
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DBeta _ -> false

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

end




lang GammaDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DGamma { k: Expr, theta: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DGamma t -> DGamma {{ t with k = f t.k }
                            with theta = f t.theta }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DGamma t -> f (f acc t.k) t.theta

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DGamma t ->
    let i = pprintIncr indent in
    match printArgs i env [t.k, t.theta] with (env,args) then
      (env, join ["Gamma", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
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

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DGamma t ->
    let err = lam. infoErrorExit info "Type error Gamma" in
    match tyTm t.k with TyFloat _ then
      match tyTm t.theta with TyFloat _ then
        TyFloat { info = info }
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DGamma _ -> false

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

end




-- DCategorical {p=p} is equivalent to DMultinomial {n=1, p=p}
lang CategoricalDist =
  Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- p has type [Float]: the list of probabilities
  | DCategorical { p: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DCategorical t -> DCategorical { t with p = f t.p }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DCategorical t -> f acc t.p

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DCategorical t ->
    let i = pprintIncr indent in
    match printArgs i env [t.p] with (env,p) then
      (env, join ["Categorical", pprintNewline i, p])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DCategorical { p = p2 } ->
    match lhs with DCategorical { p = p1 } then
      eqExprH env free p1 p2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DCategorical t ->
    DCategorical { t with p = symbolizeExpr env t.p }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DCategorical t ->
    match tyTm t.p with TySeq { ty = TyFloat _ } then TyInt { info = info }
    else infoErrorExit info "Type error categorical"

  -- ANF
  sem isValueDist =
  | DCategorical _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DCategorical ({ p = p } & t) ->
    normalizeName (lam p. k (DCategorical {t with p = p})) p

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DCategorical ({ p = p } & t) ->
    match typeLiftExpr env p with (env, p) then
      (env, DCategorical {t with p = p})
    else never

end

lang MultinomialDist =
  Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- n has type Int: the number of trials
  -- p has type [Float]: the list of probabilities
  | DMultinomial { n: Expr, p: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DMultinomial t -> DMultinomial {{ t with n = f t.n }
                                        with p = f t.p }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DMultinomial t -> f (f acc t.n) t.p

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DMultinomial t ->
    let i = pprintIncr indent in
    match printArgs i env [t.n, t.p] with (env,args) then
      (env, join ["Multinomial", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
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

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DMultinomial t ->
    let err = lam. infoErrorExit info "Type error multinomial" in
    match tyTm t.n with TyInt _ then
      match tyTm t.p with TySeq { ty = TyFloat _ } then
        TySeq { ty = TyInt { info = info }, info = info }
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DMultinomial _ -> false

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

end

lang DirichletDist = Dist + PrettyPrint + Eq + Sym + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- a has type [Float]: the list of concentration parameters
  | DDirichlet { a: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DDirichlet t -> DDirichlet { t with a = f t.a }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DDirichlet t -> f acc t.a

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DDirichlet t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a] with (env,a) then
      (env, join ["Dirichlet", pprintNewline i, a])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DDirichlet { a = a2 } ->
    match lhs with DDirichlet { a = a1 } then
      eqExprH env free a1 a2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DDirichlet t ->
    DDirichlet { t with a = symbolizeExpr env t.a }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DDirichlet t ->
    match tyTm t.a with TySeq { ty = TyFloat _ } then
      TySeq { info = info, ty = TyFloat { info = info } }
    else infoErrorExit info "Type error dirichlet"

  -- ANF
  sem isValueDist =
  | DDirichlet _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DDirichlet ({ a = a } & t) ->
    normalizeName (lam a. k (DDirichlet { t with a = a })) a

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DDirichlet ({ a = a } & t) ->
    match typeLiftExpr env a with (env, a) then
      (env, DDirichlet {t with a = a})
    else never

end

lang ExponentialDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DExponential { rate: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DExponential t -> DExponential { t with rate = f t.rate }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DExponential t -> f acc t.rate

  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DExponential t ->
    let i = pprintIncr indent in
    match printParen i env t.rate with (env,rate) then
      (env, join ["Exponential", pprintNewline i, rate])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DExponential r ->
    match lhs with DExponential l then eqExprH env free l.rate r.rate else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DExponential t -> DExponential { t with rate = symbolizeExpr env t.rate }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DExponential t ->
    match tyTm t.rate with TyFloat _ then TyFloat { info = info }
    else infoErrorExit info "Type error exponential"

  -- ANF
  sem isValueDist =
  | DExponential _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DExponential ({ rate = rate } & t) ->
    normalizeName (lam rate. k (DExponential { t with rate = rate })) rate

  -- Type lift
  sem typeLiftDist (env : TypeLiftEnv) =
  | DExponential ({ rate = rate } & t) ->
    match typeLiftExpr env rate with (env, rate) then
      (env, DExponential {t with rate = rate})
    else never

end

lang EmpiricalDist =
  Dist + PrettyPrint + Eq + Sym + FloatTypeAst + RecordTypeAst + SeqTypeAst

  syn Dist =
  -- samples has type [(Float,a)]: A set of weighted samples over type a
  | DEmpirical { samples: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DEmpirical t -> DEmpirical { t with samples = f t.samples }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DEmpirical t -> f acc t.samples

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DEmpirical t ->
    let i = pprintIncr indent in
    match printParen i env t.samples with (env,samples) then
      (env, join ["Empirical", pprintNewline i, samples])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DEmpirical { samples = s2 } ->
    match lhs with DEmpirical { samples = s1 } then
      eqExprH env free s1 s2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DEmpirical t ->
    DEmpirical { t with samples = symbolizeExpr env t.samples }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DEmpirical t ->
    let err = lam. infoErrorExit info "Type error empirical" in
    match tyTm t.samples
    with TySeq { ty = TyRecord { fields = fields } } then
      if eqi (mapSize fields) 2 then
        match mapLookup (stringToSid "0") fields with Some TyFloat _ then
          match mapLookup (stringToSid "1") fields with Some ty then
            ty
          else err ()
        else err ()
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DEmpirical _ -> false

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

end

lang GaussianDist =
  Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DGaussian { mu: Expr, sigma: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DGaussian t -> DGaussian {{ t with mu = f t.mu }
                                  with sigma = f t.sigma }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DGaussian t -> f (f acc t.mu) t.sigma

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DGaussian t ->
    let i = pprintIncr indent in
    match printArgs i env [t.mu, t.sigma] with (env,args) then
      (env, join ["Gaussian", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
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

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DGaussian t ->
    let err = lam. infoErrorExit info "Type error Gaussian" in
    match tyTm t.mu with TyFloat _ then
      match tyTm t.sigma with TyFloat _ then
        TyFloat { info = info }
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DGaussian _ -> false

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

end

lang BinomialDist = Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + BoolTypeAst + FloatTypeAst

  syn Dist =
  | DBinomial { n: Expr, p: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DBinomial t -> DBinomial { { t with n = f t.n } with p = f t.p }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DBinomial t -> f (f acc t.n) t.p

   -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBinomial t ->
    let i = pprintIncr indent in
    match printArgs i env [t.n, t.p] with (env,args) then
      (env, join ["Binomial", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
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

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DBinomial t ->
    let err = lam. infoErrorExit info "Type error Binomial" in
    match tyTm t.n with TyInt _ then
      match tyTm t.p with TyFloat _ then
        TyInt { info = info }
      else dprint t.p; err ()
    else dprint t.n; err ()

  -- ANF
  sem isValueDist =
  | DBinomial _ -> false

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

end

-----------------
-- AST BUILDER --
-----------------

let dist_ = use Dist in
  lam d. TmDist {dist = d, ty = tyunknown_, info = NoInfo ()}

let tydist_ = use Dist in
  lam ty. TyDist {info = NoInfo (), ty = ty}

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

---------------------------
-- LANGUAGE COMPOSITIONS --
---------------------------

lang DistAll =
  UniformDist + BernoulliDist + PoissonDist + BetaDist + GammaDist +
  CategoricalDist + MultinomialDist + DirichletDist +  ExponentialDist +
  EmpiricalDist + GaussianDist + BinomialDist

lang Test =
  DistAll + MExprAst + MExprPrettyPrint + MExprEq + MExprSym + MExprTypeAnnot
  + MExprANF + MExprTypeLiftUnOrderedRecords

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

------------------------
-- PRETTY-PRINT TESTS --
------------------------

utest expr2str tmUniform with strJoin "\n" [
  "Uniform",
  "  1.",
  "  2."
] using eqString in

utest expr2str tmBernoulli with strJoin "\n" [
  "Bernoulli",
  "  0.5"
] using eqString in

utest expr2str tmPoisson with strJoin "\n" [
  "Poisson",
  "  0.5"
] using eqString in

utest expr2str tmBeta with strJoin "\n" [
  "Beta",
  "  1.",
  "  2."
] using eqString in

utest expr2str tmGamma with strJoin "\n" [
  "Gamma",
  "  1.",
  "  2."
] using eqString in

utest expr2str tmCategorical with strJoin "\n" [
  "Categorical",
  "  [ 0.3,",
  "    0.2,",
  "    0.5 ]"
] using eqString in

utest expr2str tmMultinomial with strJoin "\n" [
  "Multinomial",
  "  5",
  "  [ 0.3,",
  "    0.2,",
  "    0.5 ]"
] using eqString in

utest expr2str tmExponential with strJoin "\n" [
  "Exponential",
  "  1."
] using eqString in

utest expr2str tmEmpirical with strJoin "\n" [
  "Empirical",
  "  [ (1., 1.5),",
  "    (3., 1.3) ]"
] using eqString in

utest expr2str tmDirichlet with strJoin "\n" [
  "Dirichlet",
  "  [ 1.3,",
  "    1.3,",
  "    1.5 ]"
] using eqString in

utest expr2str tmGaussian with strJoin "\n" [
  "Gaussian",
  "  0.",
  "  1."
] using eqString in

utest expr2str tmBinomial with strJoin "\n" [
  "Binomial",
  "  5",
  "  0.5"
] using eqString in

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

utest tyTm (typeAnnot tmUniform) with tydist_ tyfloat_ using eqType in
utest tyTm (typeAnnot tmBernoulli) with tydist_ tybool_ using eqType in
utest tyTm (typeAnnot tmPoisson) with tydist_ tyint_ using eqType in
utest tyTm (typeAnnot tmBeta) with tydist_ tyfloat_ using eqType in
utest tyTm (typeAnnot tmGamma) with tydist_ tyfloat_ using eqType in
utest tyTm (typeAnnot tmCategorical) with tydist_ tyint_ using eqType in
utest tyTm (typeAnnot tmMultinomial) with tydist_ (tyseq_ tyint_) using eqType in
utest tyTm (typeAnnot tmExponential) with tydist_ tyfloat_ using eqType in
utest tyTm (typeAnnot tmEmpirical) with tydist_ tyfloat_ using eqType in
utest tyTm (typeAnnot tmDirichlet) with tydist_ (tyseq_ tyfloat_) using eqType in
utest tyTm (typeAnnot tmGaussian) with tydist_ tyfloat_ using eqType in
utest tyTm (typeAnnot tmBinomial) with tydist_ tyint_ using eqType in

---------------
-- ANF TESTS --
---------------

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
  ulet_ "t" (utuple_ [float_ 3.0, float_ 1.3]),
  ulet_ "t1" (utuple_ [float_ 1.0, float_ 1.5]),
  ulet_ "t2" (seq_ [(var_ "t1"), (var_ "t")]),
  ulet_ "t3" (empirical_ (var_ "t2")),
  var_ "t3"
] using eqExpr in
-- print (expr2str (_anf tmEmpirical)); print "\n";
utest _anf tmDirichlet with bindall_ [
  ulet_ "t" (seq_ [float_ 1.3, float_ 1.3, float_ 1.5]),
  ulet_ "t1" (dirichlet_ (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmGaussian with bind_ (ulet_ "t" tmGaussian) (var_ "t") using eqExpr in
utest _anf tmBinomial with bind_ (ulet_ "t" tmBinomial) (var_ "t") using eqExpr in

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

()

