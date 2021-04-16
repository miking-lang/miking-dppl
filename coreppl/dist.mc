

include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/info.mc"
include "string.mc"
include "seq.mc"

lang Dist = PrettyPrint
  syn Expr =
  | TmDist { dist: Dist,
             info: Info}

  syn Dist =

  sem isAtomic =
  | TmDist _ -> false

  sem pprintDist (env: PprintEnv) =

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmDist r -> pprintDist env r.dist

end


lang BernDist = Dist

  syn Dist =
  | DBern { p: Expr }

  sem pprintDist (env: PprintEnv) =
  | DBern r ->
    match pprintCode 0 env r.p with (env, p) then
      (env, join ["(Bern ", p, ")"])
    else never
end



lang BetaDist = Dist

  syn Dist =
  | DBeta { a: Expr, b: Expr }

  sem pprintDist (env: PprintEnv) =
  | DBeta r ->
    match pprintCode 0 env r.a with (env, a) then
      match pprintCode 0 env r.b with (env, b) then
        (env, join ["(Beta ", a, ", ", b, ")"])
      else never
    else never
end

-- DCategorical {p=p} is equivalent to DMultinomial {n=1, p=p}
lang CategoricalDist = Dist

  syn Dist =
  | DCategorical { p: Expr }

  sem pprintDist (env: PprintEnv) =
  | DCategorical r ->
    match pprintCode 0 env r.p with (env, p) then
      (env, join ["(Categorical ", p, ")"])
    else never
end

lang MultinomialDist = Dist

  syn Dist =
  | DMultinomial { n: Expr, p:[Expr] }

  sem pprintDist (env: PprintEnv) =
  | DMultinomial r ->
    match pprintCode 0 env r.n with (env, n) then
      match pprintCode 0 env r.p with (env, p) then
        (env, join ["(Multinomial ", n, ", ", p, ")"])
      else never
    else never
end

lang ExpDist = Dist

  syn Dist =
  | DExp { rate: Expr }

  sem pprintDist (env: PprintEnv) =
  | DExp r ->
    match pprintCode 0 env r.rate with (env, rate) then
      (env, join ["(Exp ", rate, ")"])
    else never
end

lang EmpiricalDist = Dist

  syn Dist =
  | DEmpirical { samples: [(Float, Expr)] }

  sem pprintDist (env: PprintEnv) =
  | DEmpirical r ->
      (env, join ["(Empirical size=", int2string (length r.samples), ")"])

end

-- Convenience functions for manually constructing ASTs

let bern_ = use BernDist in
  lam p. TmDist {dist = DBern {p = p}, info = NoInfo ()}

let beta_ = use BetaDist in
  lam a. lam b. TmDist {dist = DBeta {a = a, b = b}, info = NoInfo ()}

let categorical_ = use CategoricalDist in
  lam p. TmDist {dist = DCategorical {p = p}, info = NoInfo ()}

let multinomial_ = use MultinomialDist in
  lam n. lam p. TmDist {dist = DMultinomial {n = n, p = p}, info = NoInfo ()}

let exp_ = use ExpDist in
  lam rate. TmDist { dist = DExp {rate = rate}, info = NoInfo () }

let empirical_ = use EmpiricalDist in
  lam lst. TmDist {dist = DEmpirical {samples = lst}, info = NoInfo ()}

-- Language compositions

lang DistAll =
  BernDist + BetaDist + ExpDist + EmpiricalDist + CategoricalDist +
  MultinomialDist + PrettyPrint

lang Test = DistAll + MExprAst + MExprPrettyPrint

mexpr

use Test in

utest expr2str (empirical_ [(1.0, float_ 1.5), (3.0, float_ 1.3)]) with "(Empirical size=2)" in
utest expr2str (categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]))
  with "(Categorical [ 3.0e-1,\n  2.0e-1,\n  5.0e-1 ])" in
utest expr2str (multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]))
  with "(Multinomial 5, [ 3.0e-1,\n  2.0e-1,\n  5.0e-1 ])" in
utest expr2str (exp_ (float_ 1.0)) with "(Exp 1.0e-0)" in

()

