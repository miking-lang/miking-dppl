

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

let empirical_ = use EmpiricalDist in
  lam lst. TmDist {dist = DEmpirical {samples = lst}, info = NoInfo ()}

-- Language compositions

lang DistAll = BernDist + BetaDist + EmpiricalDist

mexpr

use DistAll in

utest expr2str (empirical_ [(1.0, float_ 1.5), (3.0, float_ 1.3)]) with "(Empirical size=2)" in

()







