-- CorePPL SMC

/-
include "mexpr/ast-builder.mc"

lang CorePPLSMC = Infer + Ast

  syn InferMethod =
  | MethodSMC { particles: Expr }

  syn Const =
  | CResample {}


  -- Lower to an MExpr expression
  sem toMExpr =
  | TmAssume r -> unit_ -- TODO
  | TmObserve r -> unit_ -- TODO
  | TmWeight r -> unit_ -- TODO


  sem getConstStringCode (indent : Int) =
  | CResample _ -> "resample"

  sem getInferStringCode (indent : Int) =
  | MethodSMC m -> join ["smc" pprintNewline (pprintIncr indent), m.particles]

end

-- Convenience functions for manually constructing ASTs
let resample_ = use CorePPLSMC in CResample {}
let methodsmc_ = use CorePPLSMC in lam p. MethodSMC {particles = p}
-/
