-- CorePPL SMC

include "coreppl.mc"

lang CorePPLSMC = CorePPL + Infer

  syn InferMethod =
  | MethodSMC { particles: Option Int }

  syn Const =
  | CResample {}

  sem delta (arg : Expr) =
  | CInfer ({method = None ()} & c) -> CInfer {c with method = Some arg}
  | CInfer {method = Some (MethodSMC r)} -> error "todo"
  -- Extract the environment from the closure and call eval
  -- perform the inference in CPS style
  -- return an empirical distribution
  -- arg is a thunk

  sem getConstStringCode (indent : Int) =
  | CResample _ -> "resample"

  sem getInferStringCode (indent : Int) =
  | MethodSMC m -> join ["smc" pprintNewline (pprintIncr indent), m.particles]

end

-- Convenience functions for manually constructing ASTs
let resample_ = use CorePPLSMC in CResample {}
let methodsmc_ = use CorePPLSMC in lam p. MethodSMC {particles = p}
