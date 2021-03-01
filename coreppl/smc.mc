-- CorePPL SMC

include "coreppl.mc"

lang CorePPLSMC = CorePPL

  syn Const =
  | CResample {}

  sem eqConst (lhs:Const) =
  | CResample {} -> match lhs with CResample _ then true else false

  sem getConstStringCode (indent : Int) =
  | CResample _ -> "resample"

end

-- Convenience functions for manually constructing ASTs
let resample_ = use CorePPLSMC in CResample {}
