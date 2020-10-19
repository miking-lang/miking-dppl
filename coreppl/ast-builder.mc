-- Convenience functions for manually constructing ASTs

include "ast.mc"
include "mexpr/ast-builder.mc"

let weight_ = use CorePPL in lam arg. TmWeight {arg = arg}

let sampleExp_ = use CorePPL in lam a. TmSampleExp {a = a}

let sampleBern_ = use CorePPL in lam p. TmSampleBern {p = p}

let resample_ = use CorePPL in TmResample {}
