-- Convenience functions for manually constructing ASTs

include "ast.mc"
include "mexpr/ast-builder.mc"

let weight_ = use PPLCore in lam arg. TmWeight {arg = arg}

let sampleExp_ = use PPLCore in lam a. TmSampleExp {a = a}

let sampleBern_ = use PPLCore in lam p. TmSampleBern {p = p}

let resample_ = use PPLCore in TmResample {}
