-- Convenience functions for manually constructing ASTs

include "ast.mc"

let weight_ = use PPLCoreAst in lam arg. TmWeight {arg = arg}

let sampleExp_ = use PPLCoreAst in lam a. TmSampleExp {a = a}

let sampleBern_ = use PPLCoreAst in lam p. TmSampleBern {p = p}

let resample_ = use PPLCoreAst in TmResample {}
