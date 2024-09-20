-- NOTE(oerikss, 2024-04-25): This files wraps the dual number implementation
-- and allows us to use dead-code elimination when parsing this runtiume code.

include "./ad/dualnum.mc"

mexpr
-- Allows us to use dead-code elimination without loosing the functions we want
-- to keep. The downside is that we get an ugly printout when we test this file.
dprint (
  dualnumLtEpsilon,
  dualnumGenEpsilon,
  Primal 0.,
  dualnumCreatePrimal,
  dualnumCreateDual,
  dualnumIsDualNum,
  dualnumPrimal,
  dualnumPrimalRec,
  dualnumUnboxPrimalExn,
  dualnumPertubationExn,
  addn,
  muln,
  eqn,
  neqn,
  ltn,
  leqn,
  gtn,
  geqn,
  negn,
  subn,
  divn,
  sinn,
  cosn,
  sqrtn,
  expn,
  logn,
  pown
  )
