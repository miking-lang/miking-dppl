include "ode-solver.mc"

mexpr
-- Allows us to use dead-code elimination without loosing the functions we want
-- to keep. The downside is that we get an ugly printout when we test this file.
dprint (
  odeSolverRK4Solve,
  odeSolverEFSolve
  )
