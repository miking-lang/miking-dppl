-- Common functionality for the test suite

--------------------------
-- Common functionality --
--------------------------

-- Parse vars.mk to find the correct binary name for CorePPL

-- Compile and run CorePPL program and get back a list of weighted string samples.
-- Parameters: Command line options (e.g., backend, inference algorithm, etc.)
-- TODO sysRunCommand ["ls", path] "" "."

-------------------------------------
-- Test helpers for models/coin.mc --
-------------------------------------
-- The correct value from which the (unimodal) distribution mean
-- should not deviate significantly
let coinTrueMean = divf 12.0 23.0

-------------------------------------
-- Test helpers for models/....... --
-------------------------------------
