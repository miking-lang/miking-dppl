-- 1. We define this `ext.mc` which contains the includes needed for log, etc. 
-- external functions
include "ext/dist-ext.mc"
include "ext/math-ext.mc"

-- 2. We parse this file to Expr
-- let externals = parseMCoreFile "src/externals/ext.mc" in

-- 3. We bind this, as well as our input, to the AST we're generating
--   let input = bind_ externals input in
-- and then in TmRecLets
--   inexpr = bind_ input invocation

-- 4. We need to get  the name for symbol that has the string "log"
-- this is where I am stuck
--   getExternalIds externals
-- will give us a Set String
-- but I am looking for a function like
--   getNameFor "log" externals
-- But I can't find that
-- There is nameGetStr, but this is from Name -> Str
-- 

