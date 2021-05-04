-- CorePPL compiler, targeting the RootPPL framework
-- TODO(dlunde,2021-05-04): Out of date

include "../coreppl/coreppl.mc"

include "../models/crbd.mc"

mexpr
use MExprPPL in

let compile: Expr -> CProg = lam prog.

  -- Symbolize with empty environment
  let prog = symbolizeExpr symEnvEmpty prog in

  -- Type annotate
  -- let prog = typeAnnot prog in

  -- ANF transformation
  -- let prog = normalizeTerm prog in

  -- Type lift
  -- match typeLift prog with (env, prog) then

  --   -- Run C compiler
  --   let cprog = compileWithMain env prog in

  --   cprog

  -- else never

  prog

in

-- print (expr2str (compile crbd));

()
