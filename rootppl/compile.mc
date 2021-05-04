-- CorePPL compiler, targeting the RootPPL framework
-- TODO(dlunde,2021-05-04): Out of date

include "../coreppl/coreppl.mc"

include "../models/crbd.mc"

mexpr
use CorePPL in

-- let t0 = wallTimeMs () in
-- let r1 = symbolize crbd in
-- let t1 = wallTimeMs () in
-- let r2 = normalizeTerm r1 in
-- let t2 = wallTimeMs () in
-- let r3 = expr2str r2 in
-- let t3 = wallTimeMs () in

-- let _ = print "\nSymbolize time: " in
-- let _ = printLn (float2string (subf t1 t0)) in
-- let _ = print "ANF time: " in
-- let _ = printLn (float2string (subf t2 t1)) in
-- let _ = print "expr2str time: " in
-- let _ = printLn (float2string (subf t3 t2)) in
-- expr2str is really slow (3.7 seconds)

-- writeFile "_crbd-init.mc" (expr2str crbd);
-- let anf = normalizeTerm (symbolize crbd) in
-- writeFile "_crbd-anf.mc" (expr2str anf);

()
