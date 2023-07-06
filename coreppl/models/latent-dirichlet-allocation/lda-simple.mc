include "common.mc"
include "string.mc"
include "seq.mc"
include "ext/dist-ext.mc"

-- There are also other data sets available (see data-lw.mc and
-- data-c3.mc)
include "data-simple.mc"

include "lda.mc"

mexpr
let res = model () in
strJoin " " (map (lam t. float2string (get t 0)) res)
