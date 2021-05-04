include "../coreppl/coreppl.mc"

let simpleModel = use CorePPL in
  bind_
    (ulet_ "p" (app_ (var_ "sample") (app_ (app_ (var_ "beta") (int_ 10)) (int_ 20))))
    (app_ (app_ (var_ "observe") (app_ (var_ "bernoulli") (var_ "p"))) false_ )

mexpr
use CorePPL in

-- printLn (expr2str simpleModel) in
()
-- let anf = normalizeTerm (symbolize crbd) in
-- let _ = writeFile "_crbd-anf.mc" (expr2str anf) in
