include "../coreppl/coreppl.mc"

let bernoullimodel = use MExprPPL in
  bindall_
  [ ulet_ "n" (int_ 10)
  , ulet_ "theta" (float_ 0.5)
  , ureclet_ "assume_rec"
              (ulam_ "n" (ulam_ "samples"
              (if_ (eqi_ (var_ "n") (int_ 0)) (var_ "samples") (app_ (app_ (var_ "assume_rec") (subi_ (var_ "n") (int_ 1))) (cons_ (assume_ (bern_ (var_ "theta"))) (var_ "samples"))))
              ))
  , app_ (app_ (var_ "assume_rec") (var_ "n")) (var_ "theta")
  ]
mexpr
use MExprPPL in

-- printLn (expr2str bernoullimodel);
utest expr2str bernoullimodel with () using (lam. lam. true) in

()
