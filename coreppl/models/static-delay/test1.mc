--cppl coreppl/models/static-delay/test1.mc -m is-lw --no-print-samples --extract-simplification inline --print-model --static-delay
mexpr
let a = assume (Gaussian 0. 1.) in
let b = assume (Gaussian a 1.) in
observe 0.5 (Gaussian b 1.);
()