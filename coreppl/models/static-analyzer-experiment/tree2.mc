mexpr
let a = assume (Gaussian 0. 1.) in
let b = assume (Gaussian a 1.) in
let c = assume (Gaussian a 1.) in
observe 0.5 (Gaussian b 1.);
observe 0.7 (Gaussian b 1.);
observe 0.1 (Gaussian c 1.);
observe 0.2 (Gaussian c 1.);
a


