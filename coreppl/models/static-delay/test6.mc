-- tree --
mexpr
let a = assume (Gaussian 0. 1.) in
let b = assume (Gaussian a 1.) in
let c = assume (Gaussian a 1.) in
observe 1.2 (Gaussian b 1.);
observe 1.8 (Gaussian b 2.);
observe 1.1 (Gaussian c 1.);
observe 2. (Gaussian c 2.)