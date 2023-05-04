mexpr
let a = assume (Gaussian 0. 1.) in
let b = assume (Gaussian a 1.) in
let c = assume (Gaussian a 1.) in
let d = assume (Gaussian b 1.) in
let e = assume (Gaussian b 1.) in
let f = assume (Gaussian c 1.) in
let g = assume (Gaussian c 1.) in
observe 0.5 (Gaussian d 1.);
observe 0.7 (Gaussian d 1.);
observe 0.1 (Gaussian e 1.);
observe 0.2 (Gaussian e 1.);
observe 0.5 (Gaussian f 1.);
observe 0.7 (Gaussian f 1.);
observe 0.1 (Gaussian g 1.);
observe 0.2 (Gaussian g 1.);
a


