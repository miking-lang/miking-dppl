mexpr
let a = assume (Gaussian 0. 1.) in
let b = assume (Gaussian a 1.) in
let c = assume (Gaussian a 1.) in
let d = assume (Gaussian b 1.) in
let e = assume (Gaussian b 1.) in
let f = assume (Gaussian c 1.) in
let g = assume (Gaussian c 1.) in
let h = assume (Gaussian d 1.) in
let i = assume (Gaussian d 1.) in
let j = assume (Gaussian e 1.) in
let k = assume (Gaussian e 1.) in
let l = assume (Gaussian f 1.) in
let m = assume (Gaussian f 1.) in
let n = assume (Gaussian g 1.) in
let o = assume (Gaussian g 1.) in
observe 0.5 (Gaussian h 1.);
observe 0.7 (Gaussian h 1.);
observe 0.1 (Gaussian i 1.);
observe 0.2 (Gaussian i 1.);
observe 0.5 (Gaussian j 1.);
observe 0.7 (Gaussian j 1.);
observe 0.1 (Gaussian k 1.);
observe 0.2 (Gaussian k 1.);
observe 0.5 (Gaussian l 1.);
observe 0.7 (Gaussian l 1.);
observe 0.1 (Gaussian m 1.);
observe 0.2 (Gaussian m 1.);
observe 0.5 (Gaussian n 1.);
observe 0.7 (Gaussian n 1.);
observe 0.1 (Gaussian o 1.);
observe 0.2 (Gaussian o 1.);
a


