mexpr
let lst = [assume (Gaussian 0. 1.), assume (Gaussian 0. 1.), assume (Gaussian 0. 1.), assume (Gaussian 0. 1.)] in
let i = assume (Categorical [0.3,0.2,0.4,0.1]) in
let a = get lst i in
observe 0.5 (Gaussian a 1.)
