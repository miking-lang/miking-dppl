
include "seq.mc"
mexpr
let y = [0.5,0.4,0.2] in
let sigma = 1. in
let mu = assume (Gaussian 0. 5.) in
let tau = assume (Beta 5. 5.) in
iter (lam y.
	let x = assume (Gaussian mu tau) in
	observe y (Gaussian x sigma)
	) y
