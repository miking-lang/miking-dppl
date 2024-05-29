mexpr
iter (lam obs.
	let a = assume (Gaussian 0. 1.) in
	let b = assume (Gaussian a 1.) in
	let c = assume (Gaussian a 1.) in
	observe obs.0 (Gaussian b 1.);
	observe obs.1 (Gaussian c 2.)
	) [(1.5,1.6),(1.1,2.)]