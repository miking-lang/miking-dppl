--cppl coreppl/models/static-delay/test10.mc -m is-lw --no-print-samples --extract-simplification inline --print-model --static-delay
mexpr
iter (lam obs.
	let a = assume (Gaussian 0. 1.) in
	let b = assume (Gaussian a 1.) in 
	observe 0.4 (Gaussian b 1.)) [0.5,0.6,0.8,0.66]