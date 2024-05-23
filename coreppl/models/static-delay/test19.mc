--  cppl coreppl/models/static-delay/test19.mc -m is-lw --no-print-samples --extract-simplification inline --print-model --static-delay
mexpr
	let a = assume (Gaussian 0. 1.) in
iter (lam sigma.
	iter (lam obs. 
		observe obs (Gaussian a 1.)) [0.3,0.4,0.5]) [1.,2.,3.]