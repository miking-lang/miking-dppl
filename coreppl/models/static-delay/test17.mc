--  cppl coreppl/models/static-delay/test17.mc -m is-lw --no-print-samples --extract-simplification inline --print-model --static-delay
mexpr
let a = assume (Gaussian 0. 1.) in
let b = assume (Gaussian 0. 2.) in
iter (lam obs. 
	observe obs.0 (Gaussian a 1.);
	observe obs.1 (Gaussian b 1.)) [(0.3,0.8),(0.4,0.7),(0.5,1.)]