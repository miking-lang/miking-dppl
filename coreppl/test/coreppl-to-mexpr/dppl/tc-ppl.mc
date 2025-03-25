mexpr

let ok1 = infer (Importance { particles = 1 })
            (lam t : ().
              weight 0.;
              observe 1. (Uniform 1. 1.);
              assume (Uniform 1. 1.))
in

let ok2 = expectation (Uniform 1. 1.) in

()
