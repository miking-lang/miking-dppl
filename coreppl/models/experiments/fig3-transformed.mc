mexpr
let v1 = 0.4 in
let v2 = 0.6 in
let x = 0 in
let t = mulf v1 v1 in
let v4 = [ v1, t ] in
let obs = [ v1, v2, t ] in
let paramI = ref [ ref (t, v2), ref (v1, v2) ] in
iter (lam o.
     let i = assume (Categorical v4) in
     let prMargP = deref (get (deref paramI) i) in
     let margMu = [...] in
     let margSigma = [...] in
     observe o (Gaussian margMu margSigma);
     let postMu = [...] in
     let postSigma = [...] in
     modref (get (deref paramI) i) (postMu, postSigma)) obs;
let prPostP = map (lam #var"2". deref #var"2") (deref paramI) in
let postParam0 = get prPostP 0 in
let postParam1 = get prPostP 1 in
let r1 = assume (Gaussian postParam0.0 postParam0.1) in
let r2 = assume (Gaussian postParam1.0 postParam1.1) in
let l = [ r1, r2 ] in
let ret = get l x in ret
