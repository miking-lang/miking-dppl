mexpr
let foo = lam v. mulf v v in
let v1 = 0.4 in
let v2 = 0.6 in
let x = 0 in
let v3 = foo v1 in let v4 = [v1,v3] in
let obs = [v1,v2,v3] in
let r1 = assume (Gaussian v3 v2) in
let r2 = assume (Gaussian v1 v2) in
let l = [r1,r2] in
iter (lam o.
  let i = assume (Categorical v4) in
  let mu = get l i in
  observe o (Gaussian mu v3)) obs;
let ret = get l x in ret
