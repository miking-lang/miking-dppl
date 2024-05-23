--cppl coreppl/models/static-delay/test20.mc -m is-lw --no-print-samples --extract-simplification inline --print-model --static-delay
/-
mexpr
let numtopics = 2 in
let vocabsize = 6 in
let numdocs = 2 in
let docs = [(0,[0,1,2,4]),(1,[1,2,3,5])] in
let alpha:[Float] = create numtopics (lam. 1.) in
let theta = [assume (Dirichlet alpha), assume (Dirichlet alpha)] in
let beta:[Float] = create vocabsize (lam. 1.) in
let phi = [assume (Dirichlet beta), assume (Dirichlet beta)]  in
iter (lam doc.
    iter (lam w.
    let theta = get theta (doc.0) in
    let z = assume (Categorical theta) in
    let phi = get phi z in
    observe w (Categorical phi)
  ) doc.1
  ) docs
-/

mexpr
let numtopics = 2 in
let vocabsize = 6 in
let numdocs = 2 in
let docs = [(0,[0,1,2,4]),(1,[1,2,3,5])] in
let alpha:[Float] = create numtopics (lam. 1.) in
let theta = create 2 (lam. assume (Dirichlet alpha)) in
iter (lam doc.
    iter (lam w.
    let theta = get theta (doc.0) in
    observe 0 (Categorical theta)
  ) doc.1
  ) docs