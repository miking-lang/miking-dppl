include "seq.mc"
include "data-lda-c3.mc"
mexpr
/-let numtopics = 2 in
let vocabsize = 6 in
let numdocs = 2 in
let docs = [(0,[0,1,2,4]),(1,[1,2,3,5])] in-/
let alpha:[Float] = create numtopics (lam. 1.) in
let theta = create numdocs (lam. assume (Dirichlet alpha)) in
let beta:[Float] = create vocabsize (lam. 1.) in
let phi = create numtopics (lam. assume (Dirichlet beta))  in
iter (lam doc.
    iter (lam w.
    let theta = get theta (doc.0) in
    let z = assume (Categorical theta) in
    let phi = get phi z in
    observe w (Categorical phi)
  ) doc.1
  ) docs
