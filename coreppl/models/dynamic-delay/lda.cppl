include "seq.mc"
include "data/data-lda-simple.mc"
mexpr
let alpha:[Float] = create numtopics (lam. 1.) in
let beta:[Float] = create vocabsize (lam. 1.) in
let theta = create numdocs (lam. delay (Dirichlet alpha)) in
let phi = create numtopics (lam. delay (Dirichlet beta))  in
iter (lam doc.
    let theta= get theta (doc.0) in
    iter (lam w.
      let theta= delayed theta in
      let z = assume (Categorical theta) in
      let phi = get phi z in
      let phi = delayed phi in
      observe w (Categorical phi)
      ) doc.1) docs
