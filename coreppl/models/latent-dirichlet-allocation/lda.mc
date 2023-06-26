include "common.mc"
include "string.mc"
include "seq.mc"
include "ext/dist-ext.mc"

include "data-c3.mc"

mexpr

let alpha: [Float] = make numtopics 1. in
let beta: [Float] = make vocabsize 1. in
let phi = create numtopics (lam. assume (Dirichlet beta)) in
let theta = create numdocs (lam. assume (Dirichlet alpha)) in
repeati (lam w.
    let word = get docs w in
    let counts = assume (Multinomial word.1 (get theta (get docids w))) in
    iteri (lam z. lam e.
        weight (mulf (int2float e)
                  (bernoulliLogPmf (get (get phi z) word.0) true))
      ) counts;
    resample
  ) (length docs);

-- We only compare execution time, and therefore return unit (nothing) as the
-- result
()
