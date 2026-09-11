mexpr
let v1 = 0.4 in
let v2 = 0.6 in
let x = 0 in
let t = mulf v1 v1 in
let v4 = [ v1,
    t ] in
let obs = [ v1,
    v2,
    t ] in
let paramI = ref [ ref (t, v2),
      ref (v1, v2) ] in
let #var"1" =
  iter
    (lam o.
       let i = assume
           (Categorical
              v4) in
       let prMargP = deref (get (deref paramI) i) in
       let margMu =
         mulf
           (mulf
              (mulf
                 prMargP.1
                 (match
                    geqf 1. 0.
                  with
                    true
                  then
                    1.
                  else
                    negf 1.))
              (mulf
                 prMargP.1
                 (match
                    geqf 1. 0.
                  with
                    true
                  then
                    1.
                  else
                    negf 1.)))
           (divf
              (addf (mulf 1. prMargP.0) 0.)
              (mulf
                 (mulf
                    prMargP.1
                    (match
                       geqf 1. 0.
                     with
                       true
                     then
                       1.
                     else
                       negf 1.))
                 (mulf
                    prMargP.1
                    (match
                       geqf 1. 0.
                     with
                       true
                     then
                       1.
                     else
                       negf 1.))))
       in
       let margSigma =
         externalSqrt
           (addf
              (mulf
                 (mulf
                    prMargP.1
                    (match
                       geqf 1. 0.
                     with
                       true
                     then
                       1.
                     else
                       negf 1.))
                 (mulf
                    prMargP.1
                    (match
                       geqf 1. 0.
                     with
                       true
                     then
                       1.
                     else
                       negf 1.)))
              (mulf t t))
       in
       let t1 =
         observe
           o (Gaussian
              margMu margSigma)
       in
       let postMu =
         divf
           (subf
              (mulf
                 (addf
                    (divf
                       (addf (mulf 1. prMargP.0) 0.)
                       (mulf
                          (mulf
                             prMargP.1
                             (match
                                gtf 1. 0.
                              with
                                true
                              then
                                1.
                              else
                                negf 1.))
                          (mulf
                             prMargP.1
                             (match
                                gtf 1. 0.
                              with
                                true
                              then
                                1.
                              else
                                negf 1.))))
                    (divf o (mulf t t)))
                 (divf
                    1.
                    (addf
                       (divf
                          1.
                          (mulf
                             (mulf
                                prMargP.1
                                (match
                                   gtf 1. 0.
                                 with
                                   true
                                 then
                                   1.
                                 else
                                   negf 1.))
                             (mulf
                                prMargP.1
                                (match
                                   gtf 1. 0.
                                 with
                                   true
                                 then
                                   1.
                                 else
                                   negf 1.))))
                       (divf 1. (mulf t t)))))
              0.)
           1.
       in
       let postSigma =
         divf
           (externalSqrt
              (divf
                 1.
                 (addf
                    (divf
                       1.
                       (mulf
                          (mulf
                             prMargP.1
                             (match
                                gtf 1. 0.
                              with
                                true
                              then
                                1.
                              else
                                negf 1.))
                          (mulf
                             prMargP.1
                             (match
                                gtf 1. 0.
                              with
                                true
                              then
                                1.
                              else
                                negf 1.))))
                    (divf 1. (mulf t t)))))
           (match
              gtf 1. 0.
            with
              true
            then
              1.
            else
              negf 1.)
       in
       let #var"3" = modref (get (deref paramI) i) (postMu, postSigma)
       in
       {})
    obs
in
let prPostP = map (lam #var"2".
       deref #var"2") (deref paramI)
in
let postParam0 = get prPostP 0 in
let postParam1 = get prPostP 1 in
let r1 = assume
    (Gaussian
       postParam0.0 postParam0.1)
in
let r2 = assume
    (Gaussian
       postParam1.0 postParam1.1)
in
let l = [ r1,
    r2 ] in
let ret = get l x in
ret
