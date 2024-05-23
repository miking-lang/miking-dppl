include "ext/math-ext.mc"
mexpr

let t =
  1.
in
let t1 =
  2.
in
let t2 =
  3.
in
let t3 =
  [ t,
    t1,
    t2 ]
in
 let t4 =
  0.
in
let t5 =
  1.
in
let pInitParam:((Float,Float),) =
  ((t4, t5),)
in
let t6 =
  foldl
    (lam outerParam:((Float,Float),).
       lam sigma:Float.
         let t7 =
           0.3
         in
         let t8 =
           0.4
         in
         let t9 =
           0.5
         in
         let t10 =
           [ t7,
             t8,
             t9 ]
         in
         let pInitParam1:((Float,Float),) =
           (outerParam.0,)
         in
         let t11 =
           foldl
             (lam p:((Float,Float),).
                lam obs:Float.
                  let t12 =
                    1.
                  in
                  let pMargParam:(Float,Float) =
                    p.0
                  in
                  let postMu =
                    mulf
                      (addf
                         (divf
                            pMargParam.0
                            (mulf
                               pMargParam.1
                               pMargParam.1))
                         (divf
                            obs
                            (mulf
                               t12
                               t12)))
                      (divf
                         1.
                         (addf
                            (divf
                               1.
                               (mulf
                                  pMargParam.1
                                  pMargParam.1))
                            (divf
                               1.
                               (mulf
                                  t12
                                  t12))))
                  in
                  let postSigma =
                    externalSqrt
                      (divf
                         1.
                         (addf
                            (divf
                               1.
                               (mulf
                                  pMargParam.1
                                  pMargParam.1))
                            (divf
                               1.
                               (mulf
                                  t12
                                  t12))))
                  in
                  let margMu =
                    mulf
                      (mulf
                         pMargParam.1
                         pMargParam.1)
                      (divf
                         pMargParam.0
                         (mulf
                            pMargParam.1
                            pMargParam.1))
                  in
                  let margSigma =
                    externalSqrt
                      (addf
                         (mulf
                            pMargParam.1
                            pMargParam.1)
                         (mulf
                            t12
                            t12))
                  in
                  let rP:(Float,Float) =
                    (postMu, postSigma)
                  in
                  let t13 =
                    observe
                      obs
                      (Gaussian
                         margMu
                         margSigma)
                  in
                  let r1:((Float,Float),) =
                    (rP,)
                  in
                  r1)
             pInitParam1
             t10
         in
         let r:((Float,Float),) =
           (t11,)
         in
         r)
    pInitParam
    t3
in
let postParam =
  t6.0
in
let a3 =
  assume
    (Gaussian
       postParam.0
       postParam.1)
in
{}