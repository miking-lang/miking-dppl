mexpr
let beta =
  create
    6
    (lam #var"3".
       1.)
in
type Option a
in
type These a1 b
in
type Either a2 b1
in
{}
; let numtopics =
  2
in
let vocabsize =
  6
in
let t =
  0
in
let t1 =
  0
in
let t2 =
  1
in
let t3 =
  2
in
let t4 =
  4
in
let t5 =
  [ t1,
    t2,
    t3,
    t4 ]
in
let t6 =
  (t, t5)
in
let t7 =
  1
in
let t8 =
  1
in
let t9 =
  2
in
let t10 =
  3
in
let t11 =
  5
in
let t12 =
  [ t8,
    t9,
    t10,
    t11 ]
in
let t13 =
  (t7, t12)
in
let docs =
  [ t6,
    t13 ]
in
let alpha =
  create
    2
    (lam #var"2".
       1.)
in

let t14 =
  assume
    (Dirichlet
       alpha)
in
let t15 =
  assume
    (Dirichlet
       alpha)
in
let theta =
  [ t14,
    t15 ]
in
let phi =
  create
    2
    (lam #var"1".
       let rv =
         assume
           (Dirichlet
              beta)
       in
       rv)
in
let t16 =
  iter
    (lam doc.
       let t17 =
         match
           doc
         with
           {#label"1" = #var"X1"}
         then
           #var"X1"
         else
           let t22 =
             never
           in
           t22
       in
       let t18 =
         iter
           (lam w.
              let t19 =
                match
                  doc
                with
                  (#var"X",)
                then
                  #var"X"
                else
                  let t21 =
                    never
                  in
                  t21
              in
              let theta1 =
                get
                  theta
                  t19
              in
              let z =
                assume
                  (Categorical
                     theta1)
              in
              let phi1 =
                get
                  phi
                  z
              in
              let t20 =
                observe
                  w
                  (Categorical
                     phi1)
              in
              t20)
           t17
       in
       {})
    docs
in
{}