mexpr
let b2 = true in
let t = 2. in
let t1 = 2. in
let margP = divf t (addf t t1) in
let #var"1" = observe
    b2 (Bernoulli
       margP) in
let postA =
  match
    b2
  with
    true
  then
    addf t 1.
  else
    t
in
let postB =
  match
    b2
  with
    true
  then
    t1
  else
    addf t1 1.
in
let a3 = assume
    (Beta
       postA postB) in
a3
