mexpr
let pickpair = lam n.
  let p = make (subi n 1) (divf 1. (int2float (subi n 1))) in
  let i = assume (Categorical p) in
  let i = addi i 2 in
  let p = make (subi i 1) (divf 1. (int2float (subi i 1))) in
  let j = assume (Categorical p) in
  (subi i 1,j) in
  pickpair 4
