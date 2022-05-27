include "common.mc"

mexpr
let b1 = ref (if assume (Bernoulli 0.5) then 1 else 2) in
repeat (lam. modref b1 (addi (deref b1) (if assume (Bernoulli 0.5) then 0 else 0))) 100;
deref b1
