let sapply = lam x. lam f.
  map f x

recursive let zipAll = lam lists.
if (any null lists) then []
else 
  let heads = map head lists in
  let tails = map tail lists in
  snoc (zipAll tails) heads
end

let mapIndex = lam f. lam n. 
  recursive let helper = lam acc. lam i. 
  if eqi i n then reverse acc
  else helper (cons (f i) acc) (addi i 1)
  in helper [] 0
