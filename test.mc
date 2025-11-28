mexpr


-- let g = lam f. lam a. f 1.0 a in
-- addf (g addf 1.0) (g mulf 3.0)

-- recursive let g = lam f. lam a. f 1.0 a in
-- addf (g addf 1.0) (g mulf 3.0)

-- recursive let map = lam f. lam l.
--   match l with [x] ++ l
--   then cons (f x) (map f l)
--   else [] in
-- concat (map (addf 1.0) [1.0, 2.0]) (map (addf 1.0) [1.0, 2.0])

-- recursive let map = lam f. lam l.
--   match l with [x] ++ l
--   then cons (f x) (map f l)
--   else [] in
-- concat (map (addf 1.0) [1.0, 2.0]) (map (mulf 1.0) [1.0, 2.0])

recursive let map = lam f. lam l.
  match l with [x] ++ l
  then cons (f x) (map f l)
  else [] in
let blub = 42.0 in
let x = blub in
concat (map (lam x. addf x blub) [1.0, 2.0]) (map (mulf 1.0) [1.0, 2.0])
