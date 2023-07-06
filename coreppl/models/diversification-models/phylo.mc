type Tree
con Node: {left: Tree, right: Tree, age: Float} -> Tree
con Leaf: {age: Float} -> Tree

recursive let logFactorial = lam n.
  if eqi n 1 then 0. else addf (log (int2float n)) (logFactorial (subi n 1))
end

recursive let countLeaves = lam tree.
  match tree with Node r then addi (countLeaves r.left) (countLeaves r.right) else 1
end

let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never
