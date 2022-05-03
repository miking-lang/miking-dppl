
-- Tree type definition
type Tree
con Node : {left : Tree, right : Tree, age : Float } -> Tree
con Leaf : {age : Float} -> Tree


-- Project the age from a tree
let getAge = lam n: Tree. match n with Node r then r.age else
                 match n with Leaf r then r.age else
                 never


-- Count the number of leaves in a tree
recursive
let countLeaves: Tree -> Int = lam tree: Tree.
  match tree with Node r then
    addi (countLeaves r.left) (countLeaves r.right)
  else 1
end


mexpr

let tree1 = Node {left = Leaf {age = 2.}, right = Leaf {age = 7.}, age = 5.0} in
let tree2 = Leaf {age = 3.} in

utest getAge tree1 with 5. in
utest getAge tree2 with 3. in
utest getAge (getLeft tree1) with 2. in
utest getAge (getRight tree1) with 7. in
utest countLeaves tree1 with 2. in
utest countLeaves tree2 with 1. in

()
