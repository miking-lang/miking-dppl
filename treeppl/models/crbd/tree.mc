/-
Built-in tree data type, functions on it, and tests.
This file should be included in programs that use the Tree
datatype.

To test, run

  mi run tree.mc --test

Successful tests will produce two dots.
-/ 

type Tree -- 14
con Leaf : { age : Float, index: Int } -> Tree -- 15
con Node : { left : Tree, right : Tree, age : Float, index: Int } -> Tree -- 16

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

utest countLeaves tree1 with 2 in
utest countLeaves tree2 with 1 in

()