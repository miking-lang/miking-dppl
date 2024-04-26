type Tree
con Leaf : {age: Float, seq: [Int]} -> Tree

let a:Tree = Leaf {age=0.0, seq=[1, 1, 0, 2, 3, 0, 0, 0, 0, 3, 1, 1, 3, 3, 2]} 
let b:Tree = Leaf {age=0.0, seq=[1, 1, 0, 2, 0, 1, 0, 0, 0, 3, 1, 0, 1, 1, 0]} 
let c:Tree = Leaf {age=0.0, seq=[0, 0, 1, 1, 0, 2, 1, 0, 0, 0, 2, 0, 3, 3, 0]}
let d:Tree = Leaf {age=0.0, seq=[0, 0, 1, 1, 0, 3, 0, 1, 0, 0, 2, 2, 3, 1, 0]}
let trees:[Tree] = [a,b,c,d]
let seqLength = 15