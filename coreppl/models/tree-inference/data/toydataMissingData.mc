type Tree
con Leaf : {age: Float, seq: [Int]} -> Tree

let a:Tree = Leaf {age=0.0, seq=[1, 1, 0, 4]} 
let b:Tree = Leaf {age=0.0, seq=[1, 1, 0, 2]} 
let c:Tree = Leaf {age=0.0, seq=[0, 4, 1, 1]}
let d:Tree = Leaf {age=0.0, seq=[0, 0, 1, 1]}
let trees:[Tree] = [a,b,c,d]
let seqLength = 4