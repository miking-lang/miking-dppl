type Counter = Ref Int 

let incrementCounter = lam counter. modref counter (addi (deref counter) 1)

let createCounter = lam. ref 0

let run = lam model. 
  let counter = createCounter () in
  model counter
