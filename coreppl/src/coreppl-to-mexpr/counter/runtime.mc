include "ext/math-ext.mc"
include "string.mc"

type Counter = Ref Int
let incrementCounter = lam c. modref c (addi (deref c) 1)
let printCounter = lam c. print (join ["Counter:",(int2string (deref c)),"\n"])

let runC = lam model. 
  (lam s. model (ref 0) s)
