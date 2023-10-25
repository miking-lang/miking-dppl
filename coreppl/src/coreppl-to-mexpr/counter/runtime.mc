include "ext/math-ext.mc"
include "string.mc"
let incrementCounter = lam c. modref c (addi (deref c) 1)
let printCounter = lam c. print (int2string (deref c))
