include "ext/dist-ext.mc"
mexpr
--let a = assume (Gamma 2. 2.) in
--let a = delayed a in
let p1 = divf 1. 2. in
let p2 = 2. in
weight (lomaxLogPdf p1 p2 1.5) 
--observe 1.5 (Exponential a)


