include "string.mc"
mexpr
--let numdocs:Integer = 2 in
--let numtopics:Integer = 2 in
--let vocabsize:Integer = 3 in
let alpha:[Float] = [0.1, 0.1] in
let beta:[Float] = [0.1, 0.1, 0.1] in
--let docs = [0, 2, 1, 2] in
let wordDocIds = [0, 0, 1, 1] in
let phis = [assume (Dirichlet beta) , assume (Dirichlet beta)]  in
let thetas = [assume (Dirichlet alpha), assume (Dirichlet alpha)] in

-- DOCUMENT 0 WORD 0 --
--let docid = get wordDocIds 0 in
let theta = get thetas 0 in
let z0 = assume (Categorical theta) in
let phi = get phis z0 in
observe 0 (Categorical phi);

-- DOCUMENT 0 WORD 1 --
let theta = get thetas 0 in
let z1 = assume (Categorical theta) in
let phi = get phis z1 in
observe 2 (Categorical phi);

-- DOCUMENT 1 WORD 0 --
let docid = get wordDocIds 2 in
let theta = get thetas 1 in
let z2 = assume (Categorical theta) in
let phi = get phis z2 in
observe 1 (Categorical phi);

-- DOCUMENT 1 WORD 1 --
let docid = get wordDocIds 3 in
let theta = get thetas docid in
let z3 = assume (Categorical theta) in
let phi = get phis z3 in
observe 2 (Categorical phi);
addi z0 (addi z1 (addi z2 z3))
--join [int2string z0, int2string z1,int2string z2,int2string z3]
