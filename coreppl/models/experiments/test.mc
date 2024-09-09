include "ext/dist-ext.mc"
mexpr
/-let mu = delay (Gamma 2. 3.) in
let mu = delayed mu in
let a  = assume (Exponential mu) in a-/
let mu = delay (Gamma 2. 3.) in
let mu = delayed mu in
let a  = assume (Exponential mu) in
observe 0.5 (Exponential mu);
--observe 0.5 (Exponential mu);
observe 1. (Gaussian a 1.)

/-
let shape = 2. in
let scale = 3. in
--let mu = assume (Gamma 2. 3.) in
--let mu = delayed mu in
let a = lomaxSample shape (divf 1. scale) in
--let a  = assume (Exponential mu) in
weight (lomaxLogPdf shape (divf 1. scale)  0.5);
--observe 0.5 (Exponential mu);
observe 1. (Gaussian a 1.)-/
