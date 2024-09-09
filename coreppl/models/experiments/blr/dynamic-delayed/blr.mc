include "../../data/data-california-housing.mc"
include "seq.mc"
mexpr
let k = delay (Gaussian 0.0 5.0) in
let b = assume (Gaussian 0.0 5.0) in
iter (lam point.
	let k = delayed k in
	let x = addf (mulf k point.0) b in
	observe point.1 (Gaussian x 2.)
) data



