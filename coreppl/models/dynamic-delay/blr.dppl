include "seq.mc"
mexpr
let data = [(8.3252, 4.526), (8.3014, 3.585), (7.2574, 3.521), (5.6431, 3.413), (3.8462, 3.422), (4.0368, 2.697), (3.6591, 2.992)] in
let k = delay (Gaussian 0.0 5.0) in
let b = assume (Gaussian 0.0 5.0) in
iter (lam point.
	let k = delayed k in
	let x = addf (mulf k point.0) b in
	observe point.1 (Gaussian x 2.)
) data



