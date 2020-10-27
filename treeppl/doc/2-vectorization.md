# 2. Vectorization
		 
## 2.1 .. operator 

	let a = 0..10 
	// [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
	// results in list, or?
	
Precedence works like this

	let n = 4
	let a = 1..n - 1
	// a is [1, 2, 3]
	
## 2.2 Subsetting and filtering

Both list and array should be OK to use for subsetting.
If the final result of the subsetting is a single element list
or array, it is transformed to a scalar.

### Arrays

You can subset arrays with lists that indicate which array indices we want, for example:

	let x = [| 0., 3.14, 1., 2., 3.0 |]

	x[0]
	// 0
	
	x[-[0,1]]
	// [| 1, 2, 3 |]
	
	x[ [ 0, 1] ]
	// [| 0, 3.14 |]
	
	x == 3.14 
	// Array.map( (k) => k == 3.0, x)
	// [| FALSE, TRUE, FALSE, FALSE, FALSE |]
	
	x[x == 3.14]
	// [| 3.14 |]


