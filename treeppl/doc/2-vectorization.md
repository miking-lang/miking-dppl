# 2. Vectorization
		 
## 2.1 Column operator 

	let a = 0:10 
	// [| 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 |]
	// results in array, not list
	
## 2.2 Subsetting and filtering

Both list and array should be OK to use for subsetting.
If the final result of the subsetting is a single element list
or array, it is transformed to a scalar.

### Lists

	let x = ["pi", 3.14, 1, 2, 3]
	
	x[0]
	// "pi"
	
	x[-[0,1]]
	// [1, 2, 3]
	
	x[[0, 1]]
	// ["pi", 3.14]
	
	x == 3.14 
	// [FALSE, TRUE, FALSE, FALSE, FALSE]
	
	
	x[x == 3.14]
	// 3.14

### Arrays

	let x = [| "pi", 3.14, 1, 2, 3 |]

	x[0]
	// "pi"
	
	x[-[0,1]]
	// [| 1, 2, 3 |]
	
	x[ [| 0, 1|] ]
	// [| "pi", 3.14 |]
	
	x == 3.14 
	// [| FALSE, TRUE, FALSE, FALSE, FALSE |]
	
	
	x[x == 3.14]
	// 3.14


