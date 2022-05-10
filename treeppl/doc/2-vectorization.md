# 2. Vectorization

## 2.1 Ranges

	a = 0:10 
	// [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	
Employ disambiguation of precedence

	n = 4
	a = 1 : n - 1
	// Error

	a = 1 : (n - 1)
	a
	// [1, 2, 3]

	t = 10 : 1
	t
	// [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
	
## 2.2 Subsetting and filtering

	type nucleotide = A | C | G | T
	type dna = array(nucleotide)

	data: array(dna) = 
    [ [ C, C, A, A, C, A, A, A, A, A, A, A, A, A, A ],
      [ C, C, A, A, A, C, A, A, A, A, A, A, A, A, A ],
      [ A, A, C, C, A, A, C, A, A, A, A, A, A, A, A ],
      [ A, A, C, C, A, A, A, C, A, A, A, A, A, A, A ] ]

	data[1]
	// [ C, C, A, A, C, A, A, A, A, A, A, A, A, A, A ]

	data[ [ 1, 2 ] ]
	// [ [ C, C, A, A, C, A, A, A, A, A, A, A, A, A, A ],
      [ C, C, A, A, A, C, A, A, A, A, A, A, A, A, A ] ]

	data[0, 1]
	// - nucleotide: C

	data[[false, false, false, true]]
	//   [ A, A, C, C, A, A, A, C, A, A, A, A, A, A, A ] ]

	data[true, false, false, true]
	// Error, makes no sense.

	x: dna = [A, C, T]
	x[0]
	// - nucleotide: A

	x[2]
	// - nucleotide: T
	
	index = [0, 1]
	x[index] 
	// [A, C]
	
	index = [true, false, true]
	x[index] 
	// [A, T]

	x[!index]
	// [C]

	x[![0, 1]]
	// [T]
		
	x != A 
	// [false, true, true]

	ix = map( (k) => k != A, x)
	x[ix]

	x[x != A]
	// [C, T]


## 2.3 Map-reduce

	myrange = 1 to 5
	squares = map((k) => k^2, myrange)
	// [1, 4, 9, 16, 25]

	reduce((x, y = 0) => x + y, myrange)
	// 15

	filter((k) => k % 2 == 0, myrange)
	// [2, 4]


## 2.4 Array operations

	x = [1, 3, 5]
	y = x + 1
	// [2, 4, 6]

	x + y
	// [3, 7, 11]

	2*x
	//[2, 6, 10]

	x*y
	// Error: uncomptatible dimensions

	x*y'
	// 44 (dot-product)

	x.*y
	// [2, 12, 30] (element-wise)

	x'*y
	// [ [ ], [ ], [ ]] (matrix multiplication)

	z = [...x, 7]
	// [1, 3, 5, 7]

	concat = (x, y) => {
		[...x , ...y]
	}

	concat(x, y)
	// [1, 3, 5, 2, 4, 6]