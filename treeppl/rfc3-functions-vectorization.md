# TreePPL RFC 3: Functions and Vectorization

## Sample function declaration 

	gtr = function(p: Probability[4], r: Rate[5]) => InstantaneousRateMatrix
	{
		// ... 
	}

## Function invocation

Arguments to functions can be labeled (like R, Python...)
Labelling is optional
Arguments can have default values
Reordering the named values should be possible
mixing of keyword and positional arguments should be possible

Example:

	  Q = gtr(p = pi, r = rep(1, 6))
	  Q = gtr(pi, rep(1, 6))
	  Q = gtr(r = rep(1, 6), p = pi)
	  Q = gtr(p = pi, rep(1, 6))
	  
## Variadic arguments

	foo(...arr1) {
	 // arr1 is an array with the arguments...
	}
	
	foo(a = 1, b = 2)
	foo(a = 1, b = 2, c = 3)
		 
## Vectorization

	a = 0:10 // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	b = a - 1 // [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	c = [...a, ...b] // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,  -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	d = [a, b] // doesn't unlist [[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]
	e = rep("a", 6) // ["a", "a", "a", "a", "a", "a"]
	
## Subsetting and filtering

Most of this syntax is inspired by R

	x = ["pi", 3.14, 1, 2, 3]
	x[0] // "pi"
	x[[0, 1]] // ["pi", 3.14]
	x[-[0, 1]] // removes elements indexed by 0 and 1, 1, 2, 3
	x == 3.14 // [FALSE, TRUE, FALSE, FALSE, FALSE]
	// alternatively x ?= 3.14
	// which(x, 3.14)
	x[x == 3.14] // 3.14
	// x[x ?= 3.14]
	// x(which(x, 3.14)


## Map and reduce

	aa = a.map(function(x, i)
	{
		x + 10
	}
	)  // aa is 10, 20,...
	
	a.reduce(function(accummulator, x) {
		accummulator + x
	} // output should be 55

## List comprehension
```
 trees[i].states ~ Infer(
	ctmc(
	    parental_state = parental_node.states[k],
	    evolution_time = parental_node.age - trees[i].age,
	    Q = Q,
	    P = transition_probabilities(Q)
	)
    ) where k in 0:(new_node.states.length - 1)
    ```
