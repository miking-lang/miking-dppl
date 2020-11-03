# 1. Probabilistic Extensions

The constructs described in this chapter make TreePPL a universal probabilistic programming language.

## 1.1 Distributions

Those should probably be expressed as Reason Modules.

### Permutation(~vec: array)

Distribution over `vec: Array[n]`, where the elements of `v` are permutations of the elements of `vector`.

### Categorical(~prob: array(probability), support: array)

Distribution over `v`, where `v` is an element of `support` with probability `probabilities[support == v]`.

Note: the suggested syntax `probabilities[support == v]` implies subsetting the probabilities vector with the index of the support `vector`, whose value is equal to `v`. This is inspired by R.

### Exponential(~rate: positive_real)

Distribution over [0, Infinity].

Note positive_real has not been implemented in the Library yet.

### Dirichet(~alpha: array(positive_real)

Distribution over `v: PositiveReal[d]`.


## 1.2 Sampling

### Assume/sample

General syntax. x is unitialized yet (in the scope), or we want to throw away its previous binding:

	assume x ~ Dist(params)  // now x is random variable (a structure with variate/ realization and the associated distribution), OR <=>
	let x = assume(Dist(params))  
	let x = assume(Dist(params), ~sample = TRUE)   // forced sampling in the sense of immediate sampling as opposed to delayed sampling
	
For example

	assume x ~ Exponential(~rate = 1) 
	// x is r.v. but may not have a realization yet
	
	let x = assume(Exponential(~rate = 1))
	// x is r.v. but may not have a realization yet same as 44
	
	let x = assume(Exponential(~rate = 1), ~sample = TRUE)
	// x is r.v. with a concreate realization of a value
	
WebPPL refernce behavior

	x = sample(Exponential({rate: 1})
	//sample is always immediate in WebPPL :(

### Observe

General syntax. `x` must exist and have value. Then:

	observe x ~ Dist(params)
	
For example
	
	observe 2 ~ Exponential(~rate = 1)
	
Reference WebPPL code:

	observe(Exponential({rate: 1}), 2)
	
### Automatic behavior

Note: this potentially could be difficult.

If `x` is in the current execution environment and has a value (`x` is e.g. `float`), then

	x ~ Dist(params) // is equivalent to:
	observe x ~ Dist(params)

`x` is observed from Dist, like `~>` in Birch.

If the binding does not exist, x is sampled, then:

	x ~ Dist(params) // is equivalent to
	assume x ~ Dist(params)

### Propose

The propose statement is used for bridge sampling.

	propose x ~ Dist(params)
	update x ~ OtherDist(params)

`x` is proposed from some suitable distribution, it is defined later
what distribution it is sampled from.

Here is reference WebPPL code

	// Expressed in WebPPL:
	var x = sample( Exponential( {a: 1} ) )    // sample from proposal distribution

	// potentially lots of intervening code
	// compensate for the fact that we proposed from the wrong distribution
	factor( -Exponential( {a: 1} ).score(x) )  // remove weight due to proposal
	factor( Gamma( {a: 2, b: 1} ).score(x) )   // add weight from correct dist.

	// Equivalent code might look like this in TreePPL:
	propose x ~ Exponential(~rate = 1)    // sample from proposal distribution
	// potentially lots of code and function calls
	// specify correct distribution (automatically correct for proposal density mismatch)
	update x ~ Gamma(~alpha = 2, ~beta = 1)


### IID Sampling

	assume x ~ iid(~dist = Dist(params), ~n = num_reps)    // iid distribution of length num_reps	
	// x is a array or list of r.v.


## 1.3 Inference

The general inference syntax is similar to WebPPL with the exception that model 
can have some additional arguments.

	// We believe in
	// - named arguments
	// - no currying
	// - default arguments
	 
	// R
	generic_model = function(p = 0.5) {
	
	}
	
	// JavaScript with some type annotation
	function generic_model(p: float = 0.5):float {
	}
	
	// ReasonML
	let generic_model = (p: float = 0.5):float =>
	{
		flip(p)
	}
	
	let mymod = () =>
	{
		generic_model(0.6)
	}
	
	let dist = Infer(model = mymod, method = 'SMC')
	
	dist
	
	
	
