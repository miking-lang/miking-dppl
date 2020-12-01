# 1. Probabilistic Extensions

The constructs described in this chapter make TreePPL a universal probabilistic programming language.

## 1.1 Distributions

### 1.1.1 Permutation(~vec: array)

Distribution over `vec: Array[n]`, where the elements of `v` are permutations of the elements of `vector`.

### 1.1.2 Categorical(~prob: array(probability), support: array)

Distribution over `v`, where `v` is an element of `support` with probability `probabilities[support == v]`.

Note: the suggested syntax `probabilities[support == v]` implies subsetting the probabilities vector with the index of the support `vector`, whose value is equal to `v`. This is inspired by R.

### 1.1.3 Exponential(~rate: positive_real)

Distribution over [0, Infinity].

Note positive_real has not been implemented in the Library yet.

### 1.1.4 Dirichet(~alpha: array(positive_real)

Distribution over `v: PositiveReal[d]`.


## 1.2 Sampling

### 1.2.1 Assume/sample

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

### 1.2.2 Observe

General syntax. `x` must exist and have value. Then:

	x = 5
	observe x ~ Dist(params)
	
For example
	
	observe 2 ~ Exponential(rate = 1)
	
Reference WebPPL code:

	observe(Exponential({rate: 1}), 2)

### 1.2.3 Factor

### 1.2.4 Propose

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

### 1.2.5 Automatic disambiguation of sampling statement

disambiguates between

assume/sample
observe
and propose/update

Note: this potentially could be difficult.

Cases:
1. Name exists but has no value
	
	float x 
	//1.  name but no value

2. Name exists and we have a value
	
	float x = 2
	//2. name and we have a value

3. `x` is undefined

If `x` is in the current scope, then

	// in case of 2
	x ~ Dist(params) // is equivalent to:
	observe x ~ Dist(params)

`x` is observed from Dist, like `~>` in Birch.

In case of 3 If the binding does not exist, x is sampled, then:

	x ~ Dist(params) // is equivalent to
	assume x ~ Dist(params)


### 1.2.6 IID Sampling

	assume x ~ iid(~dist = Dist(params), ~n = num_reps)    // iid distribution of length num_reps	
	// x is a array or list of r.v.


## 1.3 Inference

- Every TreePPL is implicity understood as a generative description of a distribution. It is thus not needed to explicitly specify an `Infer` statement.

- Inference parameters can be passed to the compiler via *inference hints*. Here are a few examples of inference hints:
```
	//~RESAMPLE
	//~PARTICLES: 10000
	//~METHOD: "SMC"
```
- The syntax of an inference hints begins with a `//~` followed by a keyword, optionally followed by `:` and a value.

- _Nested_ inference is possible.

## 1.4 Model function

Every program needs to have a function named _model_. The parameters of the model function are used for input. The return value of the model function over its multiple executions is the distribution of interest.