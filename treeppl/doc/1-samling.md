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

### Sample

	let x ~ Dist(params) // OR
	let x = Dist.sample(params)

`x` is sampled from `dist`, like '~' in Birch.

### Observe or sample

	`x ~ Dist(params)`

`x` is observed from Dist, like `~>` in Birch. If the binding does not exist, x is sampled

### Propose

The propose statement is used for bridge sampling.

	propose x ~ Dist(params)

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
	propose x ~ Exponential( rate=1 )    // sample from proposal distribution
	// potentially lots of code and function calls
	// specify correct distribution (automatically correct for proposal density mismatch)
	x ~ Gamma( alpha=2, beta=1 )


### IID Sampling

	let x ~ iid(~dist = Dist(params), ~n = num_reps)    // iid distribution of length num_reps	


## 1.3 Inference

The general inference syntax is similar to WebPPL with the exception that model can be a function of arguments.


	let dist = Infer(~model = model, ~method = 'SMC') // OR
	let dist1 = Infer(~model = model(a), ~method = 'SMC)
	dist
