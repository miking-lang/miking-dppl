# TreePPL RFC 1: Sampling, Inference and Distributions

## Built-in distributions

Distributions are usually capitalized but their parameters are key-value pairs, not objects like in WebPPL.

### Permutation(vector: Array[n])

Distribution over `v: Array[n]`, where the elements of `v` are permutations of the elements of `vector`.

### Categorical(probabilities: Probability[n], support: Array[n])

Distribution over `v`, where `v` is an element of `support` with probability `probabilities[support == v]`.

Note: the suggested syntax `probabilities[support == v]` implies subsetting the probabilities vector with the index of the support `vector`, whose value is equal to `v`. This is inspired by R.

Alternative can be `probabilities[support ?= v]` or `probabilities[which(v, support)]`.

### Exponential(rate: PositiveReal)

Distribution over [0, Infinity]

### Dirichet(alpha: PositiiveReal[d])

Distribution over `v: PositiveReal[d]`.


## Sampling

### Tilde operator

`x ~ Dist(params)`

`x` is either sampled or observed from `dist`, like '~' in Birch.

### Sample

`sample x ~ Dist(params)`

`x` is sampled from dist, like `<~` in Birch.

### Observe

`observe x ~ Dist(params) `

`x` is observed from dist, like `~>` in Birch.

### Propose

The propose statement is used for bridge sampling.

`propose x ~ Dist(params) `

`x` is proposed from some suitable distribution, it is defined later
what distribution it is sampled from.

Here is reference WebPPL code

```
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
```

### iid sampling

```
x ~ iid(Dist(params), num_reps)    // iid distribution of length num_reps
x[i] ~ Dist(params) where i in 0:(n - 1)       // alternative: comprehension style
```

Note: `where` corresponds to the `:` operator in Haskell for list
comprehension. The vectorized operation `0:(n - 1)` creates a
vector (0, 1, 2, ..., n - 1). Inspired by R.


## Inference

The general inference syntax is similar to WebPPL with the exception that model can be a function of arguments.

```
dist = Infer(model = model, method='SMC')
dist
```

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
