# TreePPL Manifesto

Semantics:

1. How do we handle typing 
 - dynamically typed language?
 - or a statistically typed language when all checking is done at compile-time (more conservative)?

2. When instructing the user, how do they think about models, do they think in terms of objects or in functions?

Syntax:

3. Inspirational syntax, what language do we as a template language?

_TreePPL is a universal, functional and vectorized, phylogenetic, interpreted, compiled and gradually typed, probabilistic programming language, inspired by WebPPL, Birch, R and Python._

1. *Universal PPL* 

	a. We are able to express any stochastic model that can be written down as a TreePPL program.

2. *Functional and vectorized*

	a. When describing algorithms the models are expressed in terms of data transformations (functions).

	b. Functions have no side effects. Objects are immutable.

	c. To process and create lists, we use map-reduce style constructs (rather than for loops).
	
	d. In addition to (c), we provide shortcuts to handle vectorized operations such as instantiation, contacatenation, element-wise operations.
	
3. *Phylogenetic*

	a. Has a phylogenetic library of standard phylogenetic data structures (trees) and algorithms (diversification, biography, phylogeney inference, etc.).
	
	b. Has optimized inference to work with phylogenetic data structures and is able to handle big phylogenetic datasets (> 10,000 species).
	
	d. Supports compositions of new inference strategies.
	
4. *Interpreted, compiled, and gradually typed*

	a. For fast prototyping a line-by-line execution is possible.
	
	b. Identifiers can be optionally typed.
	
	c. Programs can be compiled to efficient CUDA-based code.
	
5. *Inspired by WebPPL, Birch, R and Python*

	a. The syntax of TreePPL follows WebPPL as much as possible, with some simplifications and good ideas from Birch, R and Python are introduced where appropriate.
