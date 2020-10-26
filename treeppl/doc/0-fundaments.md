# 0. TreePPL Manifesto

TreePPL is a universal, functional and vectorized, phylogenetic, interpreted, compiled and gradually typed, probabilistic programming language, inspired by ReasonML.


1. **Universal PPL**

	a. We are able to express any stochastic model that can be written down as a TreePPL program.

2. **Functional and vectorized**

	a. When describing algorithms the models are expressed in terms of data transformations (functions).

	b. Functions have no side effects. Objects are immutable (mostly).

	c. To process and create lists, we use map-reduce style constructs mostly (rather than for loops).
	
	d. In addition to (c), we provide shortcuts to handle vectorized operations such as instantiation, contacatenation, element-wise operations.
	
3. **Phylogenetic**

	a. Has a phylogenetic library of standard phylogenetic data structures (trees) and algorithms (diversification, biography, phylogeney inference, etc.).
	
	b. Has optimized inference to work with phylogenetic data structures and is able to handle big phylogenetic datasets (> 10,000 species).
	
	d. Supports compositions of new inference strategies.
	
4. **Interpreted, compiled, and gradually typed**

	a. For fast prototyping a line-by-line (interpreted) execution is possible.
	
	b. Identifiers can be optionally typed: a mix of dynamical and statical typing.
	
	c. Programs can be compiled to efficient low-level code.
	
5. **Inspired by ReasonML**

	a. The syntax of TreePPL follows (a subset) of ReasonML.
	
	b. There are extensions for probabilisitc programming (Secion 1).
	
	c. There are extensions for vectorization (Section 2).
