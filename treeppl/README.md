# TreePPL – A Universal Probabilistic Programming Language for Evolutionary Biology

Probabilistic programming techniques have developed rapidly in recent years. The ultimate vision is to provide expressive, universal, Turing-complete model description languages, while at the same time supporting the automated generation of efficient inference algorithms from these model descriptions. This would allow empiricists to easily and succinctly describe any model they might be interested in, relying on the automated machinery to provide efficient inference algorithms for that model.

Current probabilistic programming languages (PPLs) are often difficult to use for empiricists. Furthermore, even though there is now swift progress in PPL inference strategies, there is still a substantial gap in many domains before PPL systems can compete successfully with dedicated software, or even provide computationally feasible solutions.

The aim of the TreePPL project is to develop a probabilistic programming language for evolutionary biology. These are our design goals:

1. **It is possible and easy to express any stochastic model in TreePPL.**


	- TreePPL is a __universal__ Probabilistic Programming Language. Any stochastic model that is computable can be expressed in it. (now)
	- We offer __a library for a high-level langauge__ (R, Python) to connect TreePPL to the high-level environment. The library does I/O between TreePPL and the high-level langauge. File-system operations, data pre-processing and post-processing/ graphics are handled in the high-level language. (now)
	- From a web browser (later)
	- Limited side-effects such as printing are supported. (now)
	- We support __vectorized__ operations. (later)
	- The syntax is based on the familiar __C/C++/JavaScript__ family of languages. (now)
	- __Statically typed__; types are only mandatory for function signatures. (now)
	- __Nested inference__. (later)
	- SMC inference (now)
	- MCMC and hybrid and more inference (later)
	- Inference spec (num particles, inference type) in preamble (now)
	- or supplied via an external language interface/ cmdline (now)
	- Inference hints in model files (now)
	- Models of tree inference (now)
	https://yulab-smu.top/treedata-book/chapter1.html

	
2. **TreePPL is especially well-suited for phylogenetics and evolutionary biology.**

	- It has a phylogenetic library of standard phylogenetic data structures (trees) and algorithms (diversification, biography, phylogeney inference, etc.).
	
	- Has optimized inference to work with phylogenetic data structures and is able to handle big phylogenetic datasets (> 10,000 species).

	
3. **TreePPL is a fast language.**

	- For fast prototyping a line-by-line (interpreted) execution is possible.
	
	- Thanks to type safety, it is find and remove bugs.
	
	- Programs can be compiled to efficient low-level code.

In a [recent paper](https://www.biorxiv.org/content/10.1101/2020.06.16.154443v3), we introduce probabilistic programming and develop efficient PPL approaches to advanced models of biological diversification, such as the BAMM model and the ClaDS models, using [WebPPL](http://webppl.org/) and another recent PPL, [Birch](http://webppl.org/). The paper demonstrates some of the potential power of probabilistic programming in statistical phylogenetics, and discusses the main hurdles that remain to be tackled before these techniques can be applied to the entire range of phylogenetic models.

TreePPL will be built on top of [Miking](https://dl.acm.org/doi/10.1145/3357766.3359531), a language framework for constructing efficient compilers for domain-specific languages.

This web site will be continuously updated, with the ultimate aim of developing it into the primary online resource for the TreePPL community.

