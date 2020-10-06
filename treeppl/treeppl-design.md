# TreePPL Design

1. TreePPL is a universal probabilistic programming language focusing on phylogenetics:

	a. able to express any stochastic model that can be written down as a program
	b. has special language features to express phylogenetic models easily
	c. has special inference fine-tuned towards solving those models and works *fast*
	
2. TreePPL has easy and understandable syntax.

	a. Learning TreePPL should be easy if you have experience with languages and concepts that biologists first come in contact with such as R and Python.
		- Knowledge of OO (constructors, inheritance, etc.) should not be required to work with
			TreePPL
		- Advanced functional programming techniques should also be hidden from the examples
	b. Implementing TreePPL for us should be straightforward as we will have a template language to go to for inspiration. Possible template languages are
		- WebPPL/ JavaScript/ TypeScript/ CoffeeScript
		- functional languages: O'Caml, Haskell, Julia
		
3. TreePPL is a safe language to program and save's the programmers time.

	a. It has an (optional) type system.
	b. Object mutability is either disallowed or strongly discouraged.
	c. Loops are either disallowed or strongly discouraged (use the familyar `sapply` syntax from R, or an alternative `map` function.
	d. It has special safety features, specific for PPL (distribution support checking, etc.).
	
4. TreePPL is a fun language

	a. Nice documentation
	b. Interesting examples
	c. Easy tools
