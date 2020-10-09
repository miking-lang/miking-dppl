# TreePPL Design "Manifesto"

1. TreePPL is a universal probabilistic programming language focusing on phylogenetics:

	a. able to express any stochastic model that can be written down as a program
	b. has special language features to express phylogenetic models easily
	c. has special inference fine-tuned towards solving those models and works *fast*
	d. supports compositions of new inference strategies
	
2. TreePPL has easy and understandable and simple syntax.

	a. Learning TreePPL should be easy if you have experience with languages and concepts that biologists first come in contact with such as R and Python.
	
	b. Knowledge of advanced OO should not be required to work with TreePPL
			- constructors
			- simple inheritance ==> probably OK
			- multiple inheritance
			- operator overloading
			- interfaces
			
	 c. Advanced functional programming techniques should also be hidden from the examples
			- functions as first class citizens ==> OK		
			
	d. Implementing TreePPL for us should be straightforward as we will have a template language to go to for inspiration.
	
	A *functional* subset of TypeScript?
	
	e. Possible template languages are
	    - Python/ R (not as template, but as understanding that is the background of our users); also MATLAB/Julia sometimes
		- WebPPL/ JavaScript/ TypeScript/ CoffeeScript
		- functional languages: O'Caml, Haskell
		
3. TreePPL is a safe language to program and saves the programmer's time.

	a. It has an (optional) type system.
	b. Object mutability is either disallowed or strongly discouraged ---> get inspired by Scala	
	c. Loops are either disallowed or strongly discouraged (use the familiar `sapply` syntax from R, or an alternative `map` function.
	d. It has special safety features, specific for PPL (distribution support checking, etc.).
	
4. TreePPL is a fun language

	a. Nice documentation
	b. Interesting examples
	c. Easy tools, maybe embedding in a higher level language such as Python and R






Semantics:
1. How do we handle typing?
How about gradual typing together with a mix of interpretation and compilation?
"Piggy-back on miking".

2. When instructing the user, how do they think about models, do they in object or in function?

Syntax:
3. Inspirational syntax, what language do we as a template language?
