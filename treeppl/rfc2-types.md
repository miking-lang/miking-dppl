# TreePPL RFC 2: Types

TreePPL should have a type system in order to make debugging easier.

Types are

a. Optional (gradual typing), i.e. type annotations, for example in function declaration possible but not necessary. 

	a: BigInt = 4
	b = 5

b. Inferred, where possible
	
	a = "hello" /* a is a string */


## Built-in Types

See https://www.typescriptlang.org/docs/handbook/basic-types.html

### Boolean

### Number

### BigInt

Note: should we create an arbitrary precision data-type such as

https://www.researchgate.net/publication/302586033_Exact_Computation_with_leda_real_-_Theory_and_Geometrie_Applications ?

Or a rational number?

### String

### Array

All values in the array are of the same type.

	list: number[] = [1, 2, 3]
	list: Array<number> = [1, 2, 3];	 
 
### Tuple

Tuple types allow you to express an array with a fixed number of elements whose types are known, but need not be the same. For example, you may want to represent a value as a pair of a string and a number:

	// Declare a tuple type
	x: [string, number]
	// Initialize it
	x = ["hello", 10] // OK
	// Initialize it incorrectly
	x = [10, "hello"] // Error

### Enum

	enum Color {
		Red,
		Green,
		Blue,
	}
	c: Color = Color.Green

### Unknown, Any, Void

We have to thing about this.


## User-defined types

### OR (|)

State is 0 or 1 or 2 or 3:

	type State = 0 | 1 | 2 | 3

### Restrictions on the domain

	type PositiveReal = x where x: Real, x > 0
	
### Recursive type





/**
 *  Sample function declaration 
 *    alternative syntax commented out */

let simple_evolution_model = function(a: TypeA, b: TypeB) => TypeC

function simple_evolution_model(a: TypeA, TypeB) : TypeC

simple_evolution_model(
    observed_nucleotide:   Phylo.Nucleotide,
    sequencing_accuracy_p: Probability = 0.9, // default value
//  sequencing_accuracy_p: Real in [0, 1],
//  sequencing_accuracy_p: Real passes positive_probability,
    Q: Phylo.InstantaneousRateMatrix,
//  Q: Real[][] passes check_QT,
    time: Real,
    μ: Real in [0, Infinity)
): Phylo.Nucleotide // return value of the function
{
  return Phylo.A
}


/**
 * Arguments to functions can be labeled (like R, Python...)
 * Labelling is optional
 * Arguments can have default values
 *  Reordering the named values should be possible
 *  mixing of keyword and positional arguments should be possible
 *  Example:
 * foo( mean = mu, sd = sigma )
 */


