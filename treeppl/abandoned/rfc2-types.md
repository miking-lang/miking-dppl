# TreePPL RFC 2: Types

TreePPL should have a type system in order to make debugging easier.

Types are

a. Optional (gradual typing), i.e. type annotations, for example in function declaration possible but not necessary. 

b. Inferred, where possible
	
Examples: 

	a: bigint = 4
	b = 5n         /* b is bigint   */
	c = 5          /* c is number   */
	a = "hello"    /* a is a string */
	
## Declarations

	a = "hello"     /* OK */
	let a = "hello" /* also OK, let is optional/ implicitly understood */
	//var a = "hello"   /* NOT implementing this */
	//const a = "hello" /* NOT implemented, remapping of identifiers not needed, everything is a const */

## Built-in Types

We are reusing the TypeScript basic types from

https://www.typescriptlang.org/docs/handbook/basic-types.html

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
	list: Array<number> = [1, 2, 3]
 
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

TODO Ask David and Daniel to think about how to take care of "uncertain" types.


## User-defined types

### Union types and Recursive Types

Note, this is just example, not how will end up implementing the tree datastructure in the library.

	type ExtantLeaf =
	{
		t: NonnegativeReal
	}
	
	type ExtinctLeaf = 
	{
		t: NonnegativeReal
	}
	
	type Speciation = 
	{
		t: NonnegativeReal,
		left:  Tree,
		right: Tree
	}
	
	type Tree = Extant | Extinct | Speciation


### Restrictions on the domain

TODO Discuss with David and Daniel

	type PositiveReal = Real | x => x > 0
	type PositiveReal = Real : x => x > 0
	type PositiveReal = Real where x => x > 0
	type PositiveReal = Real passing x => x > 0
	
	function fn(x) 
	{
		x > 0
	}
	
	type PositiveReal = Real | fn
	type PositiveReal = Real : fn
	type PositiveReal = Real where fn
	type PositiveReal = Real passing fn
	
