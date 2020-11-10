# 0. Language Fundamentals

## 0.1 Identifiers and assignment

Changes to ReasonML are:

  - no need to explicitly write `let`; the "binding" is taken care of by the assignment operator `=`;
  - support unicode characters an extended character set including the Greek;
  - support of identifiers starting with a capital letter, but discoraged.

Similarities to ReasonML:

  - overshadowing (reassignment) possible;
  - type annotations are possible.

```
x = 1
x
// - : int = 1;

x = 2
x
// - : int = 2;

x = 1.
x
// - : float = 1.;

λ = 0.2
λ
// - : float = 0.2;

A = 1
// Warning: do you mean constructor?
// - : int = 1;

transition_probability: float = 1
// - : float = 1.;
```		

## 0.2 Functions

Changes to ReasonML:
  - explicit returns allowed; no need to put parenth. around;
  - no currying;
  - tilde not needed when naming params;
  - default arguments supported;
  - no need to type `rec`.

 Similarities to ReasonML:
  - anononymous functions OK;
  - curly braces not needed if just one expression; and
  - last expression is the default return value.

A future note:
  - optional types.

```
x = (p: int) => p + 1 

x(2)
// - int: 3

x = (p: int): int => {
	d = 2
	if (p == 0)	return -1
	p*d
}

x(2)
// - int: 4

x(0)
// - int: -1

/** 
 * Illustrate default values and named parameters
 *
 * Returns the area. Default is a square with side x.
 */
make_rectangle = (x: float, y: float = 0.) => {
	if (y == 0.0) x*x else x*y
}

make_rectangle()
// Error

make_rectangle(2)
// - float: 4.0

make_rectangle(x = 2)
// - float: 4.0

make_rectangle(x = 2, y = 3)
// - float: 6.0

// order changed!
make_rectangle(y = 2, x = 3)
// - float: 6.0

make_rectangle(2, 3)
// - float: 6.0

make_rectangle(y = 2, 3)
// Error, when you mix, you first start with the positional arguments

make_rectangle(2, y = 3)
// - float: 6.0
```


## 0.3 Primitive types and operators

We take ReasonML primitive datatypes with the following changes:
  - unit and optional are removed;
  - array literals uses the `[]` rather than `[||]`;
  - lists can be instantiated with a special constructor working on an array.

```
Int	      x: int = 10;
Float	  x: float = 10.0;
Boolean	  x: bool = false;
String	  x: string = "ten";
Char	  x: char = 'c';
Tuple	  x: (int, string) = (10, "ten");
Array	  x: array(int) = [1, 2, 3];
Functions x: (int, int) => int = (a, b) => a + b;
```

Adding of float works with `+` !

```
x = 3.14
y = 3.14

x + y
// - float: 6.28
```

Example of array:

```
data: array(array(dna)) = 
    [ [ C, C, A, A, C, A, A, A, A, A, A, A, A, A, A ],
      [ C, C, A, A, A, C, A, A, A, A, A, A, A, A, A ],
      [ A, A, C, C, A, A, C, A, A, A, A, A, A, A, A ],
      [ A, A, C, C, A, A, A, C, A, A, A, A, A, A, A ] ]
```

Linked lists are implemented via a constructor:

```
x: list(int) = List([1, 2, 3])
```

## 0.4 User defined types

We want to implement as much as possible of ReasonML's type system. For example, we have:

  - algebraic types;
  - recursive types.

For example, to define a `tree`, we use

```
type tree =
    | Leaf(age, dna)
    | Node(age, dna, tree, tree);
```

We support the _module system_ for code organization.

Variant datatype:

```
type nucleotide = A | C | G | T
type dna = array(nucleotide)
```

Records:

```
type person = {
  name: string,
  age: int,
}
```

As a vision we want types such as `real`, `positive_real`, etc.

## 0.5 Conditionals

As in ReasonML, i.e. `if` and `switch`.

## 0.6 Loops

Loops are discouraged in most cases. Instead functional programming patterns like map, filter, or reduce can usually be used in their place.

```
x = 1;
y = 5;

for (i in x to y) {
  print_int(i);
  print_string(" ");
};
/* Prints: 1 2 3 4 5 */
```

The reverse direction is also possible using the downto keyword:

```
for (i in y downto x) {
  print_int(i);
  print_string(" ");
};
/* Prints: 5 4 3 2 1 */
```