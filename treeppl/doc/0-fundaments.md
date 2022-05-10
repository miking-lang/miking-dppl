# 0. Language Fundamentals

## 0.1 Identifiers, statements, and expressions

- You don't end statements with ';'.
- Type annotations are possible.
- No need to explicitly write `let`; the name binding is taken care of by the operator `=`.
- Support Unicode characters an extended character set including the Greek.
- Support of identifiers starting with a capital letter, but discouraged.
- Overshadowing (reassignment) possible;
- Blocks of statements are enclosed in `{ }`

Examples:

```
x = 1
x
// - : int = 1;

{
  x = 2
  x
  // - : int = 2;
}
x
// - : int = 1

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

- Anonymous functions OK.
- A function can be defined as a single expression (no curly braces needed).
- If a function is defined as a sequence of statements (with curly braces), an explicit return is needed. There is no need to put parenthesis around `return`. If no `return` is provided, the function is considered to have the unit type.
- No currying.
- Parameters can be named. 
- Mixing of positional and named arguments: you can begin with positional arguments and then add some named arguments, but certain "inconvenient" cases are disallowed.
- Default arguments supported.
- No need to type `rec` to indicate that the function is recursive.
- Parameter and return types can be annotated.
  
Examples:
```
x = (p: int) => p + 1 

x(2)
// - int: 3

x = (p: int): int => {
	d = 2  // binding 2 to d

	if (p == 0)	-1 
  else p*d
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
make_rectangle = (x: float, y: float = 0.): float => {
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

g = (a, b) => {
	return a + b
}


h = (a, b = 1, c) => {
//
}

h(342, 42)


```


## 0.3 Primitive types and operators

We take ReasonML primitive datatypes (without `optional`).

```
Int	      x: int = 10;
Float	    x: float = 10.0;
Boolean	  x: bool = false;
String	  x: string = "ten";
Char	    x: char = 'c';
Tuple	    x: (int, string) = (10, "ten");
Array	    x: array(int) = [1, 2, 3];
Functions x: (int, int) => int = (a, b) => a + b;
Unit      x: (int, int): () => { // some side effect }
```

- Array literals uses the `[]` rather than `[||]`.
- Overloading of arithmetic operators for `int` and `float`.
- Adding of float works with `+` !

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

## 0.4 User defined types

We want to implement as much as possible of ReasonML's type system. For example, we have:

  - algebraic types;
  - recursive types.

Constructors start with a capital letter.

Types are always small letters.

For example, to define a `tree`, we use

```
type tree =
    | Leaf(age, dna)
    | Node(age, dna, tree, tree);
```

It is also possible to name the constructors:

```
type tree =
    | Leaf(age: age, states: dna)
    | Node(age: age, states: dna, left: tree, right: tree);

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

```
  x: probability = 0.5
  // okay

  x: probability = 1.2
  // not okay
```

## 0.5 Conditionals

Examples:

```
  y = 3

  // the following is an if-expression
  x = if (y > 5) true else false

  x
  // false

  // the following is an if-statement
  if (y < 5) {
    print(y)
  }
  // 3

  if (y < 5) print(y)
  // error
```

- We have both `if`-statements and `if`-expressions.

## 0.6 Pattern matching

Pattern matching works with `switch`. Example:

```
switch (child) {
        | Node => {
            observe 0 ~ Exponential(λ)
            simulate_tree(child.left, child, λ, μ)
            simulate_tree(child.right, child, λ, μ)
        }
    }
```

- We allow for named arguments/parameters, i.e. `child.left`.

## 0.7 Loops 

Loops are discouraged in most cases. Instead functional programming patterns like map, filter, (see Section 2) or reduce can usually be used in their place.

```
x = 1;
y = 5;

for (i in x to y) {
  print_int(i)
  print_string(" ")
};
/* Prints: 1 2 3 4 5 */
```

The reverse direction is also possible using the downto keyword:

```
for (i in y downto x) {
  print_int(i)
  print_string(" ")
}
/* Prints: 5 4 3 2 1 */
```

