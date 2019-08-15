# L

This project can be built and run using cargo.

**Syntax**

**Functions**

Functions are defined using the `fn` keyword.

Examples:

```
fn f = x: Int => y: Int : Int { x + y }

fn five() -> Int { 5 }

fn identity(x: 'a) -> 'a { x }
```

In general, the syntax is 


fn \<identifier> = p<sub>0</sub> : t<sub>0</sub> => ... => 
    p<sub>n</sub> : t<sub>n</sub> : t<sub>ret</sub> { ... },
    where p<sub>0</sub>...p<sub>n</sub> are the parameter names and
    t<sub>0</sub>...t<sub>n</sub>, t<sub>ret</sub> are the types.

If the function takes either a Unit parameter or one parameter it can be defined in the alternate syntax

fn \<identifier> () → t<sub>ret</sub> { ... } for Unit accepting functions (parameterless)
(Note: Parameterless functions *must* be declared using this syntax)

fn \<identifier> (p: t) → t<sub>ret</sub> { ... } for single parameter functions.

These functions are curried and support partial application.

```
fn add = x: Int => y: Int { x + y }
let add5 = add 5;
add5 6 ! // 11

```

Note that these functions do not have an explicit return statement. This 
is because blocks themselves are expressions and evaluate to the final
expression.

**Function Application**

Functions application is done via juxtaposition/space.

Examples:

```
f 5 7 // 12

five () // 5 (Note: This should be interpreted as applying the value *Unit* 
to the function five opposed to the interpretation of many languages that 
parenthese represent a call to a function) 

identity "identity" // identity
```

**Types**

Builtin types:
- Bool
- String
- Number (Can use Int an as alias, even though its a float64 not an Int...)
- Tuples (\<Type>, ..., \<Type>)
- Function/Arrow type: `<Type> -> <Type>` (Note: The -> type constructor is right 
associative. i.e. a -> b -> c -> d == a -> (b -> (c -> d))

Type parameters: Functions can use type parameters without explicit declaration, 
as seen in the identity function example. Type parameters must begin with a tick ('). 

Example with higher order functions

```
fn zipWith = f: 'a -> 'b -> 'c => xs: List<'a> => ys: List<'b> : List<'c> { ... }
```


**Expressions and statements**

Most things in L are expressions and evaluate to some value.
This can be supressed by adding a semicolon at the end of an expression to make
it an expression statement.
The result of an expression can be printed out by prefixing it with a bang (!).


**Data Declarations**

Data declarations are of the form

It is important to note that all Typenames must begin with uppercase letters!

```
data <Typename> = <Variant> { <Types> } { | <Variant> { <Types> } ;
```

Note: Type parameter declarations are required in data declarations unlike in
function definitions.

Examples

```
data List<'a> = Nil | Cons 'a List<'a>;

data Tree<'a> = Nil | Node Tree<'a> Tree<'a>;

data StrangePairList<'a, 'b>' = Nil | Cons 'a 'b StrangePairList<'b, 'a>;

data Boolean = F | T;


data Expr
     = Abstraction String Expr
     | Application Expr Expr
     | Variable String;
```

The data constructors can be partially applied and are 
represented as curried functions.

**If let expressions and match expressions**

To use the data types we declared above, and very useful construct is to 
use pattern matching.
We can match a pattern using the if let syntax as follows: 

```
data List<'a> = Nil | Cons 'a List<'a>;
let list = Cons 0 (Cons 1 Nil);

if let Cons x xs = list {
    x!
} else {
    "List is Nil" !
}
// Result: 0 
```

The general syntax is

The match construct is a syntactic sugar over chained if lets.
The example above can be rewritten using match. Match is particularly beneficial
when there are many branches.

```
data List<'a> = Nil | Cons 'a List<'a>;
let list = Cons 0 (Cons 1 Nil);

match xs {
    | Cons x xs -> { x! }
    | Nil -> { "List is Nil"! }
}

// Result: 0
```

The braces are not necessary in general. The reason they are placed there in the 
example is because an expression is expected after the arrow (->). However, a ! turns
the expression into a print statement so it is not syntactically correct without
the braces. 

If xs is a list of String then it is valid to move the bang to the outside 
as follows.

```
match xs {
    | Cons x xs -> x!
    | Nil -> "List is Nil"
} !

```

**Patterns**

In fact the if let and match expressions can be used to match any pattern.
The types of patterns are as follows:
- Literal pattern
- Constructor pattern
- Tuple pattern
- Identifier pattern
- Wildcard pattern


The constructor pattern is the one demonstrated above used to deconstruct
algebraic data types. This pattern is refutable.

The literal pattern is used to match against Strings, Booleans, and Numbers. 
This pattern is refutable.
Example: `let x = 5`. Let bindings match against patterns, this will be explored 
in a later section.

The identifier pattern binds an identifier to a value. This pattern is irrefutable.

The wildcard pattern is used as a placeholder to ignore values you don't care about.
Like the identifier pattern, it is irrefutable.

The tuple pattern can be used to destructure tuples. This pattern is refutable if
any of its subpatterns are refutable, or equivalently is irrefutable only if all 
of its subpatterns are irrefutable.

Example:

``` 
let (a,b,c) = (1,2,3);
a! b! c!
// Prints
// 1
// 2
// 3
```

**Variable declarations and let bindings**

Variables are declared using the `var` keyword. It has the simple syntax as follows:

`var <identifier> = <expr> ;`

Variables are mutable and can be reassigned.

```
var x = 0;
x = 100;
x !
// 100
```

Let bindings support pattern matching. One example is seen above in the tuple patterns 
section. Let bindings can only be applied to irrefutable patterns.
i.e. `let Cons x xs = ...` is not valid.

**Lambda Expressions**

To write functions inline, L supports lambda expressions.

Lanbdas are similar to fn definitions but are expressions instead of declarations
and thus can be used in places where fn definitions cannot.

Syntax:
```
let lambda = \p0: t0 => ... => \pn: tn => <expr>;
```

The syntax is very similar to function definitions except each parameter is preceded
by a backslash, the return type is not explicitly specified, and there is an 
arrow before the body.   
Also, as seen above lambdas evaluate to a function and thus can be assigned to variables.

Examples:
```
data List<'a> = Nil | Cons 'a List<'a>;

fn map = f: 'a -> 'b => xs: List<'a> : List<'b> {
    match xs {
        | Cons x xs -> Cons (f x) (map f xs)
        | Nil       -> Nil
    }
}

let xs = Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil)));
map (\x: Int => x * 2) xs !
// (Cons 0 (Cons 2 (Cons 4 (Cons 6 Nil))))

map (\x: Int => x % 2 == 0) xs !
// (Cons true (Cons false (Cons true (Cons false Nil))))
```

 








 
 