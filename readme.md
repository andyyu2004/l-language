**Syntax**

**Functions**

Functions are defined using the `fn` keyword.

Examples:

`fn f = x: Int => y: Int : Int { x + y }`

`fn five() -> Int { 5 }`

`fn identity(x: 'a) -> 'a { x }`

In general, the syntax is 

fn \[fname] = p<sub>0</sub> : t<sub>0</sub> => ... => 
    p<sub>n</sub> : t<sub>n</sub> : \[t<sub>ret</sub>] { ... },
    where p<sub>0</sub>...p<sub>n</sub> are the parameter names and
    t<sub>0</sub>...t<sub>n</sub>, t<sub>ret</sub> are the types.

If the function takes none (Unit) or one parameter it can be defined in the alternate syntax

fn \[fname] () → t<sub>ret</sub> { ... } for Unit accepting functions (parameterless)
(Note: This is the only way to define parameterless functions)

fn \[fname] (p: t) → t<sub>ret</sub> { ... } for single parameter functions.

These functions are curried and support partial application.

**Function Application**

Functions application is done via juxtaposition/space.

Examples:

`f 5 7 // 12`

`five () // 5`

`identity "identity" // identity` 

**Types:**   



 