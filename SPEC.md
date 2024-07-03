# tele

An Erlang frontend. A programming language designed to be compiled to Erlang that is easier to read and write than Erlang.

The intent is that telelang has a 1 to 1 equivalence with Erlang's core semantics.

Inspired by [Mochi](https://github.com/i2y/mochi), [Rhombus](https://docs.racket-lang.org/rhombus/index.html), [Elixir](https://elixir-lang.org/), Haskell, OCaml, Python, Typescript, and Scheme.

Some basic design principles that tele follows are:

1. Tele is Erlang
2. Readability at the expense of writability
3. Use widely familiar concepts or syntax when possible
4. Minimal syntax with little amibiguity
5. No anglocentric syntax requirements (i.e. capital letters)

## Language

### Comments

C-Style comments

    // This is a line comment

    /* This is a multiline comment */

### Numbers

Numbers are the same as in Erlang

    1 2 3 4
    1.0 2.0 3.4

Char notation:

    $f
    $o
    $o

These are integers but look like chars.

Integer with a base value:

    base#value

For example a hex value:

    16#4865_316F_774F_6C64

Single underscores can be inserted between digits for visual separator.

Scientific notation:

    2.3e3
    2.3e-3

### Atoms

Atoms are like Scheme. They are prefixed with a '

    'hello

EXPERIMENTAL:

Optionally end the prefix with another tick:

    'hello'

This is mostly useful for when you put a space in an atom:

    'hello world'

This is valid in Erlang, but might not be in Tele. Instead use binary_to_atom("hello world").

### Bit Strings

    <<10, 20>>
    <<"ABC">>
    <<1:1, 0:1>>

    Bit strings that consist of a number of bits that are evenly divisible by eight

### Binaries

Binaries are like Elixir

    "foobar"

This is equivalent to <<"foobar"/utf8>> in Erlang.

### Tuples

Tuples are delimited by an opening `#(` and a closing paren `)`.

    #(1, 2)

Taken from gleam's tuples.

### Lists

Lists are the same as in Erlang

    [1, 2, 3]

Similar to Erlang, Javascript, and Python.

cons operator for list is same as in Erlang.

    l = [2, 3]
    [1 | l]

This also includes cons multiple values like Erlang.

    l = []
    [1, 2, 3 | l]

### Maps

Maps are delimited with '{' and '}': 

    {"foo": "bar"}

This makes it similar to Python or Javascript map syntax.

Tele can destructure maps like this:

    m = {"foo": "bar"}
    {"foo": b} = m
    ?assertEqual(b, "bar")

EXPERIMENTAL:

Erlang has map update syntax like this:

    M = #{},
    M#{<<"foo">> => <<"bar">>}.

Tele could do map update syntax like this:

    m = {}
    {m | "foo": "bar"}

This is similar to Elm. However, Erlang already has the cons operator, would make sense to make it similar:

    {"foo": "bar" | m}

The downside of this is that it can't be pattern matched like the list cons operator can. Which makes it not like
the list cons operator and misleading.

Keeping the value at the front of the expression makes it clearer that there is a map update happening. This map
update syntax is not too far from the Erlang syntax.

    M#{<<"foo">> => <<"bar">>}

### Anonymous Functions

Anonymous functions or "arrow" functions like they are called in JS.

Anonymous functions can be defined on a single line.

    (x, y) => x + y

    map((x) => x + 2, [1, 2, 3])

Anonymous functions can be multiple lines. Anonymous function bodies follow the same syntax rules as blocks.
Function bodies starting on same line have to be one line. Multiline bodies must be after the beginning of the block.

    map(
      (x) =>
        x2 = x + 2
        x2 + 2, 
      [1, 2, 3]
    )

    f = (x, y) =>
          z = x + y
          z + 42

    f2 = (x, y) => f(x, y)

Passing functions as values requires similar syntax as Erlang and Elixir:

    #foo/2

This is to tell which function is being used by function arity.

### Variables

Variables are lower case words with underscores also being allowed.

    a_var = "foobar"
    b_var = 12

### Records

Instantiating a record.

    record thing: #(a, b)

    a = #thing(a=12, b=43)
    b = #thing(12, 43)

Accessing a field from a record:

    a#thing.a
    a#thing.b

The record type is required.

EXPERIMENTAL:

Define records with type syntax. They are distinguished from tuples by having named fields.

    type thing: #(a=int, b=int)

See if this type of syntax is possible.

    a.a
    a.b

Would run into issues where record type is not known.

This could be solved with structural typing. The record type can be inferred by what field's are used.
Or if it is ambiguous it doesn't matter because the correct field is used semantically anyways.
The problem with structural typing with records is that in Erlang the record includes the first element
atom as the record type name. Structural typing would have to ignore that. This might be okay since records
are actually tuples anyways.

Type annotations can be used to determine what type is actually used.

This feature would need to know the current record types in scope for that module. When including
erlang header files these would need to be parsed by the tele compiler. This could be similar to how
Zig parses C header files.

### Pattern Matching

    match v:
      [1, 2, x]: x
      [1, 2, x, y]: {x, y}

    match v:
      {'foo: "bar"}: #('ok, "result")
      {'bar: bar}: bar

    match v:
      2:
        #('ok, 'done)
      3:
        ok = 'ok
        #(ok, 'failure)

EXPERIMENTAL:

Type annotation syntax in pattern matching that can be translated to guards. Inspired by Rhombus.

    match v:
      x :: int: x + 2
      x :: float: x + 2.0

The double semicolon is like an inline type annotation.

    match v:
      {_, x} :: {any, int}: x + 2
      _: {'error, 'oops}

### Function Definitions

    fun add(x, y): x + y

    fun add(x, y):
      x + y

Pattern matching with a function definition.

    fun foo(x, [1, 2]): x
    fun foo(_x, b): b


EXPERIMENTAL:

Type annotation syntax in pattern matching that can be translated to guards. Inspired by Rhombus.

    fun foo:
      (x :: int): x
      (y :: float): y

### Modules

Functions of modules are called with the . syntax

    io.format("foobar", [])

modules are defined by creating a file and using the name of the file.

A file, `basic.tl`, would be the `basic` module.  

    fun hello():
      io.format("Hello, World!")

    fun foobar(x, y):
      2 + 2

Functions by default are exported. To prevent this make a private function

    funp foobar(): 2 + 2

The module might look like this

    fun add4(x):
      add2(add2(x))

    funp add2(x):
      x + 2

EXPERIMENTAL:

Make multiple modules inside of a file.

    module foobar:

      fun foo(a, b):
        a + b

### Type Specifications 

These are equivalent to Erlang function specs.

Type specifications can to apply defined functions function.

    spec add(int, int): int
    fun add(x, y):
      x + y

    spec add2(int): int
    fun add2(x):
      x + 2

Ending parenthesis on a type specification are completely optional.

    spec add(int(), int()): int()

OR

    spec add(int, int): int

The tele compiler will be able to do this automatically.

Here is a full module with type signatures and sum types

    spec add(int, int): int 
    fun add(x, y):
      x + y

    spec add2(int): int
    fun add2(x):
      x + 2

    spec div(int, int):
      #('ok, int) | #('error, binary)
    fun div:
      (_, 0): #('error, 'div_by_zero)
      (a, b): #('ok, a / b)

Type Annotations of anonymous functions look like this:

    (int) => int

NOT

    (int): int

Despite the second looking like a function definition, this is because it makes the type annotation clearer.

    fun foo((int): int): (int): int

The above example is difficult to mentally parse.

Defining a function that accepts a function as argument and returns one looks like this:

    spec foo((int) => int): (int) => int
    fun foo(f):
      (n) => f(n)

EXPERIMENTAL:

Records with type annotations look like this:

    type foo: #(a: int, b: int)

### Type Aliases

Make a new type with a type alias

    type foo: int
    type id: int
    type options: 'foo | 'bar | 'baz

### Behaviours

Erlang has the concept of behaviours which are like module interfaces.

defining a behaviour is as simple as defining a module with callbacks.

    callback up(#(int, int)): #(int, int)
    callback down(#(int, int)): #(int, int)
    callback left(#(int, int)): #(int, int)
    callback right(#(int, int)): #(int, int)

To make a module adhere to a behaviour:

    behaviour cursor
     
    spec up(#(int, int)): #(int, int)
    fun up(#(x, y)): 
      #(x, y - 1)
    
    spec down(#(int, int)): #(int, int)
    fun down(#(x, y)):
      #(x, y + 1)
    
    spec left(#(int, int)): #(int, int)
    fun left(#(x, y)):
      #(x - 1, y)
    
    spec right(#(int, int)): #(int, int)
    fun right(#(x, y)):
      #(x + 1, y)

### Module Aliases

EXPERIMENTAL:

alias attribute to make it easier working with long module names.

    alias lm: long_module_name

This could be used for other things as well. Simple string substitution like erlang macros.

This doesn't have a direct correlation with erlang and so it would be handled at compile time.

### Module Constants

EXPERIMENTAL:

Elixir has constants like:

    @pi 3.14

Erlang version would be:

    -compile({inline, [pi/0]}).
    pi() -> 3.14.

Tele might look like:

    fun pi: 3.14

Notice no function signature.

### Try Catch Statements

Erlang can handle exceptions with try/catch statements. The syntax for this in tele is similar to match statements.

    try 42 / 0:
       result: result
    catch:
       _.exception:
         'oops
