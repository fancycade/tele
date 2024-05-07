# tele

An Erlang frontend. A programming language designed to be compiled to Erlang that is easier to read and write than Erlang.

This means that Telelang must have a 1 to 1 feature equivalent feature with Erlang.

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

### PID

You won't need to declare syntax for PID. It is represented like it is in Erlang:

    <0.54.0>

### Tuples

Tuples are delimited by an opening `#(` and a closing paren `)`.

    #(1, 2)

Taken from gleam's tuples.

### Lists

Lists are the same as in Erlang

    [1, 2, 3]

Similar to Erlang, Javascript, and Python.

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
    {m, "foo": "bar"}

The updated map must be the first element of the new map.

### Anonymous Functions

Anonymous functions can be defined on a single line.

    (x, y) -> x + y

    map((x) -> x + 2, [1, 2, 3])

Anonymous functions can be multiple lines. Anonymous function bodies follow the same syntax rules as blocks.
Function bodies starting on same line have to be one line. Multiline bodies must be after the beginning of the block.

    map(
      (x) ->
        x2 = x + 2
        x2 + 2, 
      [1, 2, 3]
    )

    f = (x, y) ->
          z = x + y
          z + 42

    f2 = (x, y) -> f(x, y)

### Variables

Variables are lower case words

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

    def add(x, y): x + y

    def add(x, y):
      x + y

Pattern matching with a function definition.

    def foo:
      (x, [1, 2]): x
      (_x, b): b

The difference with the match syntax is it requires a surrounding parens on each case to signify the function signature.

EXPERIMENTAL:

Type annotation syntax in pattern matching that can be translated to guards. Inspired by Rhombus.

    def foo:
      (x :: int): x
      (y :: float): y

### Modules

Functions of modules are called with the . syntax

    io.format("foobar", [])

modules are defined by creating a file and using the name of the file.

A file, `basic.tl`, would be the `basic` module.  

    def hello():
      io.format("Hello, World!")

    def foobar(x, y):
      2 + 2

Functions by default are exported. To prevent this make a private function

    defp foobar(): 2 + 2

The module might look like this

    def add4(x):
      add2(add2(x))

    defp add2(x):
      x + 2

EXPERIMENTAL:

Make multiple modules inside of a file.

    module foobar:

      def foo(a, b):
        a + b

### Type Annotations

These are equivalent to Erlang function specs.

Type annotations can go on function signatures.

    def add(int, int): int
    def add(x, y):
      x + y

    def add2(int): int
    def add2(x):
      x + 2

Here is a full module with type signatures and sum types

    def add(int, int): int 
    def add(x, y):
      x + y

    def add2(int): int
    def add2(x):
      x + 2

    def div(int, int):
      #('ok, int) | #('error, binary)
    def div:
      (_, 0): #('error, 'div_by_zero)
      (a, b): #('ok, a / b)

Type Annotations of anonymous functions look like this:

    (int) -> int

NOT

    (int): int

Despite the second looking like a function definition, this is because it makes the type annotation clearer.

    def foo((int): int): (int): int

Is not visually pleasing.

Defining a function that accepts a function as argument and returns one looks like this:

    def foo((int) -> int): (int) -> int
    def foo(f):
      (n) -> f(n)

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

    callback def up(#(int, int)): #(int, int)
    callback def down(#(int, int)): #(int, int)
    callback def left(#(int, int)): #(int, int)
    callback def right(#(int, int)): #(int, int)

TODO:

Determine if possible to get rid of callback keyword by function signature without implementation.

To make a module adhere to a behaviour:

    behaviour: cursor 
     
    def up(#(int, int)): #(int, int)
    def up(#(x, y)): 
      #(x, y - 1)
    
    def down(#(int, int)): #(int, int)
    def down(#(x, y)):
      #(x, y + 1)
    
    def left(#(int, int)): #(int, int)
    def left(#(x, y)):
      #(x - 1, y)
    
    def right(#(int, int)): #(int, int)
    def right(#(x, y)):
      #(x + 1, y)

### Module Aliases

EXPERIMENTAL:

alias attribute to make it easier working with long module names.

    alias lm: long_module_name

This could be used for other things as well. Simple string substitution like erlang macros.

This doesn't have a direct correlation with erlang and so it would be handled at compile time.
