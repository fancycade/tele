# tele

An Erlang frontend. A programming language designed to be compiled to Erlang that is easier to read and write than Erlang.

This means that Telelang must have a 1 to 1 feature equivalent feature with Erlang.

Inspired by [Mochi](https://github.com/i2y/mochi), Rhombus, Elixir, Haskell, OCaml, Python, Typescript, and Scheme.

## Language

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

Optionally end the prefix with another tick:

    'hello'

This is mostly useful for when you put a space in an atom:

    'hello world'

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

You won't need to declare syntax for PID. Is represented like Erlang:

    <0.54.0>

### Tuples

Tuples are delimited with curly brackets and comma. The parens are required. 
    
    {1, 2}

A single element tuple can be defined as:

    {1}

### Lists

Lists are the same as in Erlang

    [1, 2, 3]

### Maps

Maps are prefixed with #: 

    #{"foo": "bar"}

### Anonymous Functions

Anonymous functions can be defined on a single line.

    (x, y) -> x + y

    map((x) -> x + 2, [1, 2, 3])

Anonymous functions can be multiple lines. Here we delimit the end of the function expression with the comma of the map function arguements.

    map((x) ->
      x2 = 2
      x2 + 2,
      [1, 2, 3]
    )

### Variables

Variables are lower case words

    a_var = "foobar"
    b_var = 12

### Records

Instantiating a record.

    type thing: #(a, b)

    a = #thing(a=12, b=43)
    b = #thing(12, 43)

Accessing a field from a record:

    a.a
    a.b

In some cases it may be necessary to specify what record type is used when accessing a field:

    a#thing.a
    a#thing.b

### Pattern Matching

    match v
      | [1, 2, x]: x
      | [1, 2, x, y]: {x, y}

    match v
      | #{'foo: "bar"}: {'ok, "result"}
      | #{'bar: bar}: bar

EXPERIMENTAL:

Type annotation syntax in pattern matching that can be translated to guards. Inspired by Rhombus.

    match v
      | x :: int: x + 2
      | x :: float: x + 2.0

The double semicolon is like an inline type annotation.

    match v
      | {_, x} :: {any, int}: x + 2
      | _: {'error, 'oops}

### Function Definitions

    def add(x, y): x + y

    def add(x, y):
      x + y

Pattern matching with a function definition.

    def foo | (x, [1, 2]): x
            | (_x, b): b

The difference with the match syntax is it requires a surrounding parens on each case to signify the function signature.

EXPERIMENTAL:

Type annotation syntax in pattern matching that can be translated to guards. Inspired by Rhombus.

    def foo
      | (x :: int): x
      | (y :: float): y

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

    def add(int): int
    def add2(x):
      x + 2

    def div(int, int):
      {'ok, int} | {'error, binary}
    def div
      | (_, 0): {'error, 'div_by_zero}
      | (a, b): {'ok, a / b}

Type Annotations of anonymous functions look like this:

    (int) -> int

NOT

    (int): int

Despite the second looking like a function definition, because it makes the type annotation clearer

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

    callback def up({int, int}): {int, int}
    callback def down({int, int}): {int, int}
    callback def left({int, int}): {int, int}
    callback def right({int, int}): {int, int}

TODO:

Determine if possible to get rid of callback keyword by function signature without implementation.

To make a module adhere to a behaviour:

    behaviour: cursor 
     
    def up({int, int}): {int, int}
    def up({x, y}): 
      {x, y - 1}
    
    def down({int, int}): {int, int}
    def down({x, y}):
      {x, y + 1}
    
    def left({int, int}): {int, int}
    def left({x, y}):
      {x - 1, y}
    
    def right({int, int}): {int, int}
    def right({x, y}):
      {x + 1, y}

