# tele

An Erlang frontend. A programming language designed to be compiled to Erlang that is easier to read and write than Erlang.

The intent is that telelang has a 1 to 1 equivalence with Erlang's core semantics.

Inspired by [Mochi](https://github.com/i2y/mochi), [Rhombus](https://docs.racket-lang.org/rhombus/index.html), [Elixir](https://elixir-lang.org/), Haskell, OCaml, Python, Typescript, and Scheme.

Some basic design principles that tele follows are:

1. Tele is Erlang
2. Use widely familiar concepts or syntax when possible
3. Minimal syntax with nonambiguous consistent use
4. No anglocentric syntax requirements (i.e. capital letters)
5. Syntax similarity corresponds to semantic similarity

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

tele's atom syntax is similar to LISP. They are prefixed with a '. Here is an example:

    'hello

EXPERIMENTAL:

Erlang supports atom's with special characters such as spaces.

    'hello world'

Single quotes are an alternate syntax for atom's. Tele's apostrophe syntax cannot support the case of an atom with spaces.
Instead tele also has an alternate syntax for atom's. Prefixing single quote atom's with a # means we want a single quote delimited
atom. The above Erlang example would look like this in Tele:

    #'hello world'

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

Erlang has map update syntax like this:

    M = #{},
    M#{<<"foo">> => <<"bar">>}.

Tele could do map update syntax like this:

    m = {}
    {m | "foo": "bar"}

This was inspired by Elm's record update syntax.

Keeping the value at the front of the expression makes it clearer that there is a map update happening. This map
update syntax is not too far from the Erlang syntax.

    M#{<<"foo">> => <<"bar">>}

### Anonymous Functions

Anonymous functions look like function definitions with no names. Hence anonymous.

    fun (x, y):
      x + y

Anonymous function can be defined on a single line.

    fun (x, y): x + y

More examples:

    lists.map(
      fun (x):
        x2 = x + 2
        x2 + 2,
      [1, 2, 3]
    )

    f = fun (x, y):
          z = x + y
          z + 42

    f2 = fun (x, y): f(x, y)

Passing functions as values requires similar syntax as Erlang:

    fun foo/2

This is to tell which function is being used by signature.

#### Pattern matching with anonymous functions

    fun (0): 1
        (n): n + 2

Or start the signature on the newline after fun:

    fun
      (0): 1
      (n): n + 2



### Variables

Variables are lower case words with underscores also being allowed.

    a_var = "foobar"
    b_var = 12

#### Calling Variables

Since the syntax for a variable and a function call can be ambiguous. There is a special operator
to tell the tele compiler that this module or function name should be an Erlang variable.

    fun apply(m, f):
      @m.@f()

Results in the equivalent Erlang code:

    apply(M, F) ->
      M:F().

### Records

Defining a record looks like this:

    record thing: #(a, b)

To specify optional values for the keys:

   record thing: #(a = 42, b)

To specify the types of the keys:

   record thing:
     #(a = 42: integer(), b: integer())

Instantiating a record.

    a = #thing(a=12, b=43)
    b = #thing(12, 43)

Accessing a field from a record:

    a#thing.a
    a#thing.b

The record type is required.

### Pattern Matching

Case expressions are very similar to Erlang:

    case v:
      [1, 2, x]: x
      [1, 2, x, y]: {x, y}

    case v:
      {'foo: "bar"}: #('ok, "result")
      {'bar: bar}: bar

    case v:
      2:
        #('ok, 'done)
      3:
        ok = 'ok
        #(ok, 'failure)

#### Receive Expressions

Erlang has a builtin keyword receive that is meant to be used for handling messages sent to a process. In Tele,
it is like a case statement without a variable to match on. Here is an example:

    receive:
      #('msg, msg):
        'ok
      _:
        #('error, 'unhandled_message)

### Try Catch Statements

Erlang can handle exceptions with try/catch statements. The syntax for this in tele is similar to match statements.

    try 42 / 0:
       result: result
    catch:
       @e.@exception:
         #('error, #(@e, @exception))

### Function Definitions

    fun add(x, y): x + y

    fun add(x, y):
      x + y

Functions are by default exported by the module. To turn this implicit behaviour off, use `funp`.

    funp add2(x): x + 2

This function will only be accessible in the module it is declared in.

Pattern matching with a function definition.

    fun foo(x, [1, 2]): x
           (_x, b): b

There are two benefits to this over having fun for each pattern match like Elixir. The first reason is that we don't have
the possibility of mixing fun and funp. The first fun defines if it is public or private. The second reason is that it mirrors match syntax as well as spec overloading.

    doc """
    Handle code in ways you would never understand
    """
    spec foo (any(), 'thing): any()
             ('stuff, any()): any()
    fun foo (x, 'thing): x
            ('stuff, x): x

    spec factorial(integer()): integer()
    fun factorial(x): factorial(x, 1)

    spec factorial (0, integer()): integer()
                   (integer(), integer()): integer()
    fun factorial (0, acc): acc
                  (n, acc): factorial(n - 1, acc * n)

We can see that this is similar to the case syntax:

    spec foobar (any(), 'thing): any()
                ('stuff, any()): any()
    fun foobar(y, z):
      case #(y, z):
        #(x, 'thing): x
        #('stuff, x): x

### Modules`

Functions of modules are called with the . syntax

    io.format("foobar", [])

modules are defined by creating a file and using the name of the file.

A file, `basic.tl`, would be the `basic` module.  

    fun hello():
      io.format("Hello, World!")

    fun foobar(x, y):
      2 + 2

### Module Attributes

Inside attributes, besides `define`, any fun vals are converted to `name/arity` syntax instead of prefixed with fun.

For example:

    nifs([fun hello/2])

Becomes:

    -nifs([hello/2]).

The reason for this is to unify the syntaxes for function values instead of having a special syntax for attributes but not expressions.

## import

Tele equivalent of this Erlang code:

    -import(foo_mod, [do_thing/1, do_thing2/3]).

Would be:

    import foo_mod(
      do_thing/1, 
      do_thing2/3
    )

## on_load

    on_load(init_info/2)

## nifs

    nifs([native_call/2, native_other_call/3])

## export_type

    export_type([some_type/0])

## behaviour

    behaviour(gen_server)

## include

    include("include/some/header.hrl")

## include_lib

    include_lib("kernel/include/file.htl")

## doc

    doc("this is a documentation")

    doc("""
    This is a multiline
    comment that goes on and on...
    """)

## moduledoc

    moduledoc("this is module doc")
    
    moduledoc("""
    this is a multiline module doc
    """)


## callback

    callback do_thing(integer): integer

## define

    define NUMBER: 42
    define MACRO1(x, y):
      #('a, x, 'b, y)

## Not Supported

- module
- export
- export_all
- compile
- feature
- vsn
- file

## Custom Attributes

If an erlang attribute is not supported by a builtin tele attribute you can use the `attr` prefix to specify an attribute.

    attr compile('inline)

Compiles into:

    -compile(inline).

### Types

#### Type Aliases

Make a new type with a type alias

    type foo(): integer()
    type id(): integer()
    type options(): 'foo | 'bar | 'baz

Opaque type aliases are:

    opaque foo(): integer()

Unlike Erlang, parentheses for types are optional.

Both of the syntaxes below are valid.

    type id: integer()
    type id: integer

#### Type Variables

To disambiguate type variables use the @ prefix.

    type orddict(@k, @v): [#(@k, @v)]

#### Function Types

Defining a function that accepts any arity:

    fun (...): any()

Any function:

    fun ()

Function that takes no arguments and returns an integer:

    fun (): integer()

Function that takes any number of arguments and returns an integer:

    fun (...): integer()

Function that takes two integers and returns an integer:

    fun (integer(), integer()): integer()

Overloaded function signatures:

    fun (integer(), integer()): integer()
        (binary(), binary()): binary()

#### Function Specifications 

These are equivalent to Erlang function specs.

Type specifications can apply to function definitions.

    spec add(integer, integer): integer
    fun add(x, y):
      x + y

    spec add2(integer): integer
    fun add2(x):
      x + 2

The syntax for overloading a function specification is similar to match syntax:

    spec add (integer, integer): integer
             (list, list): list
    fun add(a, b) when is_list(a) andalso is_list(b):
      a ++ b
    fun add(a, b) when is_integer(a) andalso v is_integer(b):
      a + b

Here is a full module with type signatures and sum types

    spec add(integer, integer): integer
    fun add(x, y):
      x + y

    spec add2(integer): integer
    fun add2(x):
      x + 2

    spec div(integer, integer):
      #('ok, integer) | #('error, binary)
    fun div
      (_, 0): #('error, 'div_by_zero)
      (a, b): #('ok, a / b)

#### Overloading Function Specifications

    spec foo(pos_integer): pos_integer
            (integer): integer

#### Function Specification Type Variables

Type variables can be used to specify the relationship between input and output.

    spec id(@x): @x

These types can be constrained by guard like subtype constraints:

    spec id(@x): @x when @x :: tuple

The @ prefix is used to disambiguate the type variable from a normal type.

Using a type variable in a function specification is done like this:

    spec foobar(@req, @env): #('ok, @req, @env) | #('error, any) 
      when @req :: cowboy_req.req,
           @env :: cowboy_middleware.env
    fun foobar(req, env):
      #(ok, req, env)

    spec foobar(x :: integer, y :: integer)

An anonymous type variable can be specified with `_`:

    spec foobar(string, _): string

### Behaviours

Erlang has the concept of behaviours which are like module interfaces.

defining a behaviour is as simple as defining a module with callbacks.

    type point: #(integer, integer)

    callback up(point): point 
    callback down(point): point
    callback left(point): point
    callback right(point): point 

To make a module adhere to a behaviour:

    behaviour cursor

    type point: #(integer, integer)
     
    spec up(point): point
    fun up(#(x, y)): 
      #(x, y - 1)
    
    spec down(point): point
    fun down(#(x, y)):
      #(x, y + 1)
    
    spec left(point): point
    fun left(#(x, y)):
      #(x - 1, y)
    
    spec right(point): point
    fun right(#(x, y)):
      #(x + 1, y)

### Tests

EXPERIMENTAL:

Defining eunit tests in Erlang looks like this:

    foobar_test() ->
        ?assertEqual(2, foobar(1, 1)).

In tele we can use block syntax:

    test foobar:
      ?assertEqual(2, foobar(1, 1))

It would be nice if we can place unit tests throughout our source code like Zig. tele can organize all test cases into a section of the generated Erlang module. Another automatic option is to import the eunit library.
