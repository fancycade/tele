# Builtin Attributes

## import

```
import foobar(
  some_fun/1, 
  some_fun/2
)
```

[Read More](https://www.erlang.org/doc/system/modules.html#pre-defined-module-attributes)

## behaviour

```
behaviour(gen_server)
```

[Read More](https://www.erlang.org/doc/system/design_principles.html#behaviours)

## callback

```
callback foo(integer): integer
```

[Read More](https://www.erlang.org/doc/system/design_principles.html#behaviours)

## include_lib

```
include_lib("test/lib/test.hrl")
```

[Read More](https://www.erlang.org/doc/system/macros.html)

## nifs

```
nifs(foo/2, bar/3)
```

[Read More](https://www.erlang.org/doc/system/nif.html)

## doc

```
doc("this is a doc attribute")
```

[Read More](https://www.erlang.org/doc/system/documentation.html)

## moduledoc

```
moduledoc("this is a module doc.")
```

[Read More](https://www.erlang.org/doc/system/documentation.html)

## on_load

```
on_load(some_function/0)
```
[Read More](https://www.erlang.org/doc/system/code_loading.html#running-a-function-when-a-module-is-loaded)


