# Builtin Attributes

## import

```
import foobar(
  some_fun/1, 
  some_fun/2
)
```

## behaviour

```
behaviour(gen_server)
```

## callback

```
callback foo(integer): integer
```

## include

```
include("test/lib/test.hrl")
```

## include_lib

```
include_lib("test/lib/test.hrl")
```

## export_type

```
export_type(id/0)
```

## nifs

```
nifs(foo/2, bar/3)
```

## doc

```
doc("this is a doc attribute")
```

## moduledoc

```
moduledoc("this is a module doc.")
```

## onload

```
onload("something")
```


