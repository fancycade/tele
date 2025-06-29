# Header Files

Header files are useful for reusable types, records, and macros.

Header files will normally go into the `include` directory.

Tele header files use the `.htl` extension. Tele supports including header files with this extension.

NOTE:
If you are using a header file written in Tele in an Erlang module make sure to use the `.hrl` extension.

An example header file might looks like this:

```
record point: #(x, y)

type ids: list(integer)

define PI: 3.14
```

Only certain statements like records, types, and macros are supported in header files.
