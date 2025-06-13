# Try/Catch

In Tele there is a typical convention of returning a tuple as a result to signify an error or not. This is a bit like
the Result type in Rust.

For example some function `foobar`. Might return a result like this:

```
#('ok, v) = foobar()
```

OR

```
#('error, err) = foobar()
```

However, there are times when actual exceptions can happen. There are times when we might want to catch those exceptions for a variety of reasons.
For that we use a try/catch expression. A try must be followed by a catch. An example might look like this:

```
try maybe_result():
  #('ok, _v) = foo: foo
catch:
  _._: #('error, 'oops)
```

Notice that the body of the try is pattern matching on the successful result of `maybe_result`. However we could match on other results:

```
try maybe_result():
  #('ok, 42): #('ok, 'right_number)
  #('ok, 43): #('ok, 'almost_right_number)
  _: #('error, 'undefined)
```

Here we are matching on an ok tuple that contains the number 42, or the number 43. If nothing matches we return an error tuple with the `'undefined` atom.
This works exactly like a case statement.

The `catch` expression works in a similar way except it matches on exceptions.

TODO: Describe exceptions in more detail
