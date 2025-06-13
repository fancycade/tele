# Case

```
case some_function():
  1: 'one
  2: 'two
  3: 'three
  _: 'undefined
```

The equivalent to an if statement in Tele is something like this:
```
case 1 =:= 1:
  'true: 'ok
  'false: #('error, 'math_broken)
```
