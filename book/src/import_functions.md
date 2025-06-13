# Importing Functions

The easiest way to use a function from a module is simply use the `module.function` syntax.

Sometimes the name of the module might be long and used in several places. Let's say we were working on a web app using the `nine` library.
We might be calling the `json_response` function a lot.

Our module might look like this:
```
fun handle_get_todos({'req: req} = c):
  todos = [{"id": 1, body": "Go to the store"},
           {"id": 2, body": "Wash the dishes"}]
  nine_cowboy_util.json_response(200, todos, req)
```

Perhaps this gets tiring after a while so we can simply import the function:

```
import nine_cowboy_util(json_response/3)
```

Then we can rewrite our `handle_get_todos` function like this:

```
fun handle_get_todos({'req: req} = c):
  todos = [{"id": 1, "body": "Go to the store"},
           {"id": 2, "body": "Wash the dishes"}]
  json_response(200, todos, req)
```

See how we didn't need to use the module prefix for the function call.
