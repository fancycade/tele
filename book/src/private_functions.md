# Private Functions

Sometimes when you make a module you don't want to export internal functions for a variety of reasons.

To prevent a function from being exported simply use `funp`. To see this in action lets continue our web app example.

We will make a private function to get our list of todos.

```
fun handle_get_todos({'req: req} = c):
  todos = get_todos()
  json_response(200, todos, req)

funp get_todos():
  [{"id": 1, "body": "Go to the grocery store"},
   {"id": 2, "body": "Wash the dishes"}]
```

In this example we need to export `handle_get_todos` so it can be used by the router to serve web requests. However, we don't need to
export our `get_todos` method as that can be internal to the module.
