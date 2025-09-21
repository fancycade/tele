/* Collection of code snippets that failed in real world testing */

fun example1
  (#('send_async, _msg), state):
    #('noreply, state)
  (_msg, state):
    #('noreply, state)


fun example2(msg, state):
  case msg:
    _: 'ok
  #('noreply, state)

fun example3(n):
  case example_helper(n):
    #('ok, v):
      v
    #('error, _err) = e:
      e

funp example_helper
  ('true): #('ok, 42)
  ('false): #('error, 'not_found)

fun example4():
  case example4_helper({'token: "foo"}):
    #('ok, v): #('ok, v + 2)

funp example4_helper
  ({'token: _token}): #('ok, 0)
  
fun example5(): #('GET, #'GET')

fun example6(): <<>>

fun example7
  (x) when x == 'foo
      when x == 'bar:
    42
  (_): 43

fun example8():
  a = [#('mods, [#()])]
  b = [#('mods, [#('simple_app, [])])]
  #(a, b)                

fun example9
  (label, 'undefined):
    example9_helper("INSERT INTO nodes (body) VALUES (?)", [label])
  (label, parent_id):
    example9_helper("INSERT INTO nodes (body, parent_id) VALUES (?, ?)", [label, parent_id])

funp example9_helper(_sql, args): args

fun example10(uri):
  case uri_string.parse(uri):
    {'scheme: scheme,
     'host: host}: 
      #('ok, scheme, host)
    _:
      #('error, 'missing_scheme_or_host)

fun example11():
  fun lists.map/2
