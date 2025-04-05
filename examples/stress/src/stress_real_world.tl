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
    #('error, _err) = e: e

funp example4_helper
  ({'token: _token}): #('ok, 0)
  (_): #('error, 'invalid_input)
  
fun example5(): #('GET, 'GET', #'GET')

fun example6(): <<>>
