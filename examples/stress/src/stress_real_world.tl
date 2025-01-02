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
