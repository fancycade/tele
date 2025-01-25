behaviour(application)

fun start(_start_type, _start_args):
  example_release_sup.start_link()

fun stop(_state):
  'ok
