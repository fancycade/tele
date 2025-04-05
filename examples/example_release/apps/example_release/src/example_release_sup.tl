behaviour(supervisor)

define SERVER: ?MODULE

fun start_link():
  supervisor.start_link(#('local, ?SERVER), ?MODULE, [])

fun init([]):
  sup_flags =  {'strategy: 'one_for_all,
                'intensity: 0,
				'period: 1}
  child_specs = []
  #('ok, #(sup_flags, child_specs))
