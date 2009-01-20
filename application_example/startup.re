module Startup
  def initialize
    todos = [{~what: 'buy milk', ~when: ~today}, {~what: 'call parents', ~when: ~tomorrow}, {~what: 'visit dentist', ~when: ~later}]
    ets::new(~mocks, [~named_table])
    ets::insert(~mocks, (~todos, todos))
