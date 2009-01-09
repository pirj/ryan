class Todo
  def initialize
    @todos = [{~what: 'buy milk', ~when: ~today}, {~what: 'call parents', ~when: ~tomorrow}, {~what: 'visit dentist', ~when: ~later}]

  def index(parameters, cookies)
    bindings = {}.insert(~todos, @todos)
    ('todo/index', bindings)

  def today(parameters, cookies)
    for_range(~today)

  def tomorrow(parameters, cookies)
    for_range(~tomorrow)

  def few_days(parameters, cookies)
    for_range(~few_days)

  def later(parameters, cookies)
    for_range(~later)

  def for_range(range)
    todos = @todos.filter{|t| t[~when]==range}
    ('todo/list', {}.insert(~todos, todos))

  def show(parameters, cookies)
    id = parameters[~id]
    if(id == nil)
      ''
    else
      apple = @apples[id.to_int()]
      ('fruits/show', {}.insert(~apple, apple))
