class Todo
  def initialize
    @todos = [{~what: 'buy milk', ~when: ~today}, {~what: 'call parents', ~when: ~tomorrow}, {~what: 'visit dentist', ~when: ~later}]

  def index(parameters, cookies)
    bindings = {}.insert(~todos, @todos)
    handlers = [('#add', ~append, ~todos, '/app/todo/add'),
    ('#today', ~update, ~todos, '/app/todo/today', true),
    ('#tomorrow', ~update, ~todos, '/app/todo/tomorrow', true)]
    ('todo/index', bindings, handlers)

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

  def add(parameters, cookies)
    'something added<br/>'

  def show(parameters, cookies)
    id = parameters[~id]
    if(id == nil)
      ''
    else
      apple = @apples[id.to_int()]
      ('fruits/show', {}.insert(~apple, apple))
