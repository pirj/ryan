class Todo < Controller
  def index
    data = get_data()
    bindings = {}.insert(~todos, get_data())
    handlers = [{~id: '#add_new', ~command: ~update, ~what: ~todo_new, ~url: '/app/todo/add_new', ~fade: true},
    {~id: '#today', ~command: ~update, ~what: ~todos, ~url: '/app/todo/today'},
    {~id: '#tomorrow', ~command: ~update, ~what: ~todos, ~url: '/app/todo/tomorrow'}]
    ('todo/index', bindings, handlers)

  def today
    @session.set(~current_day, ~today)
    for_range(~today)

  def tomorrow
    @session.set(~current_day, ~tomorrow)
    for_range(~tomorrow)

  def for_range(range)
    data = get_data()
    todos = data.filter{|t| t[~when]==range}
    ('todo/list', {}.insert(~todos, todos))

  def add_new
    handlers = [{~id: '#add', ~command: ~prepend, ~what: ~todos, ~url: '/app/todo/add_todo'},
    {~id: '#add', ~command: ~empty, ~what: ~todo_new, ~fade: true},
    {~id: '#cancel', ~command: ~empty, ~what: ~todo_new, ~fade: true}]
    ('todo/new', {}, handlers)

  def add_todo
    day = @session.get(~current_day)
    todos = @session.get(~todo_data).unshift({}.insert(~what, 'something').insert(~when, day))
    @session.set(~todo_data, todos)
    'something<br/>'

  def get_data
    initial_todos = [{~what: 'buy milk', ~when: ~today}, {~what: 'call parents', ~when: ~tomorrow}, {~what: 'visit dentist', ~when: ~later}]
    @session.set(~todo_data, initial_todos) if @session.get(~todo_data) == nil
    @session.get(~todo_data)
