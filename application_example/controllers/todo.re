class Todo < Controller
  def initialize(session, _parameters)
    @session = session
    initial_todos = [{~what: 'buy milk', ~when: ~today}, {~what: 'call parents', ~when: ~tomorrow}, {~what: 'visit dentist', ~when: ~later}]
    @session.set(~todo, initial_todos) if @session.get(~todo) == nil
    
  def index
    data = @session.get(~todo)
    bindings = {}.insert(~todos, data)
    handlers = [{~id: '#add_new', ~command: ~update, ~what: ~todo_new, ~url: '/app/todo/add_new', ~effect: ~slide},
    {~id: '#today', ~command: ~update, ~what: ~todos, ~url: '/app/todo/today'},
    {~id: '#tomorrow', ~command: ~update, ~what: ~todos, ~url: '/app/todo/tomorrow'}]
    render('todo/index', bindings, handlers)

  def today
    @session.set(~current_day, ~today)
    for_range(~today)

  def tomorrow
    @session.set(~current_day, ~tomorrow)
    for_range(~tomorrow)

  def for_range(range)
    data = @session.get(~todo)
    todos = data.filter{|t| t[~when]==range}
    render('todo/list', {}.insert(~todos, todos), [])

  def add_new
    handlers = [{~id: '#add', ~command: ~prepend, ~what: ~todos, ~url: '/app/todo/add_todo'},
    {~id: '#add', ~command: ~empty, ~what: ~todo_new, ~effect: ~slide},
    {~id: '#cancel', ~command: ~empty, ~what: ~todo_new, ~effect: ~slide}]
    render('todo/new', {}, handlers)

  def add_todo
    day = @session.get(~current_day)
    todos = @session.get(~todo).unshift({}.insert(~what, 'something').insert(~when, day))
    @session.set(~todo, todos)
    text('something<br/>')
