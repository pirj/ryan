class Todo < Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    initial_todos = [{~what: 'buy milk', ~when: ~today}, {~what: 'call parents', ~when: ~tomorrow}, {~what: 'visit dentist', ~when: ~later}]
    @session.set(~todo, initial_todos) if @session.get(~todo) == nil
  end
    
  def index
    data = @session.get(~todo)
    bindings = {}.insert(~todos, data)
    handlers = [{~id: '#add_new', ~command: ~update, ~what: ~todo_new, ~url: '/app/todo/add_new', ~effect: ~slide},
    {~id: '#today', ~command: ~update, ~what: ~todos, ~url: '/app/todo/today'},
    {~id: '#tomorrow', ~command: ~update, ~what: ~todos, ~url: '/app/todo/tomorrow'}]
    render('todo/index', bindings, handlers)
  end

  def today
    @session.set(~current_day, ~today)
    for_range(~today)
  end

  def tomorrow
    @session.set(~current_day, ~tomorrow)
    for_range(~tomorrow)
  end

  def for_range(range)
    data = @session.get(~todo)
    todos = data.filter{|t| t[~when]==range}
    render('todo/list', {}.insert(~todos, todos), [])
  end

  def add_new
    handlers = [{~id: '#add', ~command: ~prepend, ~what: ~todos, ~url: '/app/todo/add_todo', ~get: '#todo_new_text'},
    {~id: '#add', ~command: ~empty, ~what: ~todo_new, ~effect: ~fade},
    {~id: '#cancel', ~command: ~empty, ~what: ~todo_new, ~effect: ~fade}]
    render('todo/new', {}, handlers)
  end

  def add_todo
    day = @session.get(~current_day)
    todo_text = @parameters[~todo_new_text]
    todos = @session.get(~todo).unshift({}.insert(~what, todo_text).insert(~when, day))
    @session.set(~todo, todos)
    text('#{todo_text}<br/>')
  end
end
