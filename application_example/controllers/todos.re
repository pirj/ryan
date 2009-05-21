class Todos < Controller
  def json(filename, bindings)
    js = @callbacks.map{ |callback| get_callback(callback)}.join(';\n')
    contents = view(filename, bindings)
    content(:json, "{script: #{js}, contents: #{contents}}")
  end

  def get_callback(callback)
    arguments = callback.to_list().map{ |(k,v)| "#{k}: '#{v}'"}.join(', ')
    'callback({#{arguments}})'
  end
  
  def on(what, event, where)
    @callbacks = @callbacks.unshift({}.insert(:what, what).insert(:event, event).insert(:where, where))
  end

  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    @callbacks = []
  end
  
  def index2
    data = Models.all(Todo)
    bindings = {}.insert(:todos, data)
    on('#lala', :click, '/app/todos/json_test')
    render('todos/index', bindings, [])
  end

  def index
    data = Models.all(Todo)
    bindings = {}.insert(:todos, data)
    handlers = [{:id => '#add_new', :command => :update, :what => :todo_new, :url => '/app/todos/add_new', :effect => :slide},
    {:id => '#today', :command => :update, :what => :todos, :url => '/app/todos/today'},
    {:id => '#tomorrow', :command => :update, :what => :todos, :url => '/app/todos/tomorrow'},
    {:id => '#few_days', :command => :update, :what => :todos, :url => '/app/todos/few_days'},
    {:id => 'a[icon=delete]', :command => :update, :url => '/app/todos/delete'},
    {:id => '#day_select a', :command => :toggleclass, :clazz => :selected}]
    render('todos/index', bindings, handlers)
  end

  def today
    @session.set(:current_day, :today)
    for_range('today')
  end

  def tomorrow
    @session.set(:current_day, :tomorrow)
    for_range('tomorrow')
  end
  
  def few_days
    @session.set(:current_day, :few_days)
    for_range('few_days')
  end

  def for_range(range)
    data = Models.all(Todo)
    todos = data.filter{|t| t[:when]==range}
    render('todos/list', {}.insert(:todos, todos), [])
  end

  def add_new
    handlers = [{:id => '#add', :command => :prepend, :what => :todos, :url => '/app/todos/add_todo', :get => '#todo_new_text'},
    {:id => '#add', :command => :empty, :what => :todo_new, :effect => :fade},
    {:id => '#cancel', :command => :empty, :what => :todo_new, :effect => :fade}]
    render('todos/new', {}, handlers)
  end

  def add_todo
    day = @session.get(:current_day)
    todo_text = @parameters[:todo_new_text]
    Todo({}.insert(:what, todo_text).insert(:when, day)).save()
    text('#{todo_text}<br/>')
  end
  
  def delete
    day = @session.get(:current_day)
    what = @parameters[:id]
    Todo.delete({}.insert(:what, what).insert(:when, day))
    text('')
  end
end
