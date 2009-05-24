class Todos < Controller
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
    @commands = []
  end
  
  def update(command)
    @commands = @commands.unshift(command.insert(:command, :update))
  end
  
  def growl(command)
    @commands = @commands.unshift(command.insert(:command, :growl))
  end
  
  def perform
    json = @commands.map {|command| parse_command(command)}
    json.to_s().puts()
    (:json, json.to_s().to_list())
  end
  
  def parse_command(command)
    ['{', ['#{key}: "#{command[key]}"' | key in command.keys()].join(', '), '}'].join()
  end

  def add_new
    handlers = [{:id => '#add', :command => :prepend, :what => :todos, :url => '/app/todos/add_todo', :get => '#todo_new_text'},
    {:id => '#add', :command => :empty, :what => :todo_new, :effect => :fade},
    {:id => '#cancel', :command => :empty, :what => :todo_new, :effect => :fade}]

    render('todos/new', {}, handlers)
  end
  
  def show_new
    update({:where => "#todo_new", :effect => "slide", :url => '/app/todos/add_new'})
    perform()
  end
  
  def index
    on('#add_new', :click, '/app/todos/show_new')

    page = view('todos/index', {})
    js = @callbacks.map{ |callback| get_callback(callback)}.join(';\n')
    (:html, '<script>#{js}</script>#{page}'.to_list())
  end

  def index2
    handlers = [
    #{:id => '#add_new', :command => :update, :what => :todo_new, :url => '/app/todos/add_new', :effect => :slide},
    {:id => '#today', :command => :update, :what => :todos, :url => '/app/todos/today'},
    {:id => '#tomorrow', :command => :update, :what => :todos, :url => '/app/todos/tomorrow'},
    {:id => '#few_days', :command => :update, :what => :todos, :url => '/app/todos/few_days'},
    {:id => '#day_select a', :command => :toggleclass, :clazz => :selected}]
    render('todos/index', {}, handlers)
  end

  def today
    for_range(:today)
  end

  def tomorrow
    for_range(:tomorrow)
  end
  
  def few_days
    for_range(:few_days)
  end

  def for_range(range)
    @session.set(:current_day, range)
    todos = Models.find(Todo, :when, range)
    todos = [todo.data() | todo in todos]
    handlers = [{:id => 'a[icon=delete]', :command => :update, :url => '/app/todos/delete'}]
    render('todos/list', {}.insert(:todos, todos), handlers)
  end

  def add_todo
    day = @session.get(:current_day)
    todo_text = @parameters[:todo_new_text]
    todo = Todo({}.insert(:what, todo_text).insert(:when, day))
    todo.save()
    render('todos/list', {}.insert(:todos, [todo.data()]), [])
  end
  
  def delete
    # insecure!
    Models.get(@parameters[:_id]).delete()
    text('')
  end
end
