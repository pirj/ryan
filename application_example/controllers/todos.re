class Todos < Controller
# move this to Controller vvvvvvvvvvvvvvvvvv

  def get_callback(callback)
    arguments = callback.to_list().map{ |(k,v)| "#{k}: '#{v}'"}.join(', ')
    'callback({#{arguments}})'
  end
  
  def on(what, event, where)
    callback = {}.insert(:what, what).insert(:event, event).insert(:where, where)
    @callbacks = @callbacks.unshift(callback)
  end
  
  def on(what, event, where, get)
    callback = {}.insert(:what, what).insert(:event, event).insert(:where, where).insert(:get, get)
    @callbacks = @callbacks.unshift(callback)
  end
  
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    @callbacks = []
    @commands = []
  end
  
  def update(where, data)
    (:html, what) = data
    command = {}.insert(:command, :update).insert(:where, where).insert(:html, what.to_string())
    @commands = @commands.unshift(command)
  end

  def update(where, data, effect)
    (:html, what) = data
    command = {}.insert(:command, :update).insert(:where, where).insert(:html, what.to_string()).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end

  def prepend(where, data)
    (:html, what) = data
    command = {}.insert(:command, :prepend).insert(:where, where).insert(:html, what.to_string())
    @commands = @commands.unshift(command)
  end
  
  def prepend(where, data, effect)
    (:html, what) = data
    command = {}.insert(:command, :prepend).insert(:where, where).insert(:html, what.to_string()).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def hide(where, effect)
    command = {}.insert(:command, :hide).insert(:where, where).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def empty(where, effect)
    command = {}.insert(:command, :empty).insert(:where, where).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def growl(text)
    command = {}.insert(:command, :growl).insert(:text, text)
    @commands = @commands.unshift(command)
  end
  
  def perform
    json = @commands.map {|command| parse_command(command)}
    (:json, json.to_list().to_s().split(/\n/).join().to_list())
  end
  
  def parse_command(command)
    ['{', ['#{key}: "#{command[key]}"' | key in command.keys()].join(', '), '}'].join()
  end

# move this to Controller ^^^^^^^^^^^^



  def add_new
    on('#add', :click, '/app/todos/add_todo', '#todo_new_text')
    on('#cancel', :click, '/app/todos/add_cancel')
    
    page = view('todos/new', {})
    js = @callbacks.map{ |callback| get_callback(callback)}.join(';\n')
    
    update('#todo_new', (:html, '<script>#{js}</script>#{page}'.to_list()), 'slide')
    perform()
  end
  
  def add_cancel
    empty('#todo_new', 'slide')
    perform()
  end
  
  def add_todo
    day = @session.get(:current_day)
    todo_text = @parameters[:todo_new_text]
    todo = Todo({}.insert(:what, todo_text).insert(:when, day))
    todo.save()
    data = render('todos/list', {}.insert(:todos, [todo.data()]), [])

    prepend('#todos', data, 'fade')
    hide('#todo_new', 'slide')
    perform()
  end
  
  def today
    data = for_range(:today)
    update('#todos', data)
    perform()
  end
  
  def tomorrow
    data = for_range(:tomorrow)
    update('#todos', data)
    perform()
  end
  
  def few_days
    data = for_range(:few_days)
    update('#todos', data)
    perform()
  end
  
  def index
    on('#add_new', :click, '/app/todos/add_new')
    on('#today', :mouseover, '/app/todos/today')
    on('#tomorrow', :mouseover, '/app/todos/tomorrow')
    on('#few_days', :mouseover, '/app/todos/few_days')
    #{:id => '#day_select a', :command => :toggleclass, :clazz => :selected}]

    page = view('todos/index', {})
    js = @callbacks.map{ |callback| get_callback(callback)}.join(';\n')
    (:html, '<script>#{js}</script>#{page}'.to_list())
  end

  def for_range(range)
    @session.set(:current_day, range)
    todos = Models.find(Todo, :when, range)
    todos = [todo.data() | todo in todos]
    handlers = [{:id => 'a[icon=delete]', :command => :update, :url => '/app/todos/delete'}]
    render('todos/list', {}.insert(:todos, todos), handlers)
  end

  def delete
    # insecure!
    Models.get(@parameters[:_id]).delete()
    text('')
  end
end
