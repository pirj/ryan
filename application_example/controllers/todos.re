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
  
  def append(where, data, effect)
    (:html, what) = data
    command = {}.insert(:command, :append).insert(:where, where).insert(:html, what.to_string()).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def hide(where, effect)
    command = {}.insert(:command, :hide).insert(:where, where).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def show(where, effect)
    command = {}.insert(:command, :show).insert(:where, where).insert(:effect, effect)
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
  
  def toggleclass(where, clazz)
    command = {}.insert(:command, :toggleclass).insert(:where, where).insert(:clazz, clazz)
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


  
  def index
    on('#add_new', :click, '/app/todos/add_new')
    on('#today', :mouseover, '/app/todos/today')
    on('#tomorrow', :mouseover, '/app/todos/tomorrow')
    on('#few_days', :mouseover, '/app/todos/few_days')

    render('todos/index', {})
  end

  def add_new
    on('#add', :click, '/app/todos/add_todo', '#todo_new_text')
    on('#cancel', :click, '/app/todos/add_cancel')
    
    page = render('todos/new', {})
    update('#todo_new', page, 'slide')
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

    page = render('todos/list', {}.insert(:todos, [todo.data()]))
    prepend('#todos', page, 'fade')
    hide('#todo_new', 'slide')
    perform()
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

    on('a[icon=delete]', :click, '/app/todos/delete')
    toggleclass('#day_select ##{range}', :selected)

    page = render('todos/list', {}.insert(:todos, todos))
    update('#todos', page)
    perform()
  end

  def delete
    id = @parameters[:_id]
    hide('p[_id=#{id}] > a[_id=#{id}]', 'slide')

    on('p[_id=#{id}] a[icon=delete]', :click, '/app/todos/confirmed_delete')
    on('p[_id=#{id}] a[icon=no]', :click, '/app/todos/cancel_delete')

    page = render('todos/yesno', {}.insert(:_id, id))
    append('p[_id=#{id}]', page, 'slide')
    perform()
  end
  
  def confirmed_delete
    # insecure!
    id = @parameters[:_id]
    Models.get(id).delete()

    empty('p[_id=#{id}]', 'slide')
    perform()
  end  
  
  def cancel_delete
    id = @parameters[:_id]
    
    empty('p[_id=#{id}] span', 'slide')
    show('a[_id=#{id}]', 'slide')
    perform()
  end
end
