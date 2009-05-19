class Budget < Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    initial_todos = [{:what => 'buy milk', :when => :today}, {:what => 'call parents', :when => :tomorrow}, {:what => 'visit dentist', :when => :later}]
    @session.set(:todo, initial_todos) if @session.get(:todo) == nil
  end
    
  def index
    on(:add_new, :click) do add_new() end
    on(:today, :click) do day(:today) end
    on(:tomorrow, :click) do day(:tomorrow) end

    render('todos/index', {}, [])
  end

  def day(range)
    @session.set(:current_day, range)

    data = @session.get(:todo)
    todos = data.filter{|t| t[:when]==range}
    contents = render('todos/list', {}.insert(:todos, todos), [])

    update(:todos, contents)
  end

  def add_new
    # on(:add, :click, [:todo_new_text]) do add_todo() end
    # on(:cancel, :click) do empty(:todo_new, :fade) end
    
    (:html, contents) = render('todo/new', {}, [])
    update(:todo_new, contents)
  end

  def add_todo
    empty(:todo_new)
    day = @session.get(:current_day)
    todo_text = @parameters[:todo_new_text]
    todos = @session.get(:todo).unshift({}.insert(:what, todo_text).insert(:when, day))
    @session.set(:todo, todos)
    prepend(:todos, '#{todo_text}<br/>')
  end
  
  def prepend(what, contents)
    "prepending #{what} with #{contents}".puts()
  end
  
  def update(what, contents)
    "updating #{what}".puts()
  end
  
  def empty(what)
    "empying #{what}".puts()
  end
  
  # def update(what, contents, effect)
  #   "updating #{what} with #{effect} effect".puts()
  # end
  # 
  def on(what, event, &block)
    ''
  end
end
