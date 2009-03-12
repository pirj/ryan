class Budget < Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    initial_todos = [{:what => 'buy milk', :when => :today}, {:what => 'call parents', :when => :tomorrow}, {:what => 'visit dentist', :when => :later}]
    @session.set(:todo, initial_todos) if @session.get(:todo) == nil
  end
    
  def index
    on(:add_new, :click, fun {add_new()})
    on(:today, :click, fun {day(:today)})
    on(:tomorrow, :click, fun {day(:tomorrow)})

    render('todo/index', {}, [])
  end

  def day(range)
    @session.set(:current_day, range)

    data = @session.get(:todo)
    todos = data.filter{|t| t[:when]==range}
    contents = render('todo/list', {}.insert(:todos, todos), [])

    update(:todos, contents)
  end

  def add_new
    on_withget(:add, :click, fun {add_todo()}, [:todo_new_text])
    on(:cancel, :click, fun {empty(:todo_new, :fade)})
    
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
  def on(what, event, lambda)
    ''
  end
end
