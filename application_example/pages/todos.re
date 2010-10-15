class Todos < Page
  def index
    on('#add_new', :click, '/app/todos/add_new')
    on('#today', :mouseover, '/app/todos/today')
    on('#tomorrow', :mouseover, '/app/todos/tomorrow')
    on('#few_days', :mouseover, '/app/todos/few_days')

    view('todos/index', {})
  end

  def add_new
    on_get('#add', :click, '/app/todos/add_todo', '#todo_new_text')
    on('#cancel', :click, '/app/todos/add_cancel')
    
    page = view('todos/new', {})
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

    page = view('todos/list', {}.insert(:todos, [todo.data()]))
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
    todos = [todo.data() for todo in todos]

    on('a[icon=delete]', :click, '/app/todos/delete')
    toggleclass('#day_select ##{range}', :selected)

    page = view('todos/list', {}.insert(:todos, todos))
    update('#todos', page, 'fade')
    perform()
  end

  def delete
    id = @parameters[:_id]
    hide('p[_id=#{id}] > a[_id=#{id}]', 'slide')

    on('p[_id=#{id}] a[icon=delete]', :click, '/app/todos/confirmed_delete')
    on('p[_id=#{id}] a[icon=no]', :click, '/app/todos/cancel_delete')

    page = view('todos/yesno', {}.insert(:_id, id))
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
