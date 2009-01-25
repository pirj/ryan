class Todo
  def index(session, parameters)
    data = get_data(session)
    bindings = {}.insert(~todos, get_data(session))
    handlers = [('#add', ~prepend, ~todos, '/app/todo/add'),
    ('#today', ~update, ~todos, '/app/todo/today'),
    ('#tomorrow', ~update, ~todos, '/app/todo/tomorrow')]
    ('todo/index', bindings, handlers)

  def today(session, parameters)
    session.set(~current_day, ~today)
    for_range(session, ~today)

  def tomorrow(session, parameters)
    session.set(~current_day, ~tomorrow)
    for_range(session, ~tomorrow)

  def for_range(session, range)
    data = get_data(session)
    todos = data.filter{|t| t[~when]==range}
    ('todo/list', {}.insert(~todos, todos))

  def add(session, parameters)
    day = session.get(~current_day)
    todos = session.get(~todo_data).unshift({}.insert(~what, 'something').insert(~when, day))
    session.set(~todo_data, todos)
    'something<br/>'

  def get_data(session)
    initial_todos = [{~what: 'buy milk', ~when: ~today}, {~what: 'call parents', ~when: ~tomorrow}, {~what: 'visit dentist', ~when: ~later}]
    session.set(~todo_data, initial_todos) if session.get(~todo_data) == nil
    session.get(~todo_data)

  def show(session, parameters)
    id = parameters[~id]
    if(id == nil)
      ''
    else
      apple = @apples[id.to_int()]
      ('fruits/show', {}.insert(~apple, apple))

# erlang_couchdb:create_database({"localhost", 5984}, "iplaywow").
# erlang_couchdb:database_info({"localhost", 5984}, "iplaywow").
# erlang_couchdb:server_info({"localhost", 5984}).
# erlang_couchdb:create_document({"localhost", 5984}, "iplaywow", [{<<"name">>, <<"Korale">>}, {<<"type">>}, <<"character">>}]).
# erlang_couchdb:retrieve_document({"localhost", 5984}, "iplaywow", "0980...").
# erlang_couchdb:update_document({"localhost", 5984}, "iplaywow", "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}]).
# erlang_couchdb:delete_document({"localhost", 5984}, "iplaywow", "1fd0...", "1193...").
# erlang_couchdb:create_view({"localhost", 5984}, "iplaywow", "characters", <<"javascript">>, [{<<"realm">>, <<"function(doc) { if (doc.type == 'character')  emit(doc.realm_full, null) }">>}]).
# erlang_couchdb:invoke_view({"localhost", 5984}, "iplaywow", "characters", "realm", [{"key", "\"Medivh-US\""}]).
