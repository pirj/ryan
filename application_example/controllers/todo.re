class Todo
  def index(session, parameters)
    bindings = {}.insert(~todos, todos())
    handlers = [('#add', ~append, ~todos, '/app/todo/add'),
    ('#today', ~update, ~todos, '/app/todo/today', true),
    ('#tomorrow', ~update, ~todos, '/app/todo/tomorrow', true)]
    ('todo/index', bindings, handlers)

  def today(session, parameters)
    for_range(~today)

  def tomorrow(session, parameters)
    for_range(~tomorrow)

  def few_days(session, parameters)
    for_range(~few_days)

  def later(session, parameters)
    for_range(~later)

  def for_range(range)
    todos = todos().filter{|t| t[~when]==range}
    ('todo/list', {}.insert(~todos, todos))

  def add(session, parameters)
    'something added<br/>'

  def todos
    [(~todos, todos)] = ets::lookup(~mocks, ~todos)
    todos

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
