Main.puts('Running example application')
if erlang_couchdb::database_info(('localhost'.to_list(), 5984), 'todo'.to_list()) == (:ok,[(<<"error">>,<<"not_found">>),(<<"reason">>,<<"Missing">>)])
  erlang_couchdb::create_database(('localhost'.to_list(), 5984), 'todo'.to_list())
  todos = [{:what => 'buy milk', :when => :today}, {:what => 'call parents', :when => :tomorrow}, {:what => 'visit dentist', :when => :few_days}]
  
  todos.each do |todo|
    todo2 = todo.to_list().map do |(k, v)|
      (k.to_list().to_string().to_binary(), v.to_list().to_string().to_binary())
    end

    erlang_couchdb::create_document(('localhost'.to_list(), 5984), 'todo'.to_list(), todo2)
  end

  erlang_couchdb::create_view(("localhost".to_list(), 5984), 'todo'.to_list(), 'todo'.to_list(), <<"javascript">>, [(<<"realm">>, <<"function(doc) { emit(doc, doc) }">>)])
end

