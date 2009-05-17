module Startup
  def initialize
    Main.puts('Running example application')
    if erlang_couchdb::database_info(('localhost'.to_list(), 5984), 'todo'.to_list()) == (:ok,[(<<"error">>,<<"not_found">>),(<<"reason">>,<<"Missing">>)])
      erlang_couchdb::create_database(('localhost'.to_list(), 5984), 'todo'.to_list())
      [{:what.to_list() => 'buy milk'.to_list(), :when.to_list() => :today.to_list()}, {:what.to_list() => 'call parents'.to_list(), :when.to_list() => :tomorrow.to_list()}, {:what.to_list() => 'visit dentist'.to_list(), :when.to_list() => :few_days.to_list()}].each do |todo|
        erlang_couchdb::create_document('localhost'.to_list(), 5984, 'todo'.to_list(), todo.to_list())
      end

#      erlang_couchdb::create_view('localhost'.to_list(), 5984, 'todo'.to_list(), 'todos', "javascript">>, [{<<"realm">>, <<"function(doc) { if (doc.type == 'todo')  emit(doc, null) }">>}])
    end

    # do something
  end
end
