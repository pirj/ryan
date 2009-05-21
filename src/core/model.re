class Model
  def initialize
    @data = {}
    base(:default)
  end
  
  def all
    view(:all)
  end

  def save()
    data2 = @data.to_list().map do |(k, v)|
      (k.to_s().to_binary(), v.to_s().to_binary())
    end

    erlang_couchdb::create_document((@db_host, @db_port), @db_name, data2.unshift((<<"_type">>, class().to_s().to_binary())))
    #erlang_couchdb::update_document((@db_host, @db_port), @db_name, "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}]).
  end
  
#  def get
#    erlang_couchdb::retrieve_document(('localhost'.to_list(), 5984), 'todo'.to_list(), "0980...").
#  end
  
#  def delete(element)
#    erlang_couchdb:delete_document({"localhost", 5984}, "iplaywow", "1fd0...", "1193...").
#  end

  def base(name)
    base_host(:localhost, 5984, name)
  end

  def base_host(host, port, name)
    @db_host = host.to_list()
    @db_port = port
    @db_name = name.to_list()
    if erlang_couchdb::database_info((@db_host, @db_port), @db_name) == (:error,(:json,[(<<"error">>,<<"not_found">>),(<<"reason">>,<<"Missing">>)]))
    #(:ok,[(<<"error">>,<<"not_found">>),(<<"reason">>,<<"Missing">>)])
      'Creating database #{name}'.puts()
      erlang_couchdb::create_database((@db_host, @db_port), @db_name)
      type = class()
      add_view(:all, "function(doc) { if (doc.type == '#{type}') emit(doc, doc) }")
    end
  end
  
  def add_view(name, map)
    erlang_couchdb::create_view((@db_host, @db_port), @db_name, name.to_list(), <<"javascript">>, [(name.to_s().to_binary(), map.to_s().to_binary())])
  end
    
  def add_view2(name, map, reduce)
    erlang_couchdb::create_view((@db_host, @db_port), @db_name, name.to_list(), <<"javascript">>, [(name.to_s().to_binary(), map.to_s().to_binary(), reduce.to_s().to_binary())])
  end
  
  def view(name)
  # (:json, [_, _, (<<"rows">>, data)])
  # different CouchDB versions? Behaves differently on Ubuntu 9 and OSX 10.4 ?
    (:json, [_, (<<"rows">>, data)]) = erlang_couchdb::invoke_view((@db_host, @db_port), @db_name, name.to_list(), name.to_list(), [])
    
    data2 = data.map {|d| dict::from_list(dict::from_list(d)[<<"key">>])}
    data3 = data2.map do |d|
      dict::from_list(d.to_list().map { |(k,v)| (k.to_string().to_atom(), v.to_string())} )
    end
  end
end
