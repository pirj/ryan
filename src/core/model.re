module Models
  def all(model)
    view(model, :all)
  end
    
  def view(model, name)
    (:json, [_, _, (_, data)]) = erlang_couchdb::invoke_view(('localhost'.to_list(), 5984), 'default'.to_list(), name.to_list(), name.to_list(), [])
    
    data2 = data.map {|d| dict::from_list(dict::from_list(d)["value".to_binary()])}
    data3 = data2.map do |d|
      dict::from_list(d.to_list().map { |(k,v)| (k.to_string().to_atom(), v.to_string())} )
    end
  end
end

class Model
  def initialize(data)
    @data = data
    base(:default)
  end
  
  def save
    data2 = @data.to_list().map do |(k, v)|
      (k.to_s().to_binary(), v.to_s().to_binary())
    end
    
    erlang_couchdb::create_document((@db_host, @db_port), @db_name, data2.unshift(("_type".to_binary(), class().to_s().to_binary())))
    #erlang_couchdb::update_document((@db_host, @db_port), @db_name, "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}]).
  end
  
  def get(doc_id)
    erlang_couchdb::retrieve_document((@db_host, @db_port), @db_name, doc_id.to_list())
  end
  
  def delete(doc_id, revision)
    erlang_couchdb::delete_document((@db_host, @db_port), @db_name, doc_id.to_list(), revision.to_list())
  end

  def add_view(name, map)
    erlang_couchdb::create_view((@db_host, @db_port), @db_name, name.to_list(), 'javascript'.to_binary(), [(name.to_s().to_binary(), map.to_s().to_binary())])
  end
    
  def add_view2(name, map, reduce)
    erlang_couchdb::create_view((@db_host, @db_port), @db_name, name.to_list(), 'javascript'.to_binary(), [(name.to_s().to_binary(), map.to_s().to_binary(), reduce.to_s().to_binary())])
  end

  def base(name)
    host = :localhost
    port = 5984
    @db_host = host.to_list()
    @db_port = port
    @db_name = name.to_list()
    if erlang_couchdb::database_info((@db_host, @db_port), @db_name) == (:ok,[(<<"error">>,<<"not_found">>),(<<"reason">>,<<"Missing">>)])
      'Creating database #{name}'.puts()
      erlang_couchdb::create_database((@db_host, @db_port), @db_name)
      type = class()
      add_view(:all, "function(doc) { if (doc._type == '#{type}') emit(null, doc) }")
    end
  end
end
