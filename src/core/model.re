module Models
  def all(model)
    view(model, :all, [])
  end
  
  def find(model, field, value)
    view(model, field, [('key'.to_list(), '"#{value}"'.to_list())])
  end
    
  def view(model, name, keys)
    (:json, data) = erlang_couchdb::invoke_view(('localhost'.to_list(), 5984), 'default'.to_list(), model.to_s().to_list(), name.to_list(), keys)
    parse_view(data)
  end

  def parse_view([(<<"error">>,<<"not_found">>),(<<"reason">>,<<"Missing">>)])
    []
  end

  def parse_view([_total, _data])
    []
  end
  
  def parse_view([_total, _offset, (<<"rows">>, rows)])
    docs = [dict::from_list(dict::from_list(d)["value".to_binary()]) | d in rows]
    docs.map do |doc|
      document = map_to_object(doc)
      reia::spawn(document[:_type].to_atom(), [document])
    end
  end
  
  def map_to_object(doc)
    dict::from_list(doc.to_list().map { |(k,v)| (k.to_string().to_atom(), v.to_string())} )
  end

  def get(doc_id)
    (:json, data) = erlang_couchdb::retrieve_document(('localhost'.to_list(), 5984), 'default'.to_list(), doc_id.to_list())
    parse_get(data)
  end

  def parse_get([(<<"error">>,<<"not_found">>),(<<"reason">>,<<"missing">>)])
    nil
  end

  def parse_get(doc)
    document = map_to_object(doc)
    reia::spawn(document[:_type].to_atom(), [document])
  end
end

class Model
  def initialize(data)
    @data = data.insert(:_type, class().to_s())
  end
  
  def save
    data2 = @data.to_list().map do |(k, v)|
      (k.to_s().to_binary(), v.to_s().to_binary())
    end
    
    (:json,[(<<"ok">>,true),(<<"id">>,id),(<<"rev">>,rev)]) = erlang_couchdb::create_document(('localhost'.to_list(), 5984), 'default'.to_list(), data2)
    @data = @data.insert(:_id, id.to_string()).insert(:_rev, rev.to_string())
    #erlang_couchdb::update_document(('localhost'.to_list(), 5984), 'default'.to_list(), "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}]).
  end
  
  def delete
    id = @data[:_id].to_list()
    rev = @data[:_rev].to_list()
    erlang_couchdb::delete_document(('localhost'.to_list(), 5984), 'default'.to_list(), id, rev)
  end
  
  def create_views
    type = class()
    views = fields().map do |key|
      (key, "function(doc) { if (doc._type == '#{type}') emit(doc.#{key}, doc) }")
    end
    views = views.unshift((:all, "function(doc) { if (doc._type == '#{type}') emit(null, doc) }"))

    viewsb = reia_erl::r2e([(name.to_s().to_binary(), map.to_s().to_binary()) | (name, map) in views])
    #hack to get rid of {tuple, ...}
    erlang_couchdb::create_view(('localhost'.to_list(), 5984), 'default'.to_list(), class().to_s().to_list(), 'javascript'.to_binary(), viewsb)
  end
  
  def data
    @data
  end
end
