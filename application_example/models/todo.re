module Todo
  def all
    (:json, [_, _, (<<"rows">>, data)]) = erlang_couchdb::invoke_view(('localhost'.to_list(), 5984), 'todo'.to_list(), 'todo'.to_list(), 'realm'.to_list(), [])
    data2 = data.map {|d| dict::from_list(dict::from_list(d)[<<"key">>])}
    data3 = data2.map do |d|
      dict::from_list(d.to_list().map { |(k,v)| (k.to_string().to_atom(), v.to_string())} )
    end
  end
  
#      d.to_list().map {|(k,v)| (k.to_list().to_string().to_atom(), v.to_list().to_string())}

#  def get
#    erlang_couchdb::retrieve_document(("localhost".to_list(), 5984), 'todo'.to_list(), "0980...").
#  end
  
#  def save(element)
#    erlang_couchdb:update_document({"localhost", 5984}, "iplaywow", "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}]).
#  end
  
#  def delete(element)
#    erlang_couchdb:delete_document({"localhost", 5984}, "iplaywow", "1fd0...", "1193...").
#  end
end
