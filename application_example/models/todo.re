module Todo
  def all
    erlang_couchdb:invoke_view({"localhost", 5984}, "iplaywow", "characters", "realm", [{"key", "\"Medivh-US\""}]).
  end
  
  def get
    erlang_couchdb:retrieve_document({"localhost", 5984}, "iplaywow", "0980...").
  end
  
  def save(element)
    erlang_couchdb:update_document({"localhost", 5984}, "iplaywow", "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}]).
  end
  
  def delete(element)
    erlang_couchdb:delete_document({"localhost", 5984}, "iplaywow", "1fd0...", "1193...").
  end
end