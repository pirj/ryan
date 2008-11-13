module Ryan
 def out(abspath, method, path, cookies, parameters)
#  application = path[0]
  controller = path[1]
  action = path[2]
  (ihr, isec, insec) = erlang::now()
  parameters = parameters.map {|(k,v)| (k.to_string().to_atom(), v.to_string())}
  cookies = cookies.map {|(k,v)| (k.to_string(), v.to_string())}
  result = yaws_shim::call_action(controller, action, parameters, cookies, method)
  (hr, sec, nsec) = erlang::now()
  Local.puts([" <- incoming request: ", abspath.to_string(),"\n -> request took ", (sec-isec)*1000 + (nsec-insec)/1000, " ms"].join())
  (~html, result.to_binary())