module Ryan
  def out(abspath, method, application, controller, action, cookies, parameters)
    (_ihr, isec, insec) = erlang::now()
    parameters = parameters.map {|(k,v)| (k.to_string().to_atom(), v.to_string())}
    cookies = cookies.map {|(k,v)| (k.to_string(), v.to_string())}
    controller_file = ['controllers/', controller.to_string(), '.re'].join()
    Local.load(controller_file)
    result = yaws_shim::call_action(controller, action, parameters, cookies, method)
    (_hr, sec, nsec) = erlang::now()
    Local.puts([" <- incoming request: ", abspath.to_string(),"\n -> request took ", (sec-isec)*1000 + (nsec-insec)/1000, " ms"].join())
    (~html, result.to_binary())