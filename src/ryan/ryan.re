module Ryan
  def out(abspath, method, application, controller, action, cookies, parameters)
    (_ihr, isec, insec) = erlang::now()
    controller_file = ['controllers/', controller.to_string(), '.re'].join()
    Local.load(controller_file)
    controller = controller.to_string().capitalize().to_atom()
    action = action.to_string().to_atom()
    cookies = cookies.map {|(k,v)| (k.to_string(), v.to_string())}
    parameters = parameters.map {|(k,v)| (k.to_string().to_atom(), v.to_string())}
    result = reia::apply(controller, action, [parameters, cookies, method])
    (_hr, sec, nsec) = erlang::now()
    Local.puts([" <- incoming request: ", abspath.to_string(),"\n -> request took ", (sec-isec)*1000 + (nsec-insec)/1000, " ms"].join())
    result

  def render(file, bindings)
    file = yaws_shim::read_file(file)
    template = Retem.parse(file.to_string())
    rendered = Retem.render(template, bindings)
    Local.puts(rendered)
    rendered.to_s()
