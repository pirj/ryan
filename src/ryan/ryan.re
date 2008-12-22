module Ryan
  def out(abspath, method, application, controller, action, cookies, parameters)
    controller_file = ['controllers/', controller.to_string(), '.re'].join()
    Local.load(controller_file)
    controller = controller.to_string().capitalize().to_atom()
    action = action.to_string().to_atom()
    cookies = cookies.map {|(k,v)| (k.to_string(), v.to_string())}
    parameters = parameters.map {|(k,v)| (k.to_string().to_atom(), v.to_string())}
    result = reia::apply(controller, action, [parameters, cookies, method])
    result

  def page(pagename, bindings)
    contents = Ryan.part(pagename, bindings)
    contents.to_s()

  # def page(layout, filename, title, head, bindings)
  #   contents = Ryan.part(filename, bindings)
  #   page_bindings = {~contents: contents, ~title: title, ~head: head}
  #   page = Ryan.part(layout, page_bindings)
  #   page.to_s()

  def part(filename, bindings)
    file = yaws_shim::read_file(['views/', filename, '.retem'].join(''))
    template = Retem.parse(file.to_string())
    rendered = Retem.render(template, bindings)
    rendered.to_s()
