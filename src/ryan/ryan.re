module Ryan
  def out(abspath, method, path_parts, cookies, parameters)
    path_parts = path_parts.map { |part| part.to_string() }.to_tuple()
    (controller, action) = route(path_parts)
    controller_file = ['controllers/', controller, '.re'].join()
    Local.load(controller_file)
    controller = controller.capitalize().to_atom()
    cookies = cookies.map {|(k,v)| (k.to_string(), v.to_string())}
    parameters = parameters.map {|(k,v)| (k.to_string().to_atom(), v.to_string())}
    result = reia::apply(controller, action, [parameters, cookies, method])
    result

  def page(pagename, bindings)
    contents = Ryan.part(pagename, bindings)
    contents.to_s()

  def part(filename, bindings)
    file = yaws_shim::read_file(['views/', filename, '.retem'].join(''))
    template = Retem.parse(file.to_string())
    rendered = Retem.render(template, bindings)
    rendered.to_s()

  def route((app, controller, action))
    (controller, action.to_atom())

  def route((app, controller))
    (controller, ~index)

  def route((app))
    ('home', ~index)
