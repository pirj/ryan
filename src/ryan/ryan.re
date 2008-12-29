module Ryan
  def out(abspath, method, path_parts, cookies, parameters)
    path_parts = path_parts.map { |part| part.to_string() }.to_tuple()
    (controller, action) = route(path_parts)
    controller_file = ['controllers/', controller, '.re'].join()
    Local.load(controller_file)
    page(controller, action, parameters, cookies)

  def page(controller, action, parameters, cookies)
    controller = controller.to_s().capitalize().to_atom()
    # cookies = cookies.map {|(k,v)| (k.to_string(), v.to_string())}
    # parameters = parameters.map {|p| {p[0].to_string().to_atom(): p[1].to_string()}}
    render(reia::apply(controller, action, [parameters, cookies]))
    
  def render((~redirect, abspath))
    '' # todo

  def render((view, bindings))
    Ryan.view(view, bindings).to_s()

  def view(filename, bindings)
    file = yaws_shim::read_file(['views/', filename, '.html'].join(''))
    template = Retem.parse(file.to_string())
    rendered = Retem.render(template, bindings)
    rendered.to_s()

  def route((app, controller, action))
    (controller, action.to_atom())

  def route((app, controller))
    (controller, ~index)

  def route((app))
    ('home', ~index)
