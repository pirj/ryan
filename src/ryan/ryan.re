module Ryan
  def out(abspath, method, path_parts, cookies, parameters)
    (_h, is, ins) = erlang::now()
    path_parts = path_parts.map { |part| part.to_string() }.to_tuple()
    (controller, action) = route(path_parts)
    controller_file = ['controllers/', controller, '.re'].join()
    Local.load(controller_file) unless yaws_shim::up_to_date(controller.capitalize(), controller_file)
    result = page(controller, action, cp(parameters), cp(cookies))
    (_h, s, ns) = erlang::now()
    Local.puts([abspath.to_string(), ":", (s-is) * 1000000 + ns - ins, "ns"].join(' '))
    result

  def page(controller, action, parameters, cookies)
    controller = controller.to_s().capitalize().to_atom()
    controller_object = reia::apply(controller, ~start, [])
    render(reia::apply(controller_object, action, [parameters, cookies]))

# remove this as soon as ssa issue is resolved vvv
# parameters.map {|p| {p[0].to_string().to_atom(): p[1].to_string()}} oneliner looks better
  def cp(ps)
    cp(ps, 0, {})

  def cp(ps, i, di)
    if(ps.size() <= i)
      di
    else
      cp(ps, i+1, di.insert(ps[i][0].to_string().to_atom(), ps[i][1].to_string()))
# remove this as soon as ssa issue is resolved ^^^

# redirect to url
# example: (~redirect, 'http://search4betterplace.com')
  def render((~redirect, url))
    (~redirect, url)

# do nothing
# example: (~ok)
  def render(~ok)
    ~ok

# return status (other than default 200)
# example: (~status, 404)
# for status codes list surf to http://www.w3.org/Protocols/HTTP/HTRESP.html
  def render((~status, status))
    (~status, status)
    
# return content of a specific mimetype
# example: (~content, 'application/pdf', pdf)
  def render((~content, mimetype, content))
    (~content, mimetype, content)
    
# return rendered content from a view template file
# example: ('fruits_index', {~apple: {~weight: 30, ~color: 'red'}})
  def render((view, bindings))
    (~html, Ryan.view(view, bindings))

# return rendered content from a view template file with handlers attached
# example: ('fruits_index', {~apple: {~weight: 30, ~color: 'red'}}, [])
  def render((view, bindings, handlers))
    (~html, Ryan.view(view, bindings, handlers))

# return plain text
  def render(text)
    (~html, text)

  def view(filename, bindings)
    view(filename, bindings, [])

  def view(filename, bindings, handlers)
    file = yaws_shim::read_file(['views/', filename, '.html'].join(''))
#    (_h, is, ins) = erlang::now()
    template = Retem.parse(file.to_string())
#    (_h, s, ns) = erlang::now()
#    Local.puts(["parsing:", (s-is) * 1000000 + ns - ins, "ns"].join(' '))
    rendered = Retem.render(template, bindings)
    script = [["add_handler('", h[0].to_s(),"', '#", h[1].to_s(), " a', '#", h[2].to_s(), "');"].join() | h in handlers].join()
#    (_h, s1, ns1) = erlang::now()
#    Local.puts(["rendering:", (s1-s) * 1000000 + ns1 - ns, "ns"].join(' '))
    [rendered.to_s(),'<script>', script, '</script>'].join()

  def route((app, controller, action))
    (controller, action.to_atom())

  def route((app, controller))
    (controller, ~index)

  def route((app))
    ('home', ~index)
