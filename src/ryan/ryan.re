module Ryan
  def out(abspath, method, path_parts, session_token, parameters)
    (_h, is, ins) = erlang::now()
    path_parts = path_parts.map { |part| part.to_string() }.to_tuple()
    (controller, action) = route(path_parts)
    session_token = session_token.to_string()
    session = Sessions.get(session_token)
    result = page(controller, action, session, cp(parameters))
    (_h, s, ns) = erlang::now()
    abspath = abspath.to_string()
    took = (s-is) * 1000000 + ns - ins
    'session #{session_token}: #{abspath}, took #{took} ns'.puts()
    result

  def page(controller, action, session, parameters)
    controller_object = Controllers.get(controller, session, parameters)
    reply = reia::invoke(controller_object, action, [])
    render(session, reply)

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
  def render(session, (~redirect, url))
    (~redirect, url.to_list())

# do nothing
# example: (~ok)
  def render(session, ~ok)
    ~ok

# return status (other than default 200)
# example: (~status, 404)
# for status codes list surf to http://www.w3.org/Protocols/HTTP/HTRESP.html
  def render(session, (~status, status))
    (~status, status)
    
# return content of a specific mimetype
# example: (~content, 'application/pdf', pdf)
  def render(session, (~content, mimetype, content))
    (~content, mimetype.to_list(), content.to_list())
    
# return rendered content from a view template file with handlers attached
# example: ('fruits_index', {~apple: {~weight: 30, ~color: 'red'}}, [(~landing, ~contents, 'landing')])
  def render(session, (view, bindings, handlers))
    (~html, Ryan.view(view, bindings.insert(~session, session), handlers).to_list())

# return rendered content from a view template file
# example: ('fruits_index', {~apple: {~weight: 30, ~color: 'red'}})
  def render(session, (view, bindings))
    (~html, Ryan.view(view, bindings.insert(~session, session)).to_list())

# return plain text
  def render(session, text)
    (~html, text.to_list())

  def view(filename, bindings)
    view(filename, bindings, [])

  def view(filename, bindings, handlers)
    file = yaws_shim::read_file('views/#{filename}.html'.to_list())
    # (_h, is, ins) = erlang::now()
    template = Retem.parse(file.to_string())
    # (_h, s, ns) = erlang::now()
    # Main.puts(["parsing:", (s-is) * 1000000 + ns - ins, "ns"].join(' '))
    rendered = Retem.render(template, bindings.insert(~handlers, add_handlers(handlers)))
    # (_h, s1, ns1) = erlang::now()
    # Main.puts(["rendering:", (s1-s) * 1000000 + ns1 - ns, "ns"].join(' '))
    rendered

  def add_handlers(handlers)
    js = [add_handler(handler) | handler in handlers].join(';')
    '<script>$(document).ready(function() {#{js}})</script>'

  def add_handler(handler)
    h = handler.insert(~event, ~click)
#    arguments = h.to_list().map{|(k, v)| "#{k}: '#{v}'"}.join(',')
    arguments = h.to_list().map{|(k,v)| [k.to_s(), ": '", v.to_s(), "'"].join()}.join(',')
    'add_handler({#{arguments}})'

  def route((app, controller, action))
    (controller, action.to_atom())

  def route((app, controller))
    (controller, ~index)

  def route((app))
    ('home', ~index)
