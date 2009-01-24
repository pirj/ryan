module Ryan
  def out(abspath, method, path_parts, session_token, parameters)
    (_h, is, ins) = erlang::now()
    path_parts = path_parts.map { |part| part.to_string() }.to_tuple()
    (controller, action) = route(path_parts)
    session_token = session_token.to_string()
    result = page(controller, action, session(session_token), cp(parameters))
    (_h, s, ns) = erlang::now()
    Local.puts(['session', session_token, ':', abspath.to_string(), ":", (s-is) * 1000000 + ns - ins, "ns"].join(' '))
    result

  def page(controller, action, session, parameters)
    controller = controller.to_s()
    controller_file = ['controllers/', controller, '.re'].join()
    controller = controller.capitalize()
    Local.load(controller_file) unless yaws_shim::up_to_date(controller, controller_file)
    controller_object = reia::apply(controller.to_atom(), ~start, [])
    reply = reia::apply(controller_object, action, [session, parameters])
    render(session, reply)

  def session(token)
    session(token, ets::lookup(~sessions, token))

  def session(token, [])
    session = Session()
    ets::insert(~sessions, (token, session))
    session

  def session(_token, [(_token2, session)])
    session

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
    file = yaws_shim::read_file(['views/', filename, '.html'].join().to_list())
    # (_h, is, ins) = erlang::now()
    template = Retem.parse(file.to_string())
    # (_h, s, ns) = erlang::now()
    # Local.puts(["parsing:", (s-is) * 1000000 + ns - ins, "ns"].join(' '))
    rendered = Retem.render(template, bindings.insert(~handlers, add_handlers(handlers)))
    # (_h, s1, ns1) = erlang::now()
    # Local.puts(["rendering:", (s1-s) * 1000000 + ns1 - ns, "ns"].join(' '))
    rendered

  def add_handlers(handlers)
    js = [add_handler(handler) | handler in handlers].join()
    ['<script>$(document).ready(function() {', js, '})</script>'].join()

  def add_handler((id, command, argument))
    ["add_handler('", id.to_s(), "', 'click', '", command.to_s(), "', '", id.to_s(), "', '", argument.to_s(), "');"].join()

  def add_handler((selector, command, target, argument))
    ["add_handler('", selector.to_s(), "', 'click', '", command.to_s(), "', '#", target.to_s(), "', '", argument.to_s(), "');"].join()

  def add_handler((selector, command, target, argument, fade))
    ["add_handler('", selector.to_s(), "', 'click', '", command.to_s(), "', '#", target.to_s(), "', '", argument.to_s(), "', '", fade.to_s(), "');"].join()

  def route((app, controller, action))
    (controller, action.to_atom())

  def route((app, controller))
    (controller, ~index)

  def route((app))
    ('home', ~index)
