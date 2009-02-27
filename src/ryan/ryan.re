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
    reia::invoke(controller_object, action, [])

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

  def route((app, controller, action))
    (controller, action.to_atom())

  def route((app, controller))
    (controller, ~index)

  def route((app))
    ('home', ~index)

  def add_handlers(handlers)
    js = [add_handler(handler) | handler in handlers].join(';')
    '<script>$(document).ready(function() {#{js}})</script>'

  def add_handler(handler)
    h = handler.insert(~event, ~click)
    arguments = h.to_list().map{|(k,v)| [k.to_s(), ": '", v.to_s(), "'"].join()}.join(',')
    'add_handler({#{arguments}})'
#    arguments = h.to_list().map{|(k, v)| "#{k}: '#{v}'"}.join(',')
