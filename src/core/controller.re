module Controllers
  def get(controller, session, parameters)
    controller = controller.to_s()
    controller_file = 'controllers/#{controller}.re'
    controller = controller.capitalize().to_atom()
    up_to_date = up_to_date(controller, controller_file)
    'Reloading #{controller_file}'.puts() unless up_to_date
    Main.load(controller_file) unless up_to_date
    reia::spawn(controller, [session, parameters])

  def up_to_date(controller, controller_file)
    up_to_date(code::is_loaded(controller), controller, controller_file)

  def up_to_date(false, controller, controller_file)
    false

  def up_to_date(_loaded, controller, controller_file)
    (~ok, (~file_info,_,_,_,_,last_modified,_,_,_,_,_,_,_,_)) = file::read_file_info(controller_file.to_list())
    [_,_,(~time,last_loaded),_] = reia::apply(controller, ~module_info, [~compile])
    last_modified = erlang::localtime_to_universaltime(last_modified)
    ((y,m,d),(h,n,s))=last_modified
    last_loaded > (y,m,d,h,n,s)

class Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters

# redirect to url
  def redirect(url)
    (~redirect, url.to_list())

# do nothing
  def ok
    ~ok

# return status (other than default 200)
# example: status(404)
# for status codes list surf to http://www.w3.org/Protocols/HTTP/HTRESP.html
  def status(status)
    (~status, status)

# return content of a specific mimetype
# example: content('application/pdf', pdf)
  def content(mimetype, content)
    (~content, mimetype.to_list(), content.to_list())

# return plain text
  def text(text)
    (~html, text.to_list())

# # return rendered content from a view template file
# # example: render('fruits_index')
#   def render(filename)
#     render(filename, {})
# 
# # return rendered content from a view template file
# # example: render('fruits_index', {~apple: {~weight: 30, ~color: 'red'}})
#   def render(filename, bindings)
#     render(filename, bindings, [])
# 
  def view(filename, bindings, handlers)
    file = yaws_shim::read_file('views/#{filename}.html'.to_list())
    # (_h, is, ins) = erlang::now()
    template = Retem.parse(file.to_string())
    # (_h, s, ns) = erlang::now()
    # Main.puts(["parsing:", (s-is) * 1000000 + ns - ins, "ns"].join(' '))
    js = add_handlers(handlers)
    full_bindings = bindings.insert(~handlers, js).insert(~session, @session)
    rendered = Retem.render(template, full_bindings)
    # (_h, s1, ns1) = erlang::now()
    # Main.puts(["rendering:", (s1-s) * 1000000 + ns1 - ns, "ns"].join(' '))
    rendered

# return rendered content from a view template file with handlers attached
# example: render('fruits_index', {~apple: {~weight: 30, ~color: 'red'}}, [(~landing, ~contents, 'landing')])
  def render(filename, bindings, handlers)
    (~html, view(filename, bindings, handlers).to_list())

  def add_handlers(handlers)
    js = handlers.map{ |handler| add_handler(handler)}.join(';\n')
    '<script>$(document).ready(function() {\n#{js}\n})</script>'

  def add_handler(handler)
    h = handler.insert(~event, ~click)
    arguments = h.to_list().map{ |(k,v)| "#{k}: '#{v}'"}.join(', ')
    'add_handler({#{arguments}})'
