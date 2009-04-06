module Controllers
  def get(controller, session, parameters)
    controller = controller.to_s()
    controller_file = 'controllers/#{controller}.re'
    controller = controller.capitalize().to_atom()
    up_to_date = up_to_date(controller, controller_file)
    'Reloading #{controller_file}'.puts() unless up_to_date
    Main.load(controller_file) unless up_to_date
    reia::spawn(controller, [session, parameters])
  end

  def up_to_date(controller, controller_file)
    if code::is_loaded(controller)
      last_modified = file_last_modified(controller_file)
      class_info = erlang::apply(controller, :module_info, [])
      compile_info = [ci | (:compile, ci) in class_info][0]
      last_loaded = [ll | (:time, ll) in compile_info][0]
      last_loaded > last_modified
    else
      false
    end
  end

  def template(filename)
    last_modified = file_last_modified('views/#{filename}.html')
    cached = ets::lookup(:templates, filename)
    template_up_to_date(filename, cached, last_modified)
  end

  def template_up_to_date(filename, [], last_modified)
    parse_template(filename, last_modified)
  end

  def template_up_to_date(filename, [(_filename, parsed_at, t)], last_modified)
    if last_modified > parsed_at
      parse_template(filename, last_modified)
    else
      t
    end
  end

  def parse_template(filename, last_modified)
    "Parsing #{filename}".puts()
    file = File.read('views/#{filename}.html')
    t = Retem.parse(file.to_string())
    ets::insert(:templates, (filename, last_modified, t))
    t
  end

  def file_last_modified(path)
    (:ok, (:file_info,_,_,_,_,last_modified,_,_,_,_,_,_,_,_)) = file::read_file_info(path.to_list())
    last_modified = erlang::localtime_to_universaltime(last_modified)
    ((y,m,d),(h,n,s)) = last_modified
    (y,m,d,h,n,s)
  end
end

class Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
  end

# redirect to url
  def redirect(url)
    (:redirect, url.to_list())
  end

# do nothing
  def ok
    :ok
  end

# return status (other than default 200)
# example: status(404)
# for status codes list surf to http://www.w3.org/Protocols/HTTP/HTRESP.html
  def status(status)
    (:status, status)
  end

# return content of a specific mimetype
# example: content('application/pdf', pdf)
  def content(mimetype, content)
    (:content, mimetype.to_list(), content.to_list())
  end

# return plain text
  def text(text)
    (:html, text.to_list())
  end

# # return rendered content from a view template file
# # example: view('fruits_index', {:apple: {:weight: 30, :color: 'red'}})
  def view(filename, bindings)
    # (_h, is, ins) = erlang::now()
    # (_h, s, ns) = erlang::now()
    template = Controllers.template(filename)
    # Main.puts(["parsing:", (s-is) * 1000000 + ns - ins, "ns"].join(' '))
    rendered = Retem.render(template, bindings.insert(:session, @session))
    # (_h, s1, ns1) = erlang::now()
    # Main.puts(["rendering:", (s1-s) * 1000000 + ns1 - ns, "ns"].join(' '))
    rendered
  end

# return rendered content from a view template file with handlers attached
# example: render('fruits_index', {:apple => {:weight => 30, :color => 'red'}}, [(:landing, :contents, 'landing')])
  def render(filename, bindings, handlers)
    page = view(filename, bindings)
    js = add_handlers(handlers)
    (:html, '<script>#{js}</script>#{page}'.to_list())
  end
  
  def add_handlers(handlers)
    handlers.map{ |handler| add_handler(handler)}.join(';\n')
  end

  def add_handler(handler)
    h = handler.insert(:event, :click)
    arguments = h.to_list().map{ |(k,v)| "#{k}: '#{v}'"}.join(', ')
    'add_handler({#{arguments}})'
  end
end
