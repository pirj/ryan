module Controllers
  def page(controller, action, session, parameters)
    controller = controller.to_s().capitalize().to_atom()
    ensure_uptodate(controller)
    controller_object = erl.reia.spawn(controller, [session, parameters])
    reia::invoke(controller_object, action, [])
  end
  
  def ensure_uptodate(controller)
    controller_file = 'pages/#{controller.to_s().downcase()}.re'
    up_to_date = up_to_date(controller, controller_file)
    'Reloading #{controller_file}'.puts() unless up_to_date
    Main.load(controller_file) unless up_to_date
  end

  def up_to_date(controller, controller_file)
    if code::is_loaded(controller)
      last_modified = file_last_modified(controller_file)
      class_info = erl.erlang.apply(controller, :module_info, [])
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
