module Controllers
  def get(controller, session, parameters)
    controller = controller.to_s()
    controller_file = ['controllers/', controller, '.re'].join()
    controller = controller.capitalize().to_atom()
    up_to_date = up_to_date(controller, controller_file)
    Main.puts(['Reloading ', controller_file].join()) unless up_to_date
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
