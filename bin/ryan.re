#!/usr/bin/env reia

module GoRyan
  def run(args)
  "0".print()
    port = 8001
    port = args[1].to_int() if args[0] == '-p' or args[0] == '--port'
    
    if args[0] == '-h' or args[0] == '--help'
      help()
    else
  '1'.puts()
#      check_couchdb()
      if File.file?('startup.re')
  '2'.puts()
        dependencies()
  '3'.puts()
        init_ets()
  '4'.puts()
#        init_db()
#        load_models()
        load_pages()
  '5'.puts()
        Main.load('startup.re')
  '6'.puts()
      
        if [a for a in args, a=='--yaws'].size() == 0
          erl.mochi_shim.start_mochi(port)
        else
          erl.yaws_shim.init_yaws(port)
        end
      else
        '! You are either running from incorrect folder, either startup.re is missing'.puts()
      end
    end
  end

  def dependencies
    'Loading Ryan libs: '.print()
    ryan_dir = [erl.code.lib_dir().to_string(), 'ryan', 'lib'].join('/')
    # required = Dir.list(ryan_dir)
    required = ["behave.re", "controller.re", "controllers.re", "model.re", "page.re", "retem.re", "ryan.re", "session.re"]
    required.each do |lib|
      '#{lib} '.print()
      Main.load('#{ryan_dir}/#{lib}')
    end
    ' done.'.puts()
  end

  def init_ets
    erl.ets.new(:sessions, [:named_table, :public])
    erl.ets.new(:templates, [:named_table, :public])
  end

#  def check_couchdb
#    try
#      erl.erlang_couchdb.server_info(('localhost'.to_list(), 5984))
#      true
#    catch e
#      "! CouchDB doesn't seem to be running".puts()
#      false
#    end
#  end
  
#  def init_db
#    if Dir.list('models').size() > 0 and erl.erlang_couchdb.database_info(('localhost'.to_list(), 5984), 'default'.to_list()) == (:ok,[(<[error]>,<[not_found]>),(<[reason]>,<[Missing]>)])
#      'Creating database'.puts()
#      erl.erlang_couchdb.create_database(('localhost'.to_list(), 5984), 'default'.to_list())
#    end
#  end

  def load_pages
    'Loading pages: '.print()
    pages = Dir.list('pages')
    pages.each do |page|
      '#{page} '.print()
      Main.load('pages/#{page}')
    end
    ' done.'.puts()
  end
  
#  def load_models
#    'Loading models: '.print()
#    models = Dir.list('models')
#    models.each do |model|
#      '#{model} '.print()
#      Main.load('models/#{model}')
#      object = reia::spawn(model.split(/\\.re/)[0].capitalize().to_atom(), [{}])
#      object.create_views()
#    end
#    ' done.'.puts()
#  end
  
  def help
    'Usage: ryan [--help|-h] [--port|-p <port_number>] [--yaws|--mochi]'.puts()
  end
end  
  
"yoyo".print()
"ola".puts()
"args #{System.args()}".puts()
"run #{GoRyan.run(System.args())}".puts()

