#!/usr/bin/env reia

module GoRyan
  def run
    port = 8001
#    port = args[1].to_int() if args[0] == '-p' or args[0] == '--port'
    
#    if args[0] == '-h' or args[0] == '--help'
#      help()
#    else
#      check_couchdb()
#      if File.file?('startup.re')
        dependencies()
        init_ets()
#        init_db()
#        load_models()
        load_pages()
        erl.reia.load('startup.re'.to_list())
      
 #       if [a for a in args, a=='--yaws'].size() == 0
          erl.mochi_shim.start_mochi(port)
 #       else
 #         erl.yaws_shim.init_yaws(port)
 #       end
 #     else
 #       '! You are either running from incorrect folder, either startup.re is missing'.puts()
#      end
#    end
  end

  def dependencies
    'Loading Ryan libs: '.print()
    # ryan_dir = [erl.code.lib_dir().to_string(), 'ryan', 'lib'].join('/')
    ryan_dir = ['ebin'].join('/')
    # required = Dir.list(ryan_dir)
    required = ["controller.reb", "controllers.reb", "page.reb", "retem.reb", "core.reb", "session.reb"]
# "behave.re", "model.re", 
    required.each do |lib|
      '#{ryan_dir}/#{lib} '.print()
      erl.reia.load('#{ryan_dir}/#{lib}'.to_list())
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
    'Loading pages: '.puts()
    (:ok, pages) = erl.file.list_dir('pages'.to_list())
    pages.each do |page|
      p = page.to_binary()
      'loading #{p} '.puts()
      erl.reia.load('pages/#{p}'.to_list())
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
  

"run #{GoRyan.run()}".puts()
# todo System.args()

