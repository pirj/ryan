#!/usr/bin/env reia

module Go
  def run
    port = 8001
#    port = args[1].to_int() if args[0] == '-p' or args[0] == '--port'
    
    # if args[0] == '-h' or args[0] == '--help'
    #   help()
    # else
      dependencies()
      init_ets()
      erl.reia.load('app.re'.to_list())
  
      erl.mochi_shim.start_mochi(port)
    # end
  end

  def dependencies
    'Loading Ryan libs: '.print()
    # ryan_dir = [erl.code.lib_dir().to_string(), 'ryan', 'lib'].join('/')
    ryan_dir = ['ebin'].join('/')
    # required = Dir.list(ryan_dir)
    required = ["controller.reb", "controllers.reb", "page.reb", "retem.reb", "core.reb", "session.reb"]
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

  def help
    'Usage: ryan [--help|-h] [--port|-p <port_number>] [--yaws|--mochi]'.puts()
  end
end  

"run #{Go.run()}".puts()
