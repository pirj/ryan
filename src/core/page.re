class Page < Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    @callbacks = []
    @commands = []
  end

  def view(filename, bindings)
    template = Controllers.template(filename)
    page = template.render(bindings.insert(:session, @session))
    js = @callbacks.map{ |callback| get_callback(callback)}.join(';\n')
    (:html, '<script>#{js}</script>#{page}'.to_list())
  end

  def get_callback(callback)
    arguments = callback.to_list().map{ |(k,v)| "#{k}: '#{v}'"}.join(', ')
    'callback({#{arguments}})'
  end
  
  def on(what, event, where)
    callback = {:what => what, :event => event, :where => where}
    size = @callbacks[:size]
    @callbacks[:size] = size + 1
    @callbacks[size] = callback
  end
  
  def on_get(what, event, where, get)
    callback = {:what => what, :event => event, :where => where, :get => get}
    size = @callbacks[:size]
    @callbacks[:size] = size + 1
    @callbacks[size] = callback
  end

  def update(where, data)
    (:html, what) = data
    command = {:command => :update, :where => where, :html => what.to_string()}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end

  def update(where, data, effect)
    (:html, what) = data
    command = {:command => :update, :where => where, :html => what.to_string(), :effect => effect}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end

  def prepend(where, data)
    (:html, what) = data
    command = {:command => :prepend, :where => where, :html => what.to_string()}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
  
  def prepend(where, data, effect)
    (:html, what) = data
    command = {:command => :prepend, :where => where, :html => what.to_string(), :effect => effect}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
  
  def append(where, data, effect)
    (:html, what) = data
    command = {:command => :append, :where => where, :html => what.to_string(), :effect => effect}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
  
  def hide(where, effect)
    command = {:command => :hide,:where => where, :effect => effect}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
  
  def show(where, effect)
    command = {:command => :show, :where => where, :effect => effect}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
  
  def empty(where, effect)
    command = {:command => :empty, :where => where, :effect => effect}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
  
  def growl(text)
    command = {:command => :growl, :text => text}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
 
  def toggleclass(where, clazz)
    command = {:command => :toggleclass, :where => where, :clazz => clazz}
    size = @commands[:size]
    @commands[:size] = size + 1
    @commands[size] = command
  end
  
  def perform
    json = @commands.map {|command| parse_command(command)}
    (:json, json.to_list().to_s().split(%r/\n/).join().to_list())
  end
  
  def parse_command(command)
    ['{', ['#{key}: "#{command[key]}"' for key in command.keys()].join(', '), '}'].join()
  end
end
