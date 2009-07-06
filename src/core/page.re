class Page
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
    callback = {}.insert(:what, what).insert(:event, event).insert(:where, where)
    @callbacks = @callbacks.unshift(callback)
  end
  
  def on_get(what, event, where, get)
    callback = {}.insert(:what, what).insert(:event, event).insert(:where, where).insert(:get, get)
    @callbacks = @callbacks.unshift(callback)
  end

  def update(where, data)
    (:html, what) = data
    command = {}.insert(:command, :update).insert(:where, where).insert(:html, what.to_string())
    @commands = @commands.unshift(command)
  end

  def update(where, data, effect)
    (:html, what) = data
    command = {}.insert(:command, :update).insert(:where, where).insert(:html, what.to_string()).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end

  def prepend(where, data)
    (:html, what) = data
    command = {}.insert(:command, :prepend).insert(:where, where).insert(:html, what.to_string())
    @commands = @commands.unshift(command)
  end
  
  def prepend(where, data, effect)
    (:html, what) = data
    command = {}.insert(:command, :prepend).insert(:where, where).insert(:html, what.to_string()).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def append(where, data, effect)
    (:html, what) = data
    command = {}.insert(:command, :append).insert(:where, where).insert(:html, what.to_string()).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def hide(where, effect)
    command = {}.insert(:command, :hide).insert(:where, where).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def show(where, effect)
    command = {}.insert(:command, :show).insert(:where, where).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def empty(where, effect)
    command = {}.insert(:command, :empty).insert(:where, where).insert(:effect, effect)
    @commands = @commands.unshift(command)
  end
  
  def growl(text)
    command = {}.insert(:command, :growl).insert(:text, text)
    @commands = @commands.unshift(command)
  end
  
  def toggleclass(where, clazz)
    command = {}.insert(:command, :toggleclass).insert(:where, where).insert(:clazz, clazz)
    @commands = @commands.unshift(command)
  end
  
  def perform
    json = @commands.map {|command| parse_command(command)}
    (:json, json.to_list().to_s().split(/\n/).join().to_list())
  end
  
  def parse_command(command)
    ['{', ['#{key}: "#{command[key]}"' | key in command.keys()].join(', '), '}'].join()
  end
end