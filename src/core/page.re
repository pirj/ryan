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
    callback = {}
    callback[:what] = what
    callback[:event] = event
    callback[:where] = where

    clbks = @callbacks
    size = clbks[:size]
    clbks[:size] = size + 1
    clbks[size] = callback
  end
  
  def on_get(what, event, where, get)
    callback = {}
    callback[:what] = what
    callback[:event] = event
    callback[:where] = where
    callback[:get] = get

    clbks = @callbacks
    size = clbks[:size]
    clbks[:size] = size + 1
    clbks[size] = callback
  end

  def update(where, data)
    (:html, what) = data
    command = {}
    command[:command] = :update
    command[:where] = where
    command[:html] = what.to_string()

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end

  def update(where, data, effect)
    (:html, what) = data
    command = {}
    command[:command] = :update
    command[:where] = where
    command[:html] = what.to_string()
    command[:effect] = effect

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end

  def prepend(where, data)
    (:html, what) = data
    command = {}
    command[:command] = :prepend
    command[:where] = where
    command[:html] = what.to_string()

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
  
  def prepend(where, data, effect)
    (:html, what) = data
    command = {}
    command[:command] = :prepend
    command[:where] = where
    command[:html] = what.to_string()
    command[:effect] = effect

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
  
  def append(where, data, effect)
    (:html, what) = data
    command = {}
    command[:command] = :append
    command[:where] = where
    command[:html] = what.to_string()
    command[:effect] = effect

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
  
  def hide(where, effect)
    command = {}
    command[:command] = :hide
    command[:where] = where
    command[:effect] = effect

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
  
  def show(where, effect)
    command = {}
    command[:command] = :show
    command[:where] = where
    command[:effect] = effect

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
  
  def empty(where, effect)
    command = {}
    command[:command] = :empty
    command[:where] = where
    command[:effect] = effect

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
  
  def growl(text)
    command = {}
    command[:command] = :growl
    command[:text] = text

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
 
  def toggleclass(where, clazz)
    command = {}
    command[:command] = :toggleclass
    command[:where] = where
    command[:clazz] = clazz

    cmds = @commands
    size = cmds[:size]
    cmds[:size] = size + 1
    cmds[size] = command
  end
  
  def perform
    json = @commands.map {|command| parse_command(command)}
    (:json, json.to_list().to_s().split(%r/\n/).join().to_list())
  end
  
  def parse_command(command)
    ['{', ['#{key}: "#{command[key]}"' for key in command.keys()].join(', '), '}'].join()
  end
end
