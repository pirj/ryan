class Home < Controller
  def index
    jss = ['jquery-1.3.2.min', 'jquery.jgrowl_minimized', 'ryan', 'dyn_ryan']
    headjs = ['<script type="text/javascript" src="/js/#{js}.js"></script>' | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = ['<link href="/css/#{css}.css" rel=stylesheet type=text/css>' | css in csss].join()

#    {:id => '#menu a', :command => :toggleclass, :clazz => :selected}]
    on('#landing', :click, '/app/home/landing')
    on('#todo', :click, '/app/home/todos')
    
    js = @callbacks.map{ |callback| get_callback(callback)}.join(';\n')

    head = [headcss, headjs, '<script>$(document).ready(function() {#{js}})</script>'].join()
    
    bindings = {}.insert(:title, 'Ryan and Reia homemade webapp').insert(:head, head)
    render('home', bindings, [])
  end
  
  def landing
    update('#contents', Ryan.page(:Landing, :index, @session, @parameters), 'fade')
    perform()
  end
  
  def todos
    update('#contents', Ryan.page(:Todos, :index, @session, @parameters), 'fade')
    perform()
  end
  
# move this to Controller vvvvvvvvvvvvvvvvvv

  def get_callback(callback)
    arguments = callback.to_list().map{ |(k,v)| "#{k}: '#{v}'"}.join(', ')
    'callback({#{arguments}})'
  end

  def on(what, event, where)
    callback = {}.insert(:what, what).insert(:event, event).insert(:where, where)
    @callbacks = @callbacks.unshift(callback)
  end

  def on(what, event, where, get)
    callback = {}.insert(:what, what).insert(:event, event).insert(:where, where).insert(:get, get)
    @callbacks = @callbacks.unshift(callback)
  end

  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    @callbacks = []
    @commands = []
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

  def hide(where, effect)
    command = {}.insert(:command, :hide).insert(:where, where).insert(:effect, effect)
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

# move this to Controller ^^^^^^^^^^^^
end
