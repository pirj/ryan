class Home < Controller
  def get_callback(callback)
    arguments = callback.to_list().map{ |(k,v)| "#{k}: '#{v}'"}.join(', ')
    'callback({#{arguments}})'
  end

  def index
    jss = ['jquery-1.3.2.min', 'jquery.jgrowl_minimized', 'ryan']
    headjs = ['<script type="text/javascript" src="/js/#{js}.js"></script>' | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = ['<link href="/css/#{css}.css" rel=stylesheet type=text/css>' | css in csss].join()

    handlers = [{:id => '#landing', :command => :update, :what => :contents, :url => '/app/landing', :effect => :slide},
    {:id => '#todo', :command => :update, :what => :contents, :url => '/app/todo', :effect => :slide},
    {:id => '#budget', :command => :update, :what => :contents, :url => '/app/budget', :effect => :slide},
    {:id => '#menu a', :command => :toggleclass, :clazz => :selected}]
    js = add_handlers(handlers)


    # callbacks = [{:id => '#landing', :command => :update, :what => :contents, :url => '/app/landing', :effect => :slide},
    # {:id => '#todo', :command => :update, :what => :contents, :url => '/app/todo', :effect => :slide},
    # {:id => '#budget', :command => :update, :what => :contents, :url => '/app/budget', :effect => :slide},
    # {:id => '#menu a', :command => :toggleclass, :clazz => :selected}]
    # js = callbacks.map{ |callback| get_callback(callback)}.join(';\n')

    head = [headcss, headjs, '<script>$(document).ready(function() {#{js}})</script>'].join()
    
    bindings = {}.insert(:title, 'Ryan and Reia homemade webapp').insert(:head, head).insert(:contents, view('home', {}))
    render('layout', bindings, [])
  end
end
