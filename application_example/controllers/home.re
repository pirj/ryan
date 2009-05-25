class Home < Controller
  def index
    jss = ['jquery-1.3.2.min', 'jquery.jgrowl_minimized', 'ryan', 'dyn_ryan']
    headjs = ['<script type="text/javascript" src="/js/#{js}.js"></script>' | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = ['<link href="/css/#{css}.css" rel=stylesheet type=text/css>' | css in csss].join()

    handlers = [{:id => '#landing', :command => :update, :what => :contents, :url => '/app/landing', :effect => :slide},
    {:id => '#todo', :command => :update, :what => :contents, :url => '/app/todos', :effect => :slide},
#    {:id => '#budget', :command => :update, :what => :contents, :url => '/app/budget', :effect => :slide},
    {:id => '#menu a', :command => :toggleclass, :clazz => :selected}]
    js = add_handlers(handlers)


    head = [headcss, headjs, '<script>$(document).ready(function() {#{js}})</script>'].join()
    
    bindings = {}.insert(:title, 'Ryan and Reia homemade webapp').insert(:head, head)
    render('home', bindings, [])
  end
end
