class Home < Page
  def index
    jss = ['jquery-1.3.2.min', 'jquery.jgrowl_minimized', 'ryan', 'dyn_ryan']
    headjs = ['<script type="text/javascript" src="/js/#{js}.js"></script>' | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = ['<link href="/css/#{css}.css" rel=stylesheet type=text/css>' | css in csss].join()

#    {:id => '#menu a', :command => :toggleclass, :clazz => :selected}]
    on('#landing', :click, '/app/home/landing')
    on('#todo', :click, '/app/home/todos')
    
    js = '' #@callbacks.map{ |callback| get_callback(callback)}.join(';\n')
    head = [headcss, headjs, '<script>$(document).ready(function() {#{js}})</script>'].join()
    
    bindings = {}.insert(:title, 'Ryan and Reia homemade webapp').insert(:head, head)
    view('home', bindings)
  end
  
 def landing
   update('#contents', Ryan.page(:Landing, :index, @session, @parameters), 'fade')
   perform()
 end
  
 def todos
   update('#contents', Ryan.page(:Todos, :index, @session, @parameters), 'fade')
   perform()
 end
end
