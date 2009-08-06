class Home < Page
  def index
    jss = ['jquery-1.3.2.min', 'jquery.jgrowl_minimized', 'ryan', 'dyn_ryan']
    headjs = ['<script type="text/javascript" src="/js/#{js}.js"></script>' | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = ['<link href="/css/#{css}.css" rel=stylesheet type=text/css>' | css in csss].join()
    
    on('#landing', :click, '/app/home/landing')
    on('#todo', :click, '/app/home/todos')
    
    # page['#landing'].on(:click, Home.landing)
    # page['#todo'].on(:click, Home.todos)

    js = @callbacks.map{ |callback| get_callback(callback)}.join(';\n')
    head = [headcss, headjs, '<script>$(document).ready(function() {#{js}})</script>'].join()
    
    bindings = {}.insert(:title, 'Ryan and Reia homemade webapp').insert(:head, head)
    view('home', bindings)
  end
  
  def landing
    update('#contents', Ryan.page(:Landing, :index, @session, @parameters), 'fade')
    toggleclass('#landing', :selected)
    perform()
  end

  def todos
    update('#contents', Ryan.page(:Todos, :index, @session, @parameters), 'fade')
    toggleclass('#todo', :selected)
    perform()
  end

  # def landing
  #   page['#contents'].update(render(:Landing, :index), 'fade')
  #   page['#landing'].toggleclass(:selected)
  # end
  # 
  # def todos
  #   page['#contents'].update(render(:Todos, :index), 'fade')
  #   page['#todo'].toggleclass(:selected)
  # end
  # 
# move to Page
  # def render(controller, action)
  #   Ryan.page(controller, action, @session, @parameters)
  # end
  
  # def page(selector)
  #   Element(selector)
  # end
end

# class Element
#   
# end