class Home < Controller
  def initialize(_session, _parameters)
    jss = ['ryan', 'jquery-1.3.2.min', 'jquery.jgrowl_minimized']
    headjs = ['<script type="text/javascript" src="/js/#{js}.js"></script>' | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = ['<link href="/css/#{css}.css" rel=stylesheet type=text/css>' | css in csss].join()
    head = [headcss, headjs].join()
    @bindings = {}.insert(~title, 'Ryan and Reia homemade webapp').insert(~head, head).insert(~contents, view('home', {}, []))

    @handlers = [{~id: '#landing', ~command: ~update, ~what: ~contents, ~url: '/app/landing', ~effect: ~slide},
    {~id: '#todo', ~command: ~update, ~what: ~contents, ~url: '/app/todo', ~effect: ~slide},
    {~id: '#budget', ~command: ~update, ~what: ~contents, ~url: '/app/budget', ~effect: ~slide},
    {~id: '#menu a', ~command: ~toggleclass, ~clazz: ~selected}]
  end

  def index
    render('layout', @bindings, @handlers)
  end
end
