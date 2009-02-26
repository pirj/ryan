class Home < Controller
  def initialize(_session, _parameters)
    jss = ['ryan', 'jquery-1.3.1', 'jquery.jgrowl_minimized']
    headjs = [['<script type="text/javascript" src="/js/', js, '.js"></script>'].join() | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = [['<link href="/css/', css, '.css" rel=stylesheet type=text/css>'].join() | css in csss].join()
    head = [headcss, headjs].join()
    @bindings = {}.insert(~title, 'Ryan and Reia homemade webapp').insert(~head, head).insert(~contents, Ryan.view('home', {}))
    @handlers = [{~id: '#landing', ~command: ~update, ~what: ~contents, ~url: '/app/landing'},
    {~id: '#todo', ~command: ~update, ~what: ~contents, ~url: '/app/todo'},
    {~id: '#budget', ~command: ~update, ~what: ~contents, ~url: '/app/budget'},
    {~id: '#menu a', ~command: ~toggleclass, ~clazz: ~selected}]

  def index
    ('layout', @bindings, @handlers)
