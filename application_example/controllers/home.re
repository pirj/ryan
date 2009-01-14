class Home
  def initialize
    jss = ['ryan', 'jquery-1.2.6.min', 'jquery.livequery.min', 'ui.core.min', 'effects.core', 'effects.highlight', 'jquery.jgrowl', 'app']
    headjs = [['<script type="text/javascript" src="/js/', js, '.js"></script>'].join() | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = [['<link href="/css/', css, '.css" rel=stylesheet type=text/css>'].join() | css in csss].join()
    head = [headcss, headjs].join()
    @bindings = {}.insert(~title, 'Ryan and Reia homemade webapp').insert(~head, head).insert(~contents, Ryan.view('home', {}))
    @handlers = [(~landing, ~contents, '/app/landing'),
    (~todo, ~contents, '/app/todo'),
    (~budget, ~contents, '/app/budget')]
    
  def index(parameters, cookies)
    ('layout', @bindings, @handlers)
