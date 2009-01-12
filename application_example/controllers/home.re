class Home
  def index(parameters, cookies)
    jss = ['ryan', 'jquery-1.2.6.min', 'jquery.livequery.min', 'effects.core', 'ui.core.min', 'effects.highlight', 'jquery.jgrowl', 'app']
    headjs = [['<script type="text/javascript" src="/js/', js, '.js"></script>'].join() | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = [['<link href="/css/', css, '.css" rel=stylesheet type=text/css>'].join() | css in csss].join()
    head = [headcss, headjs].join()
    bindings = {}.insert(~title, 'Ryan and Reia homemade webapp').insert(~head, head).insert(~contents, Ryan.view('home', {}))
    handlers = [(~fruits, ~contents, 'app/fruits'), (~todo, ~contents, 'app/todo')]
    ('layout', bindings, handlers)
