class Home
  def index(parameters, cookies)
    jss = ['jquery-1.2.6.min', 'jquery.livequery.min', 'effects.core', 'ui.core.min', 'effects.highlight', 'jquery.jgrowl', 'app']
    headjs = [['<script type="text/javascript" src="/js/', js, '.js"></script>'].join() | js in jss].join()
    csss = ['app', 'jquery.jgrowl']
    headcss = [['<link href="/css/', css, '.css" rel=stylesheet type=text/css>'].join() | css in csss].join()
    head = [headcss, headjs].join()
    ('layout', {}.insert(~title, 'Ryan and Reia homemade webapp').insert(~head, head).insert(~contents, Ryan.view('home', {})))
# ('layout', {~title: 'Ryan and Reia homemade webapp', ~head: head, ~contents: Ryan.view('home', {})})
