class Home
  def index(parameters, cookies)
    jss = ['jquery-1.2.6.min', 'jquery.livequery.min', 'effects.core', 'ui.core.min', 'effects.highlight', 'jquery.jgrowl', 'app']
    pre = '<script type="text/javascript" src="/js/'
    post = '.js"></script>'
    head = [[pre, js, post].join() | js in jss].join()
    ('layout', {}.insert(~title, 'Ryan and Reia homemade webapp').insert(~head, head).insert(~contents, Ryan.view('home', {})))
# ('layout', {~title: 'Ryan and Reia homemade webapp', ~head: head, ~contents: Ryan.view('home', {})})
