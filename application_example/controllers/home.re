module Home
  def index(parameters, cookies)
    # jss = ['jquery-1.2.6.min', 'jquery.livequery.min', 'effects.core', 'ui.core.min', 'effects.highlight', 'jquery.jgrowl.min', 'app']
    # pre = '<script type="text/javascript" src="/js/'
    # post = .js'"></script>'
    # head = [[pre, js, post].join() | js in jss].join()
    # ('layout', {~title: 'home', ~head: head, ~contents: ('home_index', {})})
    ('layout', {~title: 'home', ~head: [['<script type="text/javascript" src="/js/', js, '.js"></script>'].join() | js in ['jquery-1.2.6.min', 'jquery.livequery.min', 'effects.core', 'ui.core.min', 'effects.highlight', 'jquery.jgrowl.min', 'app']].join(), ~contents: Ryan.view('home_index', {})})
