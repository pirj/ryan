class Fruits
  def index(parameters, cookies)
    jss = ['jquery-1.2.6.min', 'jquery.livequery.min', 'effects.core', 'ui.core.min', 'effects.highlight', 'jquery.jgrowl.min', 'app']
    pre = '<script type="text/javascript" src="/js/'
    post = '.js"></script>'
    head = [[pre, js, post].join() | js in jss].join()
    bindings = {}.insert(~apples, [{~color: 'red', ~weight: 2.3, ~id: 0}, {~color: 'yellow', ~weight: 1.3, ~id: 1}])
    ('layout', {}.insert(~title, 'Fruit store').insert(~head, head).insert(~contents, Ryan.view('fruits_index', bindings)))
# ('layout', {~title: 'home', ~head: head, ~contents: Ryan.view('fruits_index', bindings)})

  def show(parameters, cookies)
    apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]
    # id = parameters[~id]
    # apple = apples[id]
    apple = apples[0]
    ('fruits_show', {}.insert(~apple, apple))

  def row(parameters, cookies)
    ('fruits_row', parameters)

  def param(parameters, name)
    parameters.filter {|p| p[0].to_string() == name}[0][1]
