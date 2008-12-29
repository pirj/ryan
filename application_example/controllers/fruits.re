module Fruits
  def index(parameters, cookies)
    apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]
    ('layout', {~title: 'index', ~head: [['<script type="text/javascript" src="/js/', js, '.js"></script>'].join() | js in ['jquery-1.2.6.min', 'jquery.livequery.min', 'effects.core', 'ui.core.min', 'effects.highlight', 'jquery.jgrowl', 'app']].join(), ~contents: Ryan.view('fruits_index', {~apples: [{~color: 'red', ~weight: 2.3, ~id: 0}, {~color: 'yellow', ~weight: 1.3, ~id: 1}]})})

  def show(parameters, cookies)
    # apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]
    # id = param(parameters, 'id')
    ('fruits_show', {~apple: {~color: 'yellow', ~weight: 1.3}})

  def row(parameters, cookies)
    ('fruits_row', parameters)

  def param(parameters, name)
    parameters.filter {|p| p[0].to_string() == name}[0][1]
