module Fruits
#  @apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]

  def show(parameters, cookies, method)
    Local.puts(parameters.inspect())
    Ryan.page('layout', {~title: 'show', ~head: '', ~contents: ('fruits_show', {~apple: {~color: 'red', ~weight: 1.2}})})

  def index(parameters, cookies, method)
    # apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]
    Ryan.page('layout', {~title: 'index', ~head: '', ~contents: ('fruits_index', {~apples: [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]})})
