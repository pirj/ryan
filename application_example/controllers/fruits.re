module Fruits
  def index(parameters, cookies)
    apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]
    Ryan.view('layout', {~title: 'index', ~head: '', ~contents: Ryan.view('fruits_index', {~apples: [{~color: 'red', ~weight: 2.3, ~id: 0}, {~color: 'yellow', ~weight: 1.3, ~id: 1}]})})

  def show(parameters, cookies)
    # apples = [{~color: 'red', ~weight: 2.3}, {~color: 'yellow', ~weight: 1.3}]
    # id = param(parameters, 'id')
    Ryan.view('layout', {~title: 'show', ~head: '', ~contents: Ryan.view('fruits_show', {~apple: {~color: 'yellow', ~weight: 1.3}})})

  def row(parameters, cookies)
    Ryan.view('fruits_row', {~apple: {~color: 'yellow', ~weight: 1.3}})

  def param(parameters, name)
    parameters.filter {|p| p[0].to_string() == name}[0][1]

  def callback(parameters, cookies)
    Local.puts('something called me!')
    'apapa'
