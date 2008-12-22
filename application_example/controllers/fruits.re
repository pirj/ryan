module Fruits
  def show(parameters, cookies, method)
    Ryan.page('layout', {~title: 'show', ~head: '', ~contents: ('fruits_show', {~apple_color: 'red', ~apple_weight: 1.2})})

  def index(parameters, cookies, method)
    Ryan.page('layout', {~title: 'index', ~head: '', ~contents: ('fruits_index', {~apples: 1, ~apple_color: 'red', ~apple_weight: 1.2})})
