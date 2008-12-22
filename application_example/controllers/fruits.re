module Fruits
  def show(parameters, cookies, method)
    Ryan.page('layout', {~title: 'show', ~head: '', ~contents: {~page: 'fruits_show', ~apple_color: 'red', ~apple_weight: 1.2}})
# Ryan.page('layout', 'fruits_show', 'show', '', {~apple_color: 'red', ~apple_weight: 1.2})

  def index(parameters, cookies, method)
    Ryan.page('layout', {~title: 'index', ~head: '', ~contents: {~page: 'fruits_index', ~apples: 1, ~apple_color: 'red', ~apple_weight: 1.2}})
# Ryan.page('layout', 'fruits_index', 'index', '', {~apples: 1, ~apple_color: 'red', ~apple_weight: 1.2})
