module Fruits
  def common(title, contents)
    ["<html><head><title>", title, "</title></head><body>", contents, "</body></html>"].join('')

  # def info(parameters, cookies, method, action)
  #   ["Action requested:&quot;", action, "&quot;<br/>HTTP method:&quot;", method, "&quot;<br/>paremeters:", parameters.inspect(), "<br/>cookies:", cookies.inspect(), "</body></html>"].join('')
  #   common('Ryan and Reia test page', info(parameters, cookies, method, "show fruits"))

  def show(parameters, cookies, method)
    Ryan.render('views/fruits_show.retem', {~apples: 1, ~apple_color: 'red', ~apple_weight: 1.2})

  def index(parameters, cookies, method)
    Ryan.render('views/fruits_index.retem', {~apples: 1, ~apple_color: 'red', ~apple_weight: 1.2})
