module Fruits
  def common(parameters, cookies, method, contents)
    ["<html><head><title>Ryan and Reia</title></head><body>", "Action requested:&quot;", contents, "&quot;<br/>HTTP method:&quot;", method, "&quot;<br/>paremeters:", parameters.inspect(), "<br/>cookies:", cookies.inspect(), "</body></html>"].join('')

  def show(parameters, cookies, method)
    common(parameters, cookies, method, "show fruits")

  def index(parameters, cookies, method)
    common(parameters, cookies, method, "index")
