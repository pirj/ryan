module Fruits
  def common(parameters, cookies, method, contents)
    [prefix(), "Action requested:&quot;", contents, "&quot; HTTP method:&quot;", method, "&quot; paremeters:", parameters.inspect(), " cookies:", cookies.inspect(), postfix()].join('')

  def show(parameters, cookies, method)
    common(parameters, cookies, method, "show fruits")

  def index(parameters, cookies, method)
    common(parameters, cookies, method, "index fruits")

  def prefix
    "<html><head><title>Reia+YAWS</title></head><body>"

  def postfix
    "</body></html>"