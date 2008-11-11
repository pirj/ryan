module Fruits
  def show(parameters, cookies, method)
    ["show fruits ", method, " ", parameters.inspect(), " ", cookies.inspect()].join(',')

  def index(parameters, cookies, method)
    ["index fruits ", method, " ", parameters.inspect(), " ", cookies.inspect()].join(',')
