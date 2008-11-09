#  unless path.size() > 2
#   error("application/controller/action expected")

module Ray
 def out(method, path, cookies, params)
  path = path.map {|p| p.to_string()}
  render(shim::run_method(path[1].capitalize(), path[2], []))
#  render(path.join(','))

 def index()
  render('index')

 def render(data)
  (~html, data)

 def error(kind)
  (~html, kind)