module Ray
 def out(method, path, cookies, params)
  application = path[0]
  controller = path[1]
  action = path[2]
  render(shim::run_method(controller, action, []))

 def index()
  render('index')

 def render(data)
  (~html, data)

 def error(kind)
  (~html, kind)