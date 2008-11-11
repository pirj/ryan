module Ryan
 def out(method, path, cookies, params)
  application = path[0]
  controller = path[1]
  action = path[2]
  Local.puts([" - incoming request: ", path].join())
#  time_before = calendar::universal_time()
  result = yaws_shim::call_action(controller, action, params, cookies, method)
#  time_after = calendar::universal_time()
#  Local.puts([" - request took: ", time::now_diff(time_after, time_before)].join())
  (~html, result)