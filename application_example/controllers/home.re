module Home
  def index(parameters, cookies, method)
    Ryan.page('layout', {~title: 'home', ~head: '', ~contents: ('home_index', {})})
