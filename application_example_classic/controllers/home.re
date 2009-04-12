class Home < Controller
  def index
    bindings = {}.insert(:body, view('home', {}.insert(:contents, 'home')))
    render('layout', bindings, [])
  end
end
