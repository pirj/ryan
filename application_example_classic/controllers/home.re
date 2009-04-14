class Home < Controller
  def index
    total = Mailbox.total()
    selected = {:home => :selected}
    bindings = {}.insert(:contents, view('/home/index', {})).insert(:total, total).insert(:selected, selected)
    render('home', bindings, [])
  end
end
