class Home < Controller
  def index
    total = Mailbox.total()
    selected = {:home => :selected}
    bindings = {}.insert(:contents, 'home').insert(:total, total).insert(:selected, selected)
    render('home', bindings, [])
  end
end
