class Home < Controller
  def index
    total = {:unread => 3, :inbox => 5, :sent => 2, :spam => 1, :trash => 6}
    bindings = {}.insert(:contents, 'home').insert(:total, total)
    render('home', bindings, [])
  end
end
