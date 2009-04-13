class Mail < Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
    @total = {:unread => 3, :inbox => 5, :sent => 2, :spam => 1, :trash => 6}
  end
  
  def unread
    bindings = {}.insert(:contents, 'unread').insert(:total, @total)
    render('home', bindings, [])
  end

  def inbox
    bindings = {}.insert(:contents, 'inbox').insert(:total, @total)
    render('home', bindings, [])
  end

  def sent
    bindings = {}.insert(:contents, 'sent').insert(:total, @total)
    render('home', bindings, [])
  end

  def spam
    bindings = {}.insert(:contents, 'spam').insert(:total, @total)
    render('home', bindings, [])
  end
  
  def trash
    bindings = {}.insert(:contents, 'trash').insert(:total, @total)
    render('home', bindings, [])
  end
end
