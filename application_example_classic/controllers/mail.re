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
    contents = [format(mail) | mail in Mailbox.get(:inbox)].join('<br>')
    bindings = {}.insert(:contents, contents).insert(:total, @total)
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
  
  def format(mail)
    "From: #{mail[:from]}<br/>To: #{mail[:to]}<br/>Contents: #{mail[:contents]}<br/>"
  end
end
