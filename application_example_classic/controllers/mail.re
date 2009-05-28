class Mail < Controller
  def new
    selected = {}.insert(:new, :selected)
    total = Mailbox.total()
    values = {}
    if(@parameters[:error] != nil)
      to = @parameters[:to]
      message = @parameters[:message]
      error = @parameters[:error]
      values = {}.insert(:to, to).insert(:message, message).insert(:error, error).insert(:error_class, :error)
    end
    contents = view('mail/new', values)
    bindings = {}.insert(:contents, contents).insert(:total, total).insert(:selected, selected)
    render('home', bindings)
  end
  
  def create
    to = @parameters[:to]
    text = @parameters[:message]
    error = nil
    if(text == nil || text.length() == 0)
      error = 'Message cannot be blank'
    end
    if(to == nil || to.length() == 0)
      error = 'Please fill the addressee in'
    end
    if error == nil
      message = {}.insert(:from, 'me').insert(:to, to).insert(:contents, text)
      mail = Mailbox.get(:sent).unshift(message)
      Mailbox.set(:sent, mail)
      redirect('/app/mail/sent')
    else
      redirect('/app/mail/new?to=#{to}&message=#{text}&error=#{error}')
    end
  end

  def unread
    box(:unread)
  end

  def inbox
    box(:inbox)
  end

  def sent
    box(:sent)
  end

  def spam
    box(:spam)
  end
  
  def trash
    box(:trash)
  end
  
  def box(mbox)
    selected = {}.insert(mbox, :selected)
    total = Mailbox.total()
    contents = view('mail/index', {}.insert(:mails, Mailbox.get(mbox)))
    bindings = {}.insert(:contents, contents).insert(:total, total).insert(:selected, selected)
    render('home', bindings)
  end
end
