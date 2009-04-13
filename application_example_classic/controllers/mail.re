class Mail < Controller
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
    render('home', bindings, [])
  end
end
