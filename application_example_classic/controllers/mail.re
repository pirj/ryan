class Mail < Controller
  def initialize(session, parameters)
    @session = session
    @parameters = parameters
  end
  
  def unread
    bindings = {}.insert(:body, view('home', {}.insert(:contents, 'unread')))
    render('layout', bindings, [])
  end

  def inbox
    bindings = {}.insert(:body, view('home', {}.insert(:contents, 'inbox')))
    render('layout', bindings, [])
  end

  def sent
    bindings = {}.insert(:body, view('home', {}.insert(:contents, 'sent')))
    render('layout', bindings, [])
  end

  def spam
    bindings = {}.insert(:body, view('home', {}.insert(:contents, 'spam')))
    render('layout', bindings, [])
  end
  
  def trash
    bindings = {}.insert(:body, view('home', {}.insert(:contents, 'trash')))
    render('layout', bindings, [])
  end
end
