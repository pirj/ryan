class Controlller
  def initialize(@session, @parameters)
  end

# redirect to url
  def redirect(url)
    (:redirect, url.to_list())
  end

# do nothing
  def ok
    :ok
  end

# return status (other than default 200)
# example: status(404)
# for status codes list surf to http://www.w3.org/Protocols/HTTP/HTRESP.html
  def status(status)
    (:status, status)
  end

# return content of a specific mimetype
# example: content('application/pdf', pdf)
  def content(mimetype, content)
    (mimetype.to_list(), content.to_list())
  end

# return plain text
  def text(text)
    (:html, text.to_list())
  end

# # return rendered content from a view template file
# # example: view('fruits_index', {:apple: {:weight: 30, :color: 'red'}})
  def view(filename, bindings)
    template = Controllers.template(filename)
    template.render(bindings.insert(:session, @session))
  end

# return rendered content from a view template file with handlers attached
# example: render('fruits_index', {:apple => {:weight => 30, :color => 'red'}})
  def render(filename, bindings)
    page = view(filename, bindings)
    (:html, page.to_list())
  end
end
