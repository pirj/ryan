# How Behave works:
# 
# Behave.context('A User instance') do
#   setup do
#     user = User.find(~first)
# 
#   should('return its full name') do
#     assert('John Doe' == user.full_name)
# 
#   should('return its email address') do
#     assert('JohnDoe@gmail.com' == user.email)
# 
#   context('with a profile') do
#     user.profile = Profile.find(~first)
# 
#     should('return true calling .has_profile?') do
#       assert(user.has_profile?)
#
# Results in:
#  A User instance should return its full name.
#  A User instance should return its email address.
#  A User instance with a profile should return true calling .has_profile?.

# module Behave
#   def context(title, lambda)
#     bc = Context.start()
#     bc.context(title, lambda)

class Context
  def run
    Local.puts('init')

  def context(title, l)
    x('qqq')
    @title = title
    Local.puts(['entering context', title].join(' '))
    Local.puts(l)
    should('aaa', fun do
      Local.puts(title)
      Local.puts(@title)
    )
    l()
    Local.puts(['exiting context', title].join(' '))
    
  def should(title, l)
    Local.puts('should!')
    Local.puts(title)
    Local.puts(@title)
    if l() == true
      Local.puts(['ok', title].join(' '))
    else
      Local.puts(['failed', title].join(' '))
  
  def x(t)
     Local.puts(t)