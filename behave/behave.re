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

module Behave
  def context(title, lambda)
    bc = Context()
    bc.context(title, lambda)

class Context
  def run
    Main.puts('init')

  def context(title, l)
    @title = title
    Main.puts(['entering context', title].join(' '))
    Main.puts(l)
    should('aaa', fun do
      Main.puts(title)
      Main.puts(@title)
    )
    l()
    Main.puts(['exiting context', title].join(' '))
    
  def should(title, l)
    Main.puts('should!')
    Main.puts(title)
    Main.puts(@title)
    if l() == true
      Main.puts(['ok', title].join(' '))
    else
      Main.puts(['failed', title].join(' '))
