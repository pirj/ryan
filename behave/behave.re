# How Behave works:
# 
# Behave.context('A User instance') do
#   user = User.find(:first)
# 
#   should('return its full name') do
#     assert('John Doe' == user.full_name)
# 
#   should('return its email address') do
#     assert('JohnDoe@gmail.com' == user.email)
# 
#   context('with a profile') do
#     user.profile = Profile.find(:first)
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
    Behavior().context(title, lambda)
  end
end

class Behavior
  def context(title, lambda)
    'entering context #{title}'.puts()
    lambda()
    'exiting context #{title}'.puts()
  end

  def should(title, lambda)
    'should #{title}'.print()
    if lambda()
      ':ok'.puts()
    else
      ':failed'.puts()
    end
  end
end