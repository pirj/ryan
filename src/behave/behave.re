# How Behave works:
# 
# Behave.context('A User instance') do
#   user = User.find(:first)
# 
#   Behave.should('return its full name') do
#     assert('John Doe' == user.full_name)
#   end
# 
#   Behave.should('return its email address') do
#     assert('JohnDoe@gmail.com' == user.email)
#   end
# 
#   Behave.context('with a profile') do
#     user.profile = Profile.find(:first)
# 
#     Behave.should('return true calling .has_profile?') do
#       assert(user.has_profile?)
#     end
#   end
# end
#
# Results in:
#  A User instance should return its full name.
#  A User instance should return its email address.
#  A User instance with a profile should return true calling .has_profile?.

module Behave
  def context(title, &lambda)
    ''.puts()
    '#{title}:'.print()
    lambda()
  end

  def should(title, &lambda)
    if lambda()
      '.'.print()
    else
      'F'.print()
    end
  end
end