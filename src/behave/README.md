# Testing with Behave

Behave is a testing framework dedicated to test Reia language and libraries

## Syntax
all.re:
Behave.context('A User instance') do 
  setup do 
    user = User.find(:first)
  end

  Behave.should('return its full name') do 
    Behave.assert('John Doe' == user.full_name)
    Behave.assert('John' == user.name)
    Behave.assert('Doe' == user.surname)
  end

  Behave.should('return its email address') do 
    'JohnDoe@gmail.com' == user.email
  end

  Behave.context('with a profile') do 
    user.profile = Profile.find(:first) 

    Behave.should('return true calling .has_profile?') do 
      user.has_profile()
    end
  end
end

## Usage
Just type 'behave' in command line
Command line options:
-h, --html <file> : put output to a file in html format
-s, --show-passed : show passed tests too
-t, --trace : show trace

### Examples
$ behave
FAIL A User instance should return its email address.

$ behave --show-passed
A User instance should return its full name.
FAIL: A User instance should return its email address.
A User instance with a profile should return true calling .has_profile?.

$ behave --html
2008-11-25.html created

$ behave --verbose
FAIL A User instance should return its email address: behave/models/user.re: 14

### Useful links

[Ryan@lighthouse](http://ryan_reia.lighthouseapp.com)
[Ryan@github](http://github.com/pirj/ryan)
[Reia language home](http://reia-lang.org)
[Reia@github](http://github.com/tarcieri/reia)
[Dev blog](http://dev_addict.jot.ly)

### Authors
* Philpipp Pirozhkov pirj@mail.ru
