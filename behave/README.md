# Testing with Behave

Behave is a testing framework dedicated to test Reia language and libraries

## Syntax
all.re:
Behave.context('A User instance') do 
  setup do 
    user = User.find(~first) 

  should('return its full name') do 
    assert('John Doe' == user.full_name)
    assert('John' == user.name)
    assert('Doe' == user.surname)

  should('return its email address') do 
    'JohnDoe@gmail.com' == user.email

  context('with a profile') do 
    user.profile = Profile.find(~first) 

    should('return true calling .has_profile?') do 
      user.has_profile()

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
