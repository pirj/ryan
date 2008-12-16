# Web applications with Ryan and Reia

Ryan is a Reia/Erlang BEAM based web framework.
It allows you to create web applications with clean and short code.

Ryan consists of:
 - router module
 - Retem (templating engine)
 - Behave (RSpec-like testing tool)
 - YAWS, Mochiweb and Inets HTTP server adapters
 - SQLite3, PostgreSQL, MySQL, Mnesia and Mongo database adapters

### Useful links

[Ryan@lighthouse](http://ryan_reia.lighthouseapp.com/)

[Ryan@github](http://github.com/pirj/ryan/)

[Reia language](http://reia-lang.org)

[Dev blog](http://devaddict.jot.ly/)

### Prerequisites

[Erlang/OTP R12B-3 or higher](http://www.erlang.org/download.html),
[Reia language](http://github.com/tarcieri/reia/),
[YAWS 1.77 or higher](http://yaws.hyber.org/),
[Rake 0.8 or higher](http://github.com/jimweirich/rake)

## Building

Run the following from Ryan source folder to build it:

sudo rake install

That will install ryan libraries into Erlang lib folder and
ryan executable file into /usr/local/bin

## Running an example application

Run:
cd example_application
ryan

Surf to:
http://localhost:8001/app/fruits/show?param1=123&param2=aba
http://localhost:8001/app/fruits/index?param1=apple&param2=banana

Play with different parameter values.

Modify Fruits controller, implement or change actions
Create you own controllers and see the results immediately
Enjoy!

## Routing

Following REST idiom, Ryan provides the ability to parse any url and run the
corresponding controller.

http://host:post/store/fruits/show/122
will be passed to 'store' application, 'fruits' controller, 'show' action (method),
providing it with cookie data, http request method (~GET, ~PUT, ~POST, ~DELETE, ~UPDATE etc.),
and parsed query data list.

## Controllers
Controllers are Reia modules. They reside in application_name/controllers/ folder.
By default each controller method is available to web application user unless otherwise defined
in routing schema.

## Views

Views can be Retem templates (retems) and are placed in application_name/views/ folder.

## Models

Models are object definitions that can be stored, retrieved and queried from storage
systems.
SQLite3, PostgreSQL, MySQL, Mnesia and Mongo are to be supported.

### Authors
* Philpipp Pirozhkov pirj@mail.ru
