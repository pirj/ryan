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

Run rake from Ryan source folder to build it

Edit your yaws.conf file and add a server to it:
<server localhost>
        port = 8001
        listen = 0.0.0.0
        docroot = {path_to_your_static_files}
        appmods = <"/", yaws_shim>
</server>

Run yaws --pa <path_to_ryan_ebin>

Surf to: http://localhost:8001/app1/fruits/show/1?param1=123&param2=aba
Play with different parameter values.

Another Fruits controller action implemented is 'app1/fruits/index'
Implement yours in fruits.re! (do not forrget to run rake to rebuild)
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
