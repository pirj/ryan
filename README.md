# Web applications with Ryan and Reia

Ryan is a Reia/Erlang BEAM based web framework.
It allows you to create web applications with clean and short code.

Ryan consists of:
 - Session module
 - Routing module
 - Behave (RSpec-like testing tool)
 - Template engine adapter (Retem)
 - HTTP server adapters (YAWS, Mochiweb)
 - Database adapters (CouchDB)

### Prerequisites

[Erlang/OTP R12B-3 or higher](http://www.erlang.org/download.html),
[Reia language](http://github.com/tarcieri/reia),
[CouchDB 0.9](http://couchdb.apache.org/downloads.html)
[Leex](bundled)
[Mochiweb](bundled)
[erlang_couchdb CouchDB adapter](bundled)

## Building

Run the following from Ryan source folder to build it:
sudo make install

That will install ryan libraries into Erlang lib folder and
ryan executable file into /usr/local/bin

## Running an example application

Run:
    cd application_example
    ryan

Surf to: http://localhost:8001/app

You can specify a port number to run on in command line:
    ryan -p 8080

Modify Todo controller, implement or change actions
Create your own controllers and see the results without the need
to even reload web page.
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
Only CouchDB is currently supported.

### Useful links

[Ryan@github](http://github.com/pirj/ryan)
[Reia language home](http://reia-lang.org)
[Dev blog](http://dev_addict.jot.ly)
[Mailing list](http://groups.google.com/group/ryan-framework)

### This work wouldn't be possble without these guys
* Tony Arcieri : Reia creator
* Robert Virding : Excellent lexer library
* Nick Gerakines : CouchDB adapter
* Dmitry Chernyak : Smart JSON library refinement
* Sean Cribbs : Huge work on PEG parser
* many more

### Authors
* Phil Pirozhkov pirj@mail.ru
