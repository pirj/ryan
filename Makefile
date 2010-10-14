PREFIX = /usr/local
VERSION =

# todo: get erlang dir with code:lib_dir()
ERLANG_LIB = $(PREFIX)/lib/erlang/lib
RYAN_LIB = $(ERLANG_LIB)/ryan$(VERSION)
RYAN_LIB_RE = $(RYAN_LIB)/lib

all: deps src/retem/retem_scan.erl src/retem/retem_parse.erl compile erlangine

deps: erlang_couchdb leex mochi

erlangine:
	erlc -o ebin +debug_info deps/engejson.erl

erlang_couchdb:
	(cd deps/erlang_couchdb;$(MAKE))

leex:
	(cd deps/leex;$(MAKE))

mochi:
	(cd deps/mochiweb;$(MAKE))

uninstall:
	rm -rf $(RYAN_LIB)
	rm -f $(PREFIX)/bin/ryan.re
	rm -f $(PREFIX)/bin/behave

install: all uninstall
	mkdir $(RYAN_LIB)
	mkdir $(RYAN_LIB_RE)
	mkdir $(RYAN_LIB)/ebin

	cp deps/mochiweb/ebin/*.beam $(RYAN_LIB)/ebin
	cp deps/erlang_couchdb/ebin/erlang_couchdb.beam $(RYAN_LIB)/ebin

	cp LICENSE $(RYAN_LIB)
	cp README.md $(RYAN_LIB)
	cp -r ebin $(RYAN_LIB)

	cp src/behave/behave.re $(RYAN_LIB_RE)
	cp src/retem/retem.re $(RYAN_LIB_RE)
	cp src/core/*.re $(RYAN_LIB_RE)

	mkdir -p /usr/local/bin
	rm -rf /usr/local/bin/ryan.re
	cp bin/ryan.re /usr/local/bin
	cp bin/behave /usr/local/bin

	chmod 0755 /usr/local/bin/ryan.re
	chmod 0755 /usr/local/bin/behave

compile:
	mkdir -p ebin
	erlc -o ebin +debug_info src/**/*.erl

# Compile retem_scan using leex
src/retem/retem_scan.erl:
	bin/leex src/retem/retem_scan.xrl
	mv retem_scan.erl src/retem

src/retem/retem_parse.erl:
	bin/yecc src/retem/retem_parse.yrl

behave:
	reia behave/all.re

clean:
	rm -f ebin/*
	rm -f src/retem/retem_scan.erl src/retem/retem_parse.erl
	(cd deps/erlang_couchdb;$(MAKE) clean)
	(cd deps/mochiweb;$(MAKE) clean)
