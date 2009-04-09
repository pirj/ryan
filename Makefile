PREFIX = /usr/local

# todo: get erlang dir with code:lib_dir()
ERLANG_LIB = $(PREFIX)/lib/erlang/lib
RYAN_LIB = $(ERLANG_LIB)/reia$(VERSION)

all: leex yecc compile

uninstall:
	-rm -r $(RYAN_LIB)
	-rm $(PREFIX)/bin/ryan

install: all uninstall
	mkdir $(RYAN_LIB)

	cp LICENSE $(RYAN_LIB)
	cp README.md $(RYAN_LIB)
	cp -r ebin $(RYAN_LIB)
	-rm $(RYAN_LIB)/ebin/leex.beam

	cp src/retem/retem.re $(RYAN_LIB)
	cp src/core/ryan.re $(RYAN_LIB)
	cp src/core/session.re $(RYAN_LIB)
	cp src/core/controller.re $(RYAN_LIB)

	-mkdir /usr/local/bin
	-rm /usr/local/bin/ryan
	cp bin/ryan /usr/local/bin

	chmod 0755 /usr/local/bin/ryan

ebin/leex.beam:
	erlc -o ebin +debug_info src/third_party/leex/*.erl

compile:
	erlc -o ebin +debug_info src/**/*.erl

# Retem Leex
leex: ebin/leex.beam ebin/retem_scan.beam

# Compile retem_scan using leex
ebin/retem_scan.beam:
	bin/leex src/retem/retem_scan.xrl
	erlc +debug_info -o ebin src/retem/retem_scan.erl
	rm src/retem/retem_scan.erl

yecc: ebin/retem_parse.beam

ebin/retem_parse.beam:
	bin/yecc src/retem/retem_parse.yrl
	erlc +debug_info -o ebin src/retem/retem_parse.erl
	rm src/retem/retem_parse.erl

behave:
	reia behave/all.re

clean:
	rm -f ebin/*
	rm -f src/compiler/reia_scan.erl src/compiler/reia_parse.erl
