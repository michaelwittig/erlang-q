ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)

all: clean $(OBJECTS)

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS)

doc: clean_doc
	erl -noshell -run edoc_run application "q" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean_doc:
	-rm -Rf doc

release: clean doc
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"

test:
	erl -noshell -pa ebin -eval 'eunit:test("ebin",[verbose])' -s init stop

start:
	erl -pa ebin
