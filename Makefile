all: compile
compile:
	rebar compile

eunit: test
test: 
	rebar eunit

doc: clean_doc
	erl -noshell -run edoc_run application "q" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean_doc:
	-rm -Rf doc
