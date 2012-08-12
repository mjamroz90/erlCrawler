
all: compile

compile:
	./rebar compile

test: compile
	./rebar eunit

clean:
	./rebar clean

clean_doc:
	rm ./docs/*.html ./docs/*.css

edoc:
	erl -pa ./docs -run gen_edoc generate -run init stop -noshell
