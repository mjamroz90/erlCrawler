CXX = g++
OTPROOT = /usr/lib/erlang
C_SRC = c_src
PRIV = priv
CFLAGS_LIB = -Wall -g -O2 -std=c++0x -I $(OTPROOT)/lib/erl_interface-3.7.13/include -I $(OTPROOT)/erts-5.10.2/include -I $(C_SRC) -L  $(OTPROOT)/lib/erl_interface-3.7.13/lib -L $(PRIV)

all: compile lib64

compile:
	erlc -o ./ebin ./src/*.erl ./test/*.erl

lib32: $(C_SRC)/parse_html_driver.cpp
	$(CXX) -o ./priv/parse_html_driver.so -fpic -shared $(CFLAGS_LIB) $^ -Wl,-rpath,$(PRIV) -lhtmlcxx_i386 -lei_st
	
lib64: $(C_SRC)/parse_html_driver.cpp
	$(CXX) -o ./priv/parse_html_driver.so -fpic -shared $(CFLAGS_LIB) $^ -lhtmlcxx_amd64 -lei_st

clean: 
	rm ./ebin/*.beam
	rm ./priv/parse_html_driver.so

run:
	erl -pa ./ebin

clean_doc:
	rm ./docs/*.html ./docs/*.css

edoc:
	erl -pa ./docs -run gen_edoc generate -run init stop -noshell


