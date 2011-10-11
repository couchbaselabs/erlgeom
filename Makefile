all: build

%.beam: %.erl
	erlc -o test/ $<

build: c_src/erlgeom.c
	./rebar compile

check: test/etap.beam
	prove test/*.t

check-verbose: test/etap.beam
	prove -v test/*.t

clean:
	./rebar clean
	rm -fr priv
