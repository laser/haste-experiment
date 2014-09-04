all: client-server client-server.js

client-server.js: client-server.hs
	hastec client-server.hs

client-server:
	ghc --make client-server.hs

clean:
	-rm -r main
	-rm *~
	-rm client-server.hi
	-rm client-server.o

distclean: clean
	-rm client-server
	-rm client-server.js
