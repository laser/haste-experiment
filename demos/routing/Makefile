all: routing routing.js

routing.js: routing.hs
	hastec routing.hs

routing: routing.hs
	ghc --make routing.hs

clean:
	-rm -r main
	-rm *~
	-rm routing.hi
	-rm routing.o

distclean: clean
	-rm routing
	-rm routing.js
