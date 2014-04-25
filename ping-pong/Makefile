all: ping-pong ping-pong.js

ping-pong.js: ping-pong.hs
	hastec ping-pong.hs

ping-pong:
	ghc --make ping-pong.hs

clean:
	-rm -r main
	-rm *~
	-rm ping-pong.hi
	-rm ping-pong.o

distclean: clean
	-rm ping-pong
	-rm ping-pong.js
