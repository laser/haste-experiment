all: hello-fox hello-fox.js

hello-fox.js: hello-fox.hs
	hastec hello-fox.hs

hello-fox:
	ghc --make hello-fox.hs

clean:
	-rm hello-fox.hi
	-rm hello-fox.o

distclean: clean
	-rm hello-fox
	-rm hello-fox.js
