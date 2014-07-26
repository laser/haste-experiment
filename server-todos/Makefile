all: todo-server todo-server.js

todo-server.js: todo-server.hs
	hastec todo-server.hs

todo-server: todo-server.hs
	ghc --make todo-server.hs

clean:
	-rm -r main
	-rm *~
	-rm todo-server.hi
	-rm todo-server.o

distclean: clean
	-rm todo-server
	-rm todo-server.js
