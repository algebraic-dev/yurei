test:
	idris2 --build tests/tests.ipkg
	cd tests && ./build/exec/runtests .

build:
	idris2 --build yurei.ipkg

run: build 
	./build/exec

install: build
	idris2 --install yurei.ipkg
	