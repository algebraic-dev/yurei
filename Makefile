test:
	idris2 --build tests/tests.ipkg
	cd tests && ./build/exec/runtests .

run:
	idris2 --build yurei.ipkg
	./build/exec

install:
	idris2 --build yurei.ipkg
	idris2 --install yurei.ipkg
	