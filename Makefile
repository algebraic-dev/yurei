IDRIS2 = ~/.idris2/bin/idris2

test:
	${IDRIS2} --build tests/tests.ipkg
	cd tests && ./build/exec/runtests .

build:
	${IDRIS2}  --build yurei.ipkg

run: build 
	./build/exec

install: build
	 ${IDRIS2} --install yurei.ipkg
	