IDRIS2 = ~/.idris2/bin/idris2

test: build
	${IDRIS2} --build tests/tests.ipkg
	cd tests && ./build/exec/runtests .

build:
	@${IDRIS2}  --build yurei.ipkg

run:
	${IDRIS2}  --build yurei.ipkg
	@clear
	@./build/exec/main

install: build
	 ${IDRIS2} --install yurei.ipkg
	