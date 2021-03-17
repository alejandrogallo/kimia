test: testcxx


CONFIG = gcc
ECL_COMMIT = master
ECL_BUILD_PATH ?= $(abspath ../../lib/build/${CONFIG}/ecl/$(ECL_COMMIT))
ECL_BIN = ${ECL_BUILD_PATH}/bin/ecl
ECL_INCLUDE    ?= -I${ECL_BUILD_PATH}/include
ECL_LIB        ?= -L${ECL_BUILD_PATH}/lib -lecl -leclffi -leclgc -leclatomic
GMP_LIB ?= -L$(GMP_PATH) -lgmp

t.o: test-data.lisp
	$(ECL_BIN) --load kimia.lisp --shell lisp-to-obj.lisp -i test-data.lisp

%.o: %.lisp
	$(ECL_BIN) --shell lisp-to-obj.lisp -i $<

libclkimiat.a: t.o kimia.o
	$(ECL_BIN) \
		--shell create-static-library.lisp \
		-i t.o -o clkimiat.a --init init_clkimiat

libclkimia.a: kimia.o
	$(ECL_BIN) \
		--shell create-static-library.lisp \
		-i kimia.o -o clkimia.a --init init_clkimia

testcxx: testcxx.cxx libclkimiat.a libclkimia.a
	mpic++ -pedantic -Wall \
		$(ECL_INCLUDE) $< -Wl,-Bstatic $(ECL_LIB) \
		-L$(PWD) -lclkimiat -L$(PWD) -lclkimia \
		-Wl,-Bdynamic $(GMP_LIB) -o $@

test.h: test-data.lisp
	$(ECL_BIN) --shell ./create-test-headers.lisp | \
		sed "/^;;/d" | \
		sed "/s_nil/d" | tee $@


clean:
	-@rm -v testcxx *.a *.fasc *.o
