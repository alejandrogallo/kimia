ECL_SRC_PATH = $(abspath lib/src/ecl/$(ECL_COMMIT))
ECL_DYNAMIC_LIB = $(ECL_BUILD_PATH)/lib/libecl.so
ECL_STATIC_LIB = $(ECL_BUILD_PATH)/lib/libecl.a
ECL_GIT_REPOSITORY ?= https://gitlab.com/embeddable-common-lisp/ecl
ECL_CONFIG_FLAGS = --with-cxx \
                   --prefix $(ECL_BUILD_PATH) \
                   --disable-shared \
                   --disable-soname \
                   --enable-gmp \

$(ECL_SRC_PATH)/configure:
	mkdir -p $(@D)
	git clone $(ECL_GIT_REPOSITORY) $(@D)
	cd $(@D) && git checkout $(ECL_COMMIT)

$(ECL_SRC_PATH)/Makefile: $(ECL_SRC_PATH)/configure
	cd $(@D) && $(ECL_SRC_PATH)/configure $(ECL_CONFIG_FLAGS)

$(ECL_STATIC_LIB): $(ECL_SRC_PATH)/Makefile
	$(info Compiling $@)
	cd $(<D) && $(MAKE)
	cd $(<D) && $(MAKE) install

.PHONY: ecl ecl-clean
ecl: $(ECL_STATIC_LIB)

ecl-clean:
	rm -rf $(ECL_BUILD_PATH)

IN_PROJECT_DEPENDENCIES += ecl
