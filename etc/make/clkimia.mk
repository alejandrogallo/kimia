include etc/make/ecl.mk

CLKIMIA_SRC_PATH = $(abspath src/clkimia/)
CLKIMIA_NAME = clkimia
CLKIMIA_INI_NAME = init_clkimia
CLKIMIA_STATIC_LIB = $(CLKIMIA_BUILD_PATH)/lib$(CLKIMIA_NAME).a
CLKIMIA_FASL_LIB = $(CLKIMIA_BUILD_PATH)/$(CLKIMIA_NAME).fasc
CLKIMIA_SOURCES = $(wildcard $(CLKIMIA_SRC_PATH)/*.lisp)
CLKIMIA_BUILD_SOURCES_PATH = $(CLKIMIA_BUILD_PATH)/src
CLKIMIA_BUILD_SOURCES = $(patsubst %,\
                          $(CLKIMIA_BUILD_SOURCES_PATH)/%,\
                          $(notdir $(CLKIMIA_SOURCES)))
CLKIMIA_BUILD_OBJS = $(patsubst %.lisp,%.o,$(CLKIMIA_BUILD_SOURCES))
CLKIMIA_BUILD_FASC = $(patsubst %.lisp,%.fasc,$(CLKIMIA_BUILD_SOURCES))

define ECL_LISP_TO_O
$(ECL_BIN) --eval "(require 'cmp)" \
           --eval '(compile-file "$(1)" :system-p t)' \
           --eval "(quit)"
endef

define ECL_O_TO_STATIC
$(ECL_BIN) --eval "(require 'cmp)" \
           --eval '(c:build-static-library \
                    "$(CLKIMIA_BUILD_PATH)/$(CLKIMIA_NAME)" \
                    :lisp-files `($(patsubst %,"%",$(1))) \
                    :init-name "$(CLKIMIA_INI_NAME)")' \
           --eval "(quit)"
endef

define ECL_LISP_TO_FASC
$(ECL_BIN) --eval "(require 'cmp)" \
           --eval "(ext:install-bytecodes-compiler)" \
           --eval '(compile-file "$(1)")' \
           --eval "(quit)"
endef

%.o: %.lisp
	$(call ECL_LISP_TO_O,$<)

%.fasc: %.lisp
	$(call ECL_LISP_TO_FASC,$<)

$(CLKIMIA_BUILD_SOURCES): $(CLKIMIA_SOURCES)

$(CLKIMIA_BUILD_SOURCES):
	mkdir -p $(@D)
	for lisp in $(CLKIMIA_SOURCES); do \
		cp $$lisp $(CLKIMIA_BUILD_SOURCES_PATH); done

$(CLKIMIA_FASL_LIB): $(CLKIMIA_BUILD_FASC)
	cat $+ > $@

$(CLKIMIA_STATIC_LIB): $(CLKIMIA_BUILD_OBJS)
	mkdir -p $(@D)
	$(call ECL_O_TO_STATIC,$(CLKIMIA_BUILD_OBJS))

clkimia: $(CLKIMIA_STATIC_LIB) $(CLKIMIA_FASL_LIB)
clkimia-clean:
	rm -rf $(CLKIMIA_BUILD_PATH)
.PHONY: clkimia

IN_PROJECT_DEPENDENCIES += clkimia
IN_PROJECT_DEPENDENCIES_FILES += $(CLKIMIA_STATIC_LIB)
