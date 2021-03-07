include etc/make/ecl.mk

CLKIMIA_SRC_PATH = $(abspath lisp)
CLKIMIA_NAME = clkimia
CLKIMIA_INI_NAME = init_clkimia
CLKIMIA_STATIC_LIB = $(CLKIMIA_BUILD_PATH)/lib$(CLKIMIA_NAME).a
CLKIMIA_SOURCES = $(wildcard $(CLKIMIA_SRC_PATH)/*.lisp)
CLKIMIA_OBJS = $(patsubst %.lisp,%.o,$(CLKIMIA_SOURCES))

define ECL_LISP_TO_O
$(ECL) --eval "(require 'cmp)" \
       --eval '(compile-file "$(1)" :system-p t)' \
       --eval "(quit)"
endef

define ECL_O_TO_STATIC
$(ECL) --eval "(require 'cmp)" \
       --eval '(c:build-static-library \
                "$(CLKIMIA_BUILD_PATH)/$(CLKIMIA_NAME)" \
                :lisp-files `($(patsubst %,"%",$(1))) \
                :init-name "$(CLKIMIA_INI_NAME)")' \
       --eval "(quit)"
endef

%.o: %.lisp
	$(call ECL_LISP_TO_O,$<)

$(CLKIMIA_STATIC_LIB): $(CLKIMIA_OBJS)
	mkdir -p $(@D)
	$(call ECL_O_TO_STATIC,$(CLKIMIA_OBJS))

clkimia: $(CLKIMIA_STATIC_LIB)
.PHONY: clkimia

IN_PROJECT_DEPENDENCIES += clkimia
IN_PROJECT_DEPENDENCIES_FILES += $(CLKIMIA_STATIC_LIB)
