SRC_FILES= \
main/Kimia.cxx \

CLKIMIA_STEPS = \
src/main/steps/TensorReaderSpec.lisp \

CLKIMIA_SOURCES = \
src/clkimia/kimia.lisp \
$(CLKIMIA_STEPS) \

CLKIMIA_SOURCES_TEST = \
src/clkimia/t.lisp \
