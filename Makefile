CONFIG ?= gcc
-include config.mk
include etc/config/${CONFIG}.mk
include Objects.mk

# goals:
.DEFAULT_GOAL := all
.PHONY: test wiki kimia all clean deps

BUILD_PATH        = build/$(CONFIG)
OBJ_PATH          = $(BUILD_PATH)/obj
BIN_PATH          = $(BUILD_PATH)/bin
OBJ_FILES         = $(patsubst %.cxx,$(OBJ_PATH)/%.o,$(SRC_FILES))
DEP_FILES         = $(patsubst %.cxx,$(OBJ_PATH)/%.d,$(SRC_FILES))
TESTS_OBJECTS     = $(patsubst %.cxx,$(OBJ_PATH)/%.o,$(TEST_SRC_FILES))
# retrieve build environment
VERSION          := $(shell git describe --all --dirty --long)
DATE             := $(shell git log -1 --format="%cd")
COMPILER_VERSION := $(shell ${CXX} --version | head -n 1)

# add build environment specifics to INCLUDE_FLAGS and to KIMIA_OPTIONS
INCLUDE_FLAGS += -Isrc/main
KIMIA_OPTIONS  +=              \
-D_POSIX_C_SOURCE=200112L     \
-D__STDC_LIMIT_MACROS         \
-DFTN_UNDERSCORE=1            \
-DKIMIA_VERSION=\"${VERSION}\" \
"-DKIMIA_DATE=\"${DATE}\""     \
"-DCOMPILER_VERSION=\"${COMPILER_VERSION}\""


# This is a trick just to make sure that the dependencies are built
DEPS_DONE_FILE = $(BUILD_PATH)/deps-built
$(DEPS_DONE_FILE): $(IN_PROJECT_DEPENDENCIES)
	@mkdir -p $(@D)
	@touch $@
deps: $(DEPS_DONE_FILE)


ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),clean-all)
ifneq ($(MAKECMDGOALS),deps)
ifneq ($(CREATE_DEPS),FALSE)
ifneq ($(wildcard $(DEPS_DONE_FILE)), $(DEPS_DONE_FILE))
$(warning You have not built the dependencies for $(CONFIG), please build them)
$(warning with 'make deps CONFIG=$(CONFIG)')
$(error exiting now...)
endif
# try to include dependency files in order to trigger their creation
-include $(DEP_FILES)
endif
endif
endif
endif


# primary target
all: kimia
kimia: $(BIN_PATH)/${KIMIA_TARGET}

clean:
	rm -rf $(OBJ_PATH)
	rm -rf $(BIN_PATH)

clean-all: clean
	@$(MAKE) CREATE_DEPS=FALSE $(patsubst %,%-clean,$(IN_PROJECT_DEPENDENCIES))

test:
	$(MAKE) -C $@

unit-test: $(BIN_PATH)/Test

# generate documentation
doc:
	doxygen

wiki:
	bash utils/extract.sh -R -d wiki/dist -b wiki/build -p src test.wiki

# copy binary to installation directory
install: $(BIN_PATH)/$(KIMIA_TARGET)
	mkdir -p ${KIMIA_INSTALL}
	cp $< ${KIMIA_INSTALL}

# build dependencies only
depend: $(DEP_FILES)

# keep intermediate files
.PRECIOUS: ${OBJ_FILES} ${DEP_FILES}

# create a dependency for object file
$(OBJ_PATH)/%.d: src/%.cxx
	$(info [DEP] $@)
	mkdir -p $(dir $@)
	${CXX} -MM ${KIMIA_OPTIONS} ${INCLUDE_FLAGS} -c src/$*.cxx | \
	  sed 's#[^ :]*\.o[ :]*#$(OBJ_PATH)/$*.o $@: #g' > $@

# compile an object file
$(OBJ_PATH)/%.o: $(OBJ_PATH)/%.d
	$(info [OBJ] $@)
	mkdir -p $(dir $@)

	${CXX} ${KIMIA_OPTIONS} ${OPTIMIZE} ${INCLUDE_FLAGS} -c src/$*.cxx -o $@

# compile and link executable
$(BIN_PATH)/${KIMIA_TARGET}: ${OBJ_FILES}
	$(info [BIN] $@)
	mkdir -p $(dir $@)
	${CXX} ${KIMIA_OPTIONS} ${OPTIMIZE} ${OBJ_FILES} ${INCLUDE_FLAGS} ${LINK_LIBS} -o $@

# compile and link test executable
$(BIN_PATH)/Test: ${OBJ_FILES} $(TESTS_OBJECTS)
	mkdir -p $(dir $@)
	${CXX} ${KIMIA_OPTIONS} ${OPTIMIZE} ${OBJ_FILES} $(TESTS_OBJECTS) ${INCLUDE_FLAGS} ${LINK_LIBS} -o $@
