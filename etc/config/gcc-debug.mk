include Kimia.mk
include etc/make/ctf.mk
include etc/make/clkimia.mk

# compiler and linker
CXX ?= mpicxx

# general and language options (for preprocessing, compiling and linking)
KIMIA_OPTIONS = \
-fopenmp -std=c++11 \
-Wall -pedantic --all-warnings -fmax-errors=1 \
-Wno-vla \
-Wno-int-in-bool-context \
-DDEBUG

# optimization options (only for compiling and linking)
OPTIMIZE = -O0 -g -fno-lto

CTF_CONFIG_FLAGS = CXX=$(CXX) \
                   AR=gcc-ar \
                   CXXFLAGS="-Ofast -march=native -fno-lto -fopenmp -DPROFILE" \
                   LIBS="-L$(BLAS_PATH)/lib" \
                   --no-dynamic

LINK_LIBS = \
-Wl,-Bstatic \
${CTF_LIB} \
-lquadmath \
${ECL_LIB} \
${CLKIMIA_LIB} \
-Wl,-Bdynamic \
${GMP_LIB} \

INCLUDE_FLAGS = \
${CTF_INCLUDE} \
${ECL_INCLUDE}
