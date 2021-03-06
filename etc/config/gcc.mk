include Kimia.mk
include etc/make/ctf.mk
include etc/make/ecl.mk

# compiler and linker
CXX = mpicxx

# general and language options (for preprocessing, compiling and linking)
KIMIA_OPTIONS = \
-fopenmp -std=c++11 \
-Wall -pedantic --all-warnings -fmax-errors=3 \
-Wno-vla \
-Wno-int-in-bool-context

# optimization options (only for compiling and linking)
OPTIMIZE = -Ofast -march=native -fno-lto

CTF_CONFIG_FLAGS = CXX=${CXX} \
                   AR=gcc-ar \
                   CXXFLAGS="-Ofast -march=native -fno-lto" \
                   LIBS="-L$(BLAS_PATH)" \
                   --no-dynamic

LINK_LIBS = \
-Wl,-Bstatic \
${CTF_LIB} \
-lquadmath \
-Wl,-Bdynamic \
${ECL_LIB} \

INCLUDE_FLAGS = \
${CTF_INCLUDE} \
${ECL_INCLUDE}
