include Kimia.mk
include etc/make/ctf.mk
include etc/make/ecl.mk


# compiler and linker
CXX = mpicxx

# general and language options (for preprocessing, compiling and linking)
KIMIA_OPTIONS = \
-fopenmp -std=c++11 \
-Wall -pedantic --all-warnings -fmax-errors=3

# optimization options (only for compiling and linking)
OPTIMIZE = -Ofast -march=native -fno-lto

CTF_CONFIG_FLAGS = CXX=${CXX} \
                   AR=gcc-ar \
                   CXXFLAGS="-Ofast -march=native -fno-lto" \
                   LIBS="-L$(BLAS_PATH)" \
                   --no-dynamic

LINK_LIBS = \
-Wl,-Bstatic \
${YAML_LIB} \
${CTF_LIB} \
${BLAS_LIB} \
-lgfortran -lquadmath \
-Wl,-Bdynamic \
${SCALAPACK_LIB} \



INCLUDE_FLAGS = \
${YAML_INCLUDE} \
${BLAS_INCLUDE} \
${CTF_INCLUDE}
