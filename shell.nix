with import <nixpkgs> {};
mkShell {
  buildInputs = [
    gnumake
    openmpi
    rsync
    gfortran9
    gcc
    scalapack
    blas
    gmp
  ];
  SCALAPACK_PATH = "${scalapack}";
  BLAS_PATH = "${blas}";
  FFTW = "${fftw}";
  GMP_PATH = "${gmp}/lib";
}
