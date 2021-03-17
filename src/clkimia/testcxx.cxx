#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <sstream>
#include <fstream>
#include <iterator>
#include <cwchar>
#include <ecl/ecl.h>

extern "C" void init_clkimia(cl_object);
extern "C" void init_clkimiat(cl_object);

namespace lisp {

  cl_object toLispObject(const std::string &s) {
    return c_string_to_object(s.c_str());
  }

  cl_object eval(const std::string &code) {
    return cl_safe_eval(c_string_to_object(code.c_str()), Cnil, Cnil);
  }

  cl_object fromStr(const std::string &code) {
    return c_string_to_object(code.c_str());
  }

  void initialize(int argc, char **argv) {

    // Bootstrap
    cl_boot(argc, argv);
    //atexit(cl_shutdown);
    ecl_init_module(NULL, init_clkimia);
    ecl_init_module(NULL, init_clkimiat);

  }
}


int main (int argc, char **argv) {
  cl_object output;

  lisp::initialize(argc, argv);

  //output = cl_safe_eval(c_string_to_object(":penis"), Cnil, Cnil);
  //lisp::eval("(in-package :kimia)");
  //lisp::eval("(kimia::wrap-input-script (assert-eq 'a 'a))");
  lisp::eval("(princ (translate :c++ '(struct tensor-reader-double)))");
  std::cout << "what is happending" << std::endl;

  return 0;
}
