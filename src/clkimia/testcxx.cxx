#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <sstream>
#include <fstream>
#include <iterator>
#include <cwchar>
#include <ecl/ecl.h>
#include "test.h"

#define LISP(...) lisp::fromStr(#__VA_ARGS__)

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
  //cl_object output;

  lisp::initialize(argc, argv);

  //output = cl_safe_eval(c_string_to_object(":penis"), Cnil, Cnil);
  //lisp::eval("(in-package :kimia)");
  //lisp::eval("(kimia::wrap-input-script (assert-eq 'a 'a))");
  //lisp::eval("(princ (translate :c++ '(struct tensor-reader-double)))");
  std::cout << "what is happending" << std::endl;

  cl_print(1, lisp::eval("#(1 5 6)"));
  cl_print(1, lisp::eval("(type-of #(1 5 6))"));
  cl_print(1, lisp::eval("(length #(1 5 6))"));
  cl_print(1, lisp::eval("(aref #(1 5 6) 0)"));
  cl_object o(lisp::eval("#(42 43 44)"));

  auto a(*(std::vector<int>*)v_of_clint(lisp::fromStr("#(59 48 987)")));
  auto b(*(std::vector<double>*)v_of_cldouble(lisp::fromStr("#(59.5d0 48.5d0 987.5d0)")));
  auto pa(*(std::vector<int>**)pv_of_clint(lisp::fromStr("#(59 48 987)")));

  for (auto i: a) std::cout << i << std::endl;
  for (auto i: b) std::cout << i << std::endl;
  for (auto i: *pa) std::cout << i << std::endl;


  cl_object reader_lisp
    = LISP( (:name "hello world my name is alejandro"
             :lens #(5 9 8 9.6))
           );

  auto reader(*(TensorReaderDouble*)s_tensor_reader_double(reader_lisp));
  std::cout << reader.name << std::endl;
  for (auto i: reader.lens) {
    std::cout << i + 10.1 << std::endl;
  }





  return 0;
}
