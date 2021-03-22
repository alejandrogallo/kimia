#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <sstream>
#include <fstream>
#include <iterator>
#include <cwchar>
#include <ecl/ecl.h>
#include <Generated.hpp>

extern "C" void init_clkimia(cl_object cblock);
extern cl_object struct_get_fields(cl_object);

struct A { int a; double b; };

size_t cl_object_to_int (const cl_object o, const std::vector<size_t> &args) {
  return (size_t)new int(ecl_to_int(o));
}

size_t cl_object_to_double (const cl_object o, const std::vector<size_t> &args) {
  return (size_t)new double(ecl_to_double(o));
}

size_t cl_object_to_float (const cl_object o, const std::vector<size_t> &args) {
  return (size_t)new float(ecl_to_float(o));
}

size_t cl_object_to_char (const cl_object o, const std::vector<size_t> &args) {
  return (size_t)new char(ecl_to_char(o));
}

size_t cl_object_to_bool (const cl_object o, const std::vector<size_t> &args) {
  return (size_t)new bool(ecl_to_bool(o));
}

size_t cl_object_to_struct_A
  (const cl_object o, const std::vector<size_t> &args) {
  return (size_t)new A{*(int*)args[0], *(double*)args[1]};
}



std::string wrapInProgn(const std::string &code) {
  std::string result{"(progn\n"};
  result += code;
  result += "\n)";
  return result;
}

namespace lisp {

  cl_object fromStr(const std::string &s) {
    return c_string_to_object(s.c_str());
  }

  cl_object eval_lisp(const std::string &code) {
    return cl_safe_eval(c_string_to_object(code.c_str()), Cnil, Cnil);
  }

  void initialize(int argc, char **argv) {

    // Bootstrap
    cl_boot(argc, argv);
    //    atexit(cl_shutdown);

    // Run initrc script
    //eval_lisp("(load \"input.lisp\")");


    ecl_init_module(NULL, init_clkimia);

    // Make C++ functions available to Lisp
    //DEFUN("runtime", runtime, 0);
    //DEFUN("set_runtime", set_runtime, 1);

    // Define some Lisp functions to call from C++
    eval_lisp("(defun header () (princ \"LISP SUBSYSTEM WORKING\"))");
    eval_lisp("(defun makeanumber () 3.2)");
    eval_lisp("(header)");
  }
}

std::string
describeEclObject(cl_object o) {
  switch (ecl_t_of(o)) {
  case t_start:
    return "start";
  case t_list:
    return "list";
  case t_character:
    return "character";
  case t_fixnum:
    return "fixnum";
  case t_bignum:
    return "bignum";
  case t_ratio:
    return "ratio";
  case t_singlefloat:
    return "singlefloat";
  case t_doublefloat:
    return "doublefloat";
  case t_longfloat:
    return "longfloat";
  case t_complex:
    return "complex";
  case t_last_number:
    return "last_number";
  case t_symbol:
    return "symbol";
  case t_package:
    return "package";
  case t_hashtable:
    return "hashtable";
  case t_array:
    return "array";
  case t_vector:
    return "vector";
  case t_string:
    return "string";
  case t_base_string:
    return "base_string";
  case t_bitvector:
    return "bitvector";
  case t_stream:
    return "stream";
  case t_random:
    return "random";
  case t_readtable:
    return "readtable";
  case t_pathname:
    return "pathname";
  case t_bytecodes:
    return "bytecodes";
  case t_bclosure:
    return "bclosure";
  case t_cfun:
    return "cfun";
  case t_cfunfixed:
    return "cfunfixed";
  case t_cclosure:
    return "cclosure";
  case t_structure:
    return "structure";
  case t_codeblock:
    return "codeblock";
  case t_foreign:
    return "foreign";
  case t_frame:
    return "frame";
  case t_weak_pointer:
    return "weak_pointer";
  case t_end:
    return "end";
  case t_other:
    return "other";
  case t_contiguous:
    return "contiguous";
  default:
    return "Unknown";
  }
}


int main(int argc, char **argv) {
  std::ifstream ifs("input.lisp");
  std::string contents;
  std::getline(ifs, contents, '\0');
  ifs.close();
  std::string prognContents(wrapInProgn(contents));
  std::string wrappedContents = "(kimia::wrap-input-script";
  wrappedContents += "\n";
  wrappedContents += contents;
  wrappedContents += ")\n";

  lisp::initialize(argc, argv);
  //cl_object output(lisp::eval_lisp(wrappedContents.c_str()));

  cl_object output;

  //std::cout << wrappedContents << std::endl;
  output = cl_safe_eval(c_string_to_object(wrappedContents.c_str()), Cnil, Cnil);
  //output = cl_safe_eval(c_string_to_object(contents.c_str()), Cnil, Cnil);
 // output = cl_safe_eval(c_string_to_object(prognContents.c_str()), Cnil, Cnil);

  //std::cout << "OUTPUT: " << std::endl;
  //cl_print(1, output);
  cl_object currentStep;

  while (!Null(output)) {
    std::cout << "\nRunning STEP\n" << std::endl;
    currentStep = cl_car(output);
    output = cl_cdr(output);
    auto runner = (std::string*)clstr(cl_getf(2, currentStep, lisp::fromStr(":run-name-c++")));
    std::cout << "\tFunction name: " <<  *runner << std::endl;
    cl_print(1, currentStep);
  }

  std::cout << "c++ sum vector" << std::endl;

  cl_shutdown();

  using FUN_TYPE = void (*)(void);
  setupRunnerDatabase();
  FUN_TYPE lala = (FUN_TYPE)POINTER_DATABASE["fuck<double>"];
  lala();
  int iii =0b1111111111111111111111111111111;
  ((void (*)(int&))POINTER_DATABASE["sayHy"])(iii);
  //(*(void (*)(void) *)POINTER_DATABASE["fuck<double>"])();

  SumVector<double> vv = {{{8.8}}, {98}};
  //runSumVector<double>(vv);
  //std::cout << vv.out.sum << std::endl;
  ((void (*)(SumVector<double>&))
   POINTER_DATABASE["runSumvector<double>"])(vv);
  std::cout << vv.out.sum << std::endl;

}
