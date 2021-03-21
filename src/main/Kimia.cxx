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

  cl_object toLispObject(const std::string &s) {
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

  std::vector<size_t> ids;
  cl_object i(lisp::eval_lisp("51"));
  int pi(*(int*)cl_object_to_int(i, ids));
  std::cout << pi << std::endl;

  cl_object output, env, value;

  output = cl_safe_eval(c_string_to_object(wrappedContents.c_str()), Cnil, Cnil);
  //output = cl_safe_eval(c_string_to_object(contents.c_str()), env, value);
  //output = cl_safe_eval(c_string_to_object(prognContents.c_str()), Cnil, Cnil);
  //std::cout << "env: " << describeEclObject(env) << std::endl;
  // std::cout << "value: " << describeEclObject(value) << std::endl;

  std::cout << "GOT: \n";
  cl_print(1, output);
  cl_print(1, cl_car(output));
  cl_print(1, cl_car(cl_car(output)));
  cl_print(1, cl_getf(2, cl_car(output), lisp::eval_lisp(":name")));

  std::cout << "\n<<<GOT>>>" << std::endl;

  // ecl_to_double
  //std::cout << describeEclObject(env) << std::endl;
  //std::cout << describeEclObject(value) << std::endl;

  if (output == ECL_NIL) {
    std::cout << "c++: NIL we are fucked" << std::endl;
  } else if (ECL_LISTP(output)) {
    std::cout << "c++: LISTP" << std::endl;
  } else if (ECL_CONSP(output)) {
    std::cout << "c++: CONSP" << std::endl;
  } else if (ECL_PATHNAMEP(output)) {
    std::cout << "c++: IT IS A FUCKING PATHNAME??" << std::endl;
  } else if (ecl_t_of(output) == t_string) {
    std::cout << "c++: string" << std::endl;
    std::cout << describeEclObject(output) << std::endl;
    std::wstring a((wchar_t*)output->string.self);
    std::wcout << a << std::endl;
  } else if (ecl_t_of(output) == t_symbol) {
    std::cout << "c++: symbol" << std::endl;
    std::cout << (size_t)((output->symbol.name)->d.t) << std::endl;
    std::cout << describeEclObject(output) << std::endl;
    std::cout << describeEclObject(output->symbol.name) << std::endl;
    std::cout << describeEclObject(output->symbol.value) << std::endl;
    std::cout << output->symbol.name->base_string.self << std::endl;
  } else {
    std::cout << "c++         : DONT KNOW" << std::endl;
    std::cout << "appartenly  : " << describeEclObject(output) << std::endl;
  }

  SumVector<double> s {{{5.5}}, {8.8}};
  std::cout << s.out.sum << std::endl;

  // cl_object number(lisp::eval_lisp("(makeanumber)"));
  // std::cout << "The number is " << number->SF.SFVAL << std::endl;
  // std::cout << t_pathname << std::endl;

  cl_shutdown();
}

