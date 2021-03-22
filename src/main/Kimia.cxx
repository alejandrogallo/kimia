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
  setupRunnerDatabase();

  cl_object output, currentStep;
  output = cl_safe_eval(c_string_to_object(wrappedContents.c_str()), Cnil, Cnil);

  //cl_print(1, output);
  while (!Null(output)) {
    std::cout << "\nRunning STEP\n" << std::endl;
    currentStep = cl_car(output);
    output = cl_cdr(output);
    auto runner
      = (std::string*)clstr(cl_getf(2,
                                    currentStep,
                                    lisp::fromStr(":run-name-c++")));
    cl_object _struct = cl_getf(2, currentStep, lisp::fromStr(":struct"));
    std::cout << "\tFunction name: " <<  *runner << std::endl;

    // TODO: fix getting caster directly
    auto fun = (void (*)(size_t))DATABASE[*runner];
    auto caster = (size_t (*)(cl_object))RUNNER_TO_CASTER[*runner];
    assert(DATABASE.find(*runner) != DATABASE.end());
    assert(RUNNER_TO_CASTER.find(*runner) != RUNNER_TO_CASTER.end());
    fun(caster(_struct));
    //cl_print(1, currentStep);
    //cl_print(1, _struct);
  }

  std::cout << "c++ sum vector" << std::endl;

  cl_shutdown();

  SumVector<double> vv = {{{8.8}}, {98}};
  assert(DATABASE["runSumVector<double>"]);
  ((void (*)(SumVector<double>&))
   DATABASE["runSumVector<double>"])(vv);
  std::cout << vv.out.sum << std::endl;

}
