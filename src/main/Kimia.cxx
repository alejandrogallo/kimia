#include <string>
#include <iostream>
#include <sstream>
#include <ecl/ecl.h>

using LispObject = cl_object;

LispObject toLispObject(const std::string &s) {
  return c_string_to_object(s.c_str());
}


namespace lisp {

  cl_object eval_lisp(const std::string &code) {
    return cl_safe_eval(c_string_to_object(code.c_str()), Cnil, Cnil);
  }

  void initialize(int argc, char **argv) {

    // Bootstrap
    cl_boot(argc, argv);
    atexit(cl_shutdown);

    // Run initrc script
    //eval_lisp("(load \"input.lisp\")");

    // Make C++ functions available to Lisp
    //DEFUN("runtime", runtime, 0);
    //DEFUN("set_runtime", set_runtime, 1);

    // Define some Lisp functions to call from C++
    eval_lisp("(defun header () (format t \"Starting program...~%\"))");
    eval_lisp("(defun makeanumber () 3.2)");
  }
}


int main(int argc, char **argv) {
  std::cout << "Hello world" << std::endl;
  lisp::initialize(argc, argv);
  lisp::eval_lisp("(header)");
  lisp::eval_lisp("(load \"input.lisp\")");
  LispObject number(lisp::eval_lisp("(makeanumber)"));
}

