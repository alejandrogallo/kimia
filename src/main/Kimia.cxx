#include <string>
#include <iostream>
#include <sstream>
#include <fstream>
#include <iterator>
#include <ecl/ecl.h>

using LispObject = cl_object;

extern "C" void init_clkimia(cl_object cblock);



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


    ecl_init_module(NULL, init_clkimia);

    // Make C++ functions available to Lisp
    //DEFUN("runtime", runtime, 0);
    //DEFUN("set_runtime", set_runtime, 1);

    // Define some Lisp functions to call from C++
    eval_lisp("(defun header () (format t \"Starting program...~%\"))");
    eval_lisp("(defun makeanumber () 3.2)");
    eval_lisp("(header)");
  }
}


int main(int argc, char **argv) {
  std::ifstream ifs("input.lisp");
  std::string contents;
  std::getline(ifs, contents, '\0');
  ifs.close();
  std::string wrappedContents = "(kimia::wrap-input-script";
  wrappedContents += "\n";
  wrappedContents += contents;
  wrappedContents += ")\n";


  lisp::initialize(argc, argv);

  LispObject output(lisp::eval_lisp(wrappedContents.c_str()));

  if (output == ECL_NIL) {
    std::cout << "c++: we are fucked" << std::endl;
  } else if (ECL_LISTP(output)) {
    std::cout << "c++: LISTP" << std::endl;
  } else if (ECL_CONSP(output)) {
    std::cout << "c++: CONSP" << std::endl;
  } else if (ECL_PATHNAMEP(output)) {
    std::cout << "c++: IT IS A FUCKING PATHNAME??" << std::endl;
  }

  // std::cout <<
  // ECL_CLASS_OF(output)
  //           << "\n"
  //           <<
  // ECL_CLASS_NAME(output)
  //           << "\n"
  //           <<
  //   (ecl_t_of(output) == t_pathname)
  //   << "\n";
  // std::cout << ecl_t_of(output) << std::endl;

  // LispObject number(lisp::eval_lisp("(makeanumber)"));
  // std::cout << "The number is " << number->SF.SFVAL << std::endl;
  // std::cout << t_pathname << std::endl;

}

