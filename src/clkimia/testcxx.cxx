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
#define TEST_CASE(NAME, ...) { std::cout << "[TEST]: " << NAME << std::endl; \
                               __VA_ARGS__ }

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
  lisp::initialize(argc, argv);

  TEST_CASE("(vec integer): Vector of integers",
            auto o = LISP( #(1 5 4) );
            auto r = *(std::vector<int>*)v_of_clint(o);
            assert(r.size() == 3);
            assert(r[0] == 1);
            assert(r[1] == 5);
            assert(r[2] == 4);)

  TEST_CASE("(vec double-float): Vector of doubles",
            auto o = LISP( #(1.5d0 5.8d0 4.0d0) );
            auto r = *(std::vector<double>*)v_of_cldouble(o);
            assert(r.size() == 3);
            assert(r[0] == 1.5);
            assert(r[1] == 5.8);
            assert(r[2] == 4.0);)

  TEST_CASE("(vec single-float): Vector of float",
            auto o = LISP( #(1.5 5.8 4.0) );
            auto r = *(std::vector<float>*)v_of_clfloat(o);
            assert(r.size() == 3);
            assert(r[0] == float(1.5));
            assert(r[1] == float(5.8));
            assert(r[2] == float(4.0));)

  TEST_CASE("(vec (vec integer)): Vector of vector of ints",
            auto o = LISP( #(#(1 2) #(4 5)) );
            auto r = *(std::vector<std::vector<int>>*)v_of_v_of_clint(o);
            assert(r.size() == 2);
            assert(r[0].size() == 2);
            assert(r[1].size() == 2);
            assert(r[0][0] == 1);
            assert(r[0][1] == 2);
            assert(r[1][0] == 4);
            assert(r[1][1] == 5);)

  TEST_CASE("TensorReaderDouble (not templated)",
            cl_object reader_lisp = LISP((:name "hello world"
                                          :lens #(5 9 8 9.6d0)));
            auto r(*(TensorReaderDouble*)s_tensor_reader_double(reader_lisp));
            assert(r.name == "hello world");
            assert(r.lens[0] == double(5));
            assert(r.lens[1] == double(9));
            assert(r.lens[2] == double(8));
            assert(r.lens[3] == double(9.6));)

  return 0;
}
