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
#define EVAL(...) lisp::eval(#__VA_ARGS__)
#define TEST_CASE(NAME, ...) { std::cout << "[TEST]: " << NAME << std::endl; \
                               __VA_ARGS__ }
#define SUBCASE(NAME, ...) { std::cout << "  - <" << NAME << "> :" << std::endl; \
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

  TEST_CASE("Pointers of simple types",
            SUBCASE("with constant integer",
                    auto o = LISP(42);
                    assert(!ecl_to_bool(cl_symbolp(o)));
                    auto i = (int**)pclint(o);
                    assert(i);
                    assert(**i == 42);)
            SUBCASE("with bound symbol",
                    EVAL((defparameter *pointer-1* 42));
                    auto o = LISP(*pointer-1*);
                    assert(ecl_to_bool(cl_boundp(o)));
                    assert(ecl_to_bool(cl_symbolp(o)));
                    { auto ie = (int*)clint(cl_eval(o));
                      assert(ie);
                      assert(*ie == 42);
                    }
                    auto i = (int**)pclint(o);
                    assert(i);
                    assert(**i == 42);)
            SUBCASE("with unbound symbol",
                    auto o = lisp::fromStr("pointer-1-unbound");
                    assert(ecl_to_bool(cl_symbolp(o)));
                    assert(!ecl_to_bool(cl_boundp(o)));
                    auto i = (int**)pclint(o);
                    assert(i);
                    assert(*i);
                    assert(**i == 0);
                    **i = 1024;
                    auto oo = lisp::fromStr("pointer-1-unbound");
                    auto ii = (int**)pclint(o);
                    assert(**ii == 1024);)

            SUBCASE("Vector of integers immediate",
                    auto o = lisp::eval("`#(,(+ 5 5) 2 3)");
                    auto i = (std::vector<int>**)pv_of_clint(o);
                    assert(i);
                    assert(*i);
                    assert((**i)[0] == 10);
                    assert((**i)[1] == 2);
                    assert((**i)[2] == 3);)

            SUBCASE("Vector of integers with unbound symbol",
                    auto o = lisp::fromStr("pointer-to-my-vecs-of-int");
                    auto i = (std::vector<int>**)pv_of_clint(o);
                    assert(i);
                    assert(*i);
                    assert((**i).size() == 0);
                    (**i).resize(1000);
                    (**i)[654] = 1325648;
                    auto oo = lisp::fromStr("pointer-to-my-vecs-of-int");
                    auto ii = (std::vector<int>**)pv_of_clint(o);
                    assert((**i)[654] == 1325648);)

            )

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
            auto o = LISP((:name "hello world"
                           :lens #(5 9 8 9.6d0)));
            auto r(*(TensorReaderDouble*)s_tensor_reader_double(o));
            assert(r.name == "hello world");
            assert(r.lens[0] == double(5));
            assert(r.lens[1] == double(9));
            assert(r.lens[2] == double(8));
            assert(r.lens[3] == double(9.6));)

  TEST_CASE("Monster struct <int double int>",
            auto o = LISP((:name "Monster Struct with int double int"
                           :data #(4568 987981 1657)
                           :connection (:ip (:ipv4 12300065
                                             :ipv6 456)
                                        :timeout 456.5)
                           :components (:pphh #(1 2 3)
                                        :pppp #(4 5 6)
                                        :hhhh #(7 8 9)
                                        :lens #(3 3 3))
                           :in (:date 121212)
                           :lens #(5.5 6.5)));
            auto r = *(MonsterStruct<int, double, int>*)
                      s_monster_struct_with_clint_and_cldouble_and_clint(o);
            assert(r.name == "Monster Struct with int double int");
            assert(r.connection.ip.ipv6 == 456);
            assert(r.connection.ip.ipv4 == 12300065);
            assert(r.connection.timeout == 456.5);
            assert(r.components.pphh.size() == 3);
              assert(r.components.pphh[0] == 1);
              assert(r.components.pphh[1] == 1+1);
              assert(r.components.pphh[2] == 1+2);
            assert(r.components.pppp.size() == 3);
              assert(r.components.pppp[0] == 4);
              assert(r.components.pppp[1] == 4+1);
              assert(r.components.pppp[2] == 4+2);
            assert(r.components.hhhh.size() == 3);
              assert(r.components.hhhh[0] == 7);
              assert(r.components.hhhh[1] == 8);
              assert(r.components.hhhh[2] == 9);
            assert(r.components.lens.size() == 3);
              assert(r.components.lens[0] == 3);
              assert(r.components.lens[1] == 3);
              assert(r.components.lens[2] == 3);
            assert(*r.in.date == 121212);
            assert(r.lens.size() == 2);
            assert(r.lens[0] == 5.5);
            assert(r.lens[1] == 6.5);
            )

  return 0;
}
