#include <cassert>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include <cpptest/cpptest.hxx>
#include <cpptest/version.hxx>

using namespace cpptest;

#define exp(x) [&] { x }


test_suite s1 = []{

  test("Obvious truth") = []{
    require(42 == 42);
    require(equals(42, 42));
  };

  test("Obvious lie") = []{
    int n = 4;
    check(42 == 43);
    check(equals(42, 43));

    subtest("Even more lie") = []{
      check(not_equals(std::string_view("sample_text"), std::string_view("sample_text")));
    };

disable +
    subtest("Lie including outside") = [=]{
      check(not_equals(n, 4));
    };

    subtest("Exception tests") = []{
      check(throws(exp(int n = 4;)));

      check(throws<std::runtime_error>([]{int n = 4;}));

      check(no_throws([]{throw 2;}));
    };
  };

  test("Complex lie") = []{
    check(equals(1, 1) && not_equals(1, 1));
    check(!equals(1, 1));
  };

};


int main(int argc, char **argv) { cpptest::run(); }
