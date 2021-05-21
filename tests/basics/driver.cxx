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

    subtest("Lie including outside") = [=]{
      check(not_equals(n, 4));
    };
  };

};


int main(int argc, char **argv) { cpptest::run(); }
