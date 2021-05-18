#include <cassert>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include <cpptest/cpptest.hxx>
#include <cpptest/version.hxx>

using namespace cpptest;
using namespace cpptest::assertions;
using namespace cpptest::details;

#define exp(x) [&] { x }

test_suite primary_tests = [] {
  "Obvious Truth"_test = [] {
    require(1 < 2);
    require_not_equals(1, 2);
  };

  subtest("inside") = []{
    require(1 == 3);
  };

  "Obvious Lie"_test = [] { require(1 > 2); };
};

auto other_tests = test_suite([] {
    "Not equals"_test = []{
      require_not_equals(1, 4);
    };
    test("Equals") = []{
      require_equals(1, 1);
    };
});

int main(int argc, char **argv) { cpptest::run(); }
