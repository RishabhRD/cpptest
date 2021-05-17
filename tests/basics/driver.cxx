#include <cassert>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include <cpptest/cpptest.hxx>
#include <cpptest/version.hxx>

using namespace cpptest;
using namespace cpptest::assertions;

#define exp(x) [&] { x }

auto primary_tests = test_suite([] {
  test_case("Obvious truth", []() { require(1 < 2); });
  test_case("Obvious lie", []() { require(2 < 1); });
});

auto equal_tests = test_suite([] {
  test_case("Equality Obvious truth", []() {
    require_equals(1, 2);
    /* require_equals(1, 2); */
    /* require_no_throws(exp(throw "helo";)); */
  });
});

int main(int argc, char **argv) { cpptest::run(); }
