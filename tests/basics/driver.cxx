#include <cassert>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include <cpptest/cpptest.hxx>
#include <cpptest/version.hxx>

using namespace cpptest;

auto primary_tests = test_suite([] {
  test_case("Obvious truth", []() { require(1 < 2); });
  test_case("Obvious lie", []() { require(2 < 1); });
});

int main(int argc, char **argv) { cpptest::run(); }
