#include <iostream>
#include <cassert>
#include <sstream>
#include <stdexcept>

#include <cpptest/version.hxx>
#include <cpptest/cpptest.hxx>

auto x = cpptest::test_case("Hello", [](){});

int main(int argc, char** argv){
  cpptest::main(argc, argv);
}
