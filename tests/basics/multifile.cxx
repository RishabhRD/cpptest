#include <cpptest/cpptest.hxx>
using namespace cpptest;
test_suite s4 = []{
  tag("nightly") +
  "tagged_test"_test = []{
    check(equals(1, 2));
  };
};
