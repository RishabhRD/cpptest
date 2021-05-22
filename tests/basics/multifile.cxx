#include <cpptest/cpptest.hxx>
using namespace cpptest;
test_suite s5 = []{
  tag("nightly") +
  "tagged_test"_test = []{
    check(equals(1, 3));
  };
};
