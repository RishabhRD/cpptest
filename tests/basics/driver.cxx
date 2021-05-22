#include <cassert>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include <cpptest/cpptest.hxx>
#include <cpptest/version.hxx>

using namespace cpptest;

#define exp(x) [&] { x }

test_suite s1 = [] {
  test("Obvious truth") = [] {
    require(42 == 42);
    require(equals(42, 42));
  };

  test("Obvious lie") = [] {
    int n = 4;
    check(42 == 43);
    check(equals(42, 43));

    subtest("Even more lie") = [] {
      check(not_equals(std::string_view("sample_text"),
                       std::string_view("sample_text")));
    };

    disable +
        subtest("Lie including outside") = [=] { check(not_equals(n, 4)); };

    subtest("Exception tests") = [] {
      check(throws(exp(int n = 4;)));

      check(throws<std::runtime_error>([] { int n = 4; }));

      check(no_throws([] { throw 2; }));
    };
  };

  test("Complex lie") = [] {
    check(equals(1, 1) && not_equals(1, 1));
    check(!equals(1, 1));
  };
};

test_suite s2 = [] {
  // 2 3 4 5 10 11 should be output
  test("require_tests") = [] {
    check(equals(1, 2));
    subtest("sub 1") = [] {
      check(equals(1, 3));
      subtest("sub 2") = [] {
        check(equals(1, 4));
        require(equals(1, 5));
        check(equals(1, 6));
        subtest("sub 3") = [] {
          check(equals(1, 7));
          require(equals(1, 8));
        };
        check(equals(1, 9));
      };
      check(equals(1, 10));
    };
    check(equals(1, 11));
  };
};

test_suite s3 = [] {
  // expected output: 2 5 8
  "main test"_test = [] {
    subtest("sub 1") = [] {
      check(equals(1, 2));
      throw std::runtime_error("This is with message");
      check(equals(1, 3));
      subtest("sub 2") = [] { check(equals(1, 4)); };
    };

    subtest("sub 3") = [] {
      check(equals(1, 5));
      throw std::string_view("This is without message");
      check(equals(1, 6));
      subtest("sub 4") = [] { check(equals(1, 7)); };
    };
    check(equals(1, 8));
  };
};

test_suite s4 = []{
  tag("nightly") +
  "tagged_test"_test = []{
    check(equals(1, 2));
  };
};

int main(int argc, char **argv) { cpptest::run(argc, argv); }
