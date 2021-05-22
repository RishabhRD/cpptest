# Tutorial

To start with, you just need to download the latest version of cpptest which
is just single header and include it in your source file.

This tutorial assumes you have cpptest.hxx file.

## First Example

Let's say we have written a factorial function and we want to test it.

```cpp
int factorial(int number) { return number <= 1 ? number : factorial(number - 1) * number; }
```

So, complete test would look like this:

```cpp
// in test.cpp

#include "cpptest.hxx" // will be replaced with import in future

using namespace std;

int factorial(int number) { return number <= 1 ? number : factorial(number - 1) * number; }

test_suite s1 = []{
  "testing the factorial function"_test = []{
    check(equals(factorial(1), 1));
    check(equals(factorial(2), 2));
    check(equals(factorial(3), 6));
    check(equals(factorial(10), 3628800));
  };
};

int main(int argc, char** argv){
  run(argc, argv)
}
```

This would compile and run. It would print the summary of tests and assertions.

But what about factorial(0).... that's an edge case. Let's test it.

```cpp
  "testing the factorial function"_test = []{
    check(equals(factorial(0), 1));
    check(equals(factorial(1), 1));
    check(equals(factorial(2), 2));
    check(equals(factorial(3), 6));
    check(equals(factorial(10), 3628800));
  };
```

Now it fails. And we get the errors occured like this:

```
test.cpp:10 FAILED
  TEST CASE: testing the factorial function
  test.cpp:11: ERROR [ ( 0 == 1 )]
```

So, let's correct the code:
```cpp
int factorial(int number) { return number > 1 ? factorial(number - 1) * number : 1; }
```

Now, all tests passed.

## Under the hood

- We declare a test\_suite named s1. test\_suite can be viewed as containers
that holds tests inside it. Different kinds of tests can be seperated in
different test\_suites. ( Even in different files ). A test suite
accepts a lambda (or any other callable) whose body contains tests.


- Then we declare a test with description: "testing the factorial function".
The syntax of test utilizes C++11 literals under the hood. We can also
declare tests like ``test("Some test") = []{...}``. Like test suites, tests
also accepts a lambda (or any other callable). Unlike test suites, tests doesn't
have a variable name.

- Then we do some assertions using check. check accepts any boolean
expression (or convertible to boolean expression).

- equals is a predefined expression in cpptest that is implicitly convertible
to boolean. equals can accept 3 parameters: LHS, RHS and Comparator.
Here Comparator is hidden and defaulted to ``==``.
Use of equals predefined expression helps in printing errors precisely as
logger is configured for it.

- Then comes the main function. The main function should be present in one of
the files of a unit test group. (Our build system defines the group ussually for us).

## Adding subtests

We may want to have multiple subtests for a single test each doing some aspect
of testing. This can be acheived as:

```cpp
test("vectors can be sized and resized") = []{
  std::vector<int> v(5);

  require(v.size() == 5);
  require(greater_equals(v.capacity(), 5));

  subtest("adding to the vector increases it's size") = [=]() mutable{
    v.push_back(1);

    check(equals(v.size(), 6));
    check(greater_equals(v.capacity(), 6));
  };

  subtest("reserving increases just the capacity") = [=]() mutable{
    v.reserve(6);

    check(equals(v.size(), 5));
    check(greater_equals(v.capacity(), 6));
  };
};
```

We don't want array declared to be changed in both subtests so we capture
v by value in lambda. This construct also enables many lambda tricks into
tests.

## Nesting of subtests

The power of subtest appears when we are actually able to nest them seamlessly
(limited by your stack size).

```cpp
"sometest"_test = []{
  subtest("sub") = []{
    require(equals(1, 2));
    check(equals(1, 3));
    subtest("") = []{
      check(equals(1, 4));
      subtest("") = []{
        check(equals(1, 5));
      };
    };
  }
  check(equals(1, 6));
};
```
## Require assertion

A require assertion looks like this:

```cpp
"sometest"_test = []{
  subtest("sub") = []{
    require(equals(1, 2));
    check(equals(1, 3));
  }
  check(equals(1, 4));
};
```

If require assertion fails, then the current test or subtest and all its
subtests are aborted. That means the check assertion inside subtest would
not be executed. However, check outside the subtest would be executed.

## Other syntax

cpptest supports different standard testing syntax.

### BDD syntax

```cpp
"vector resize"_test = [] {
  feature("vector") = []{
    scenario("size") = []{
      given("Intial Vector") = [] {
        std::vector<int> v(5);
        require(equals(5, std::size(v)));

        when("resize bigger") = [=]() mutable{
          v.resize(10);

          then("Size should increase") = [=] {
            check(equals(10, std::size(v));
          };
        };
      };
    };
  };
};
```

### TDD syntax

```cpp
describe("vector") = [] {
  std::vector<int> v(5);
  require(equals(5, v.size()));

  it("should resize bigger") = [=]() mutable {
    v.resize(10);
    check(equals(10, std::size(v));
  };
};
```

## Parameterized tests

Some times we need to do same tests but with different argument. C++ is
written to make code reusable.

```cpp
std::vector<int> vec = {1, 2, 3, 4, 5};

for(auto i : vec){
  test("Parameterized " + std::to_string(i)) = [=]{
    check(greater(i, 0));
  };
}
```

## Scale it

Till now, it works great for small projects. But what about big projects.
We have many components. And every components need to be unit tested.

Ussually people split testing of different components of project in different
files. We define one testing component as one test suite. The test suites
can be in same file or different file. For example:

```cpp
// In numbers.cpp
#include <cpptest.hxx>
test_suite numbers = []{
  "equals_test"_test = []{
    check(equals(1, 1));
  };

  "not_equals_test"_test = []{
    check(not_equals(1, 2));
  };
};
```

```cpp
// In exceptions.cpp
#include <cpptest.hxx>
test_suite exceptions = []{
  "throws_test"_test = []{
    check(throws([]{throw 2;}));
    check(throws<double>([]{throw 2.5;}));
  };

  "no throw"_test = []{
    check(no_throws([]{}));
  };
};
```

```cpp
// In main.cpp
#include <cpptest.hxx>
int main(int argc, char** argv){
  run(argc, argv);
}
```

If these 3 units are compiled together to get the binary, all tests would
automatically be registered. Do I need to do it manually? Your build system
is just for this thing. Your build system should be define targets that
consideres these files.

Usually we put these kind of files in tests(unit\_tests ,..) directory or any
of its subdirectory to define a test unit. \(It highly depends on your build
system configuration.\)


## Tagging tests

Having different tags enables us to run only tests we require currently to run
instead of running all tests all the time. It saves a lot of time in
Test Drived Developement because running all tests all the time can be so
time consuming.

```cpp
#include <cpptest.hxx>

test_suite s1 = []{

  "compare with 1"_test = []{
    check(equals(1, 1));
  };

  tag("with2") +
  "compare with 2"_test = []{
    check(equals(1, 2));
  };

  tag("with3") +
  "compare with 3"_test = []{
    check(equals(1, 3));
  };

  tag("with2") + tag("with3") +
  "compare with 2 and 3"_test = []{
    check(equals(1, 2));
    check(equals(1, 3));
  };
};

int main(int argc, char** argv){
  run(argc, argv);
}
```

First test is without any tags. Second test is ``with2`` tagged. Third test is
``with3`` tagged. And Fourth test is both ``with2`` and ``with3`` tagged.

``run(argc, argv);`` makes cpptest to accepts tags as command line argument.
If no command line argument is given all tests are run.

If you have binary ``test`` then:

```
./test
```
would run all tests.

```
./test with2
```
would run second and fourth tests.

```
./test with3
```
would run third and fourth tests.

```
./test with2 with3
```
would run second, third and fourth tests.



### Disabling tests

Instead of commenting some tests or removing them if you want to disable
some specific tests temporarily, you can actually tag them ``disable``.

This special ``disable`` tag would disable them from running.

```cpp
disable +
"some test"_test = []{
  check( 1 == 1 );
};

disable + tag("sometag") +
"some_other_test" = []{
  check( 2 == 2 );
};

"running_test"_test = []{
  check( 3 == 3 );
};
```

## Expressions

Using expression in check/require assertion makes the error report more verbose.
cpptest supports following expressions:

### equals

Syntax: equals(lhs, rhs, comparator)

Defaults: comparator -> lhs == rhs

Examples:
```cpp
requires(equals(1, 1));

requires(equals(1, 0, [](auto l, auto r){
      reutrn l != r;
      });
```

### not\_equals

Syntax: not\_equals(lhs, rhs, comparator)

Defaults: comparator -> lhs != rhs

### greater

Syntax: greater(lhs, rhs, comparator)

Defaults: comparator -> lhs > rhs

### greater\_eq

Syntax: greater\_eq(lhs, rhs, comparator)

Defaults: comparator -> lhs >= rhs

### lesser

Syntax: lesser(lhs, rhs, comparator)

Defaults: comparator -> lhs < rhs

### lesser\_eq

Syntax: lesser\_eq(lhs, rhs, comparator)

Defaults: comparator -> lhs <= rhs

### throws

Syntax: throws(callable) or throws<type>(callable)

Callable is any C++ entity that can be called wihout any parameter.

throws(callable) is true when calling callable throws any exception.

throws<type>(callable) is truen when calling callable throws exception
of type ``type``.

Example:

```cpp
"some test"_test = []{
  check(throws([]{ throw 2; }));
  check(throws<double>([] { throw 2.4; }));
};
```

### no\_throws

Syntax: nothrows(callable)

Callable is any C++ entity that can be called without any parameter.

no\_throws(callable) is true when calling callable doesn't throw any exception.

Example:

```cpp
"some test"_test = []{
  check(no_throws([]{int n = 4;}));
};
```

### AND, OR and NOT operators

Syntax:

- expr1 && expr2
- expr1 || expr2
- !(expr1)

  expr1 and expr2 should be expressions defined by cpptest.

- ``expr1 && expr2`` is true when both expr1 and expr2 are true.

- ``expr1 || expr2`` is true when any of expr1 or expr2 is true.

- ``!expr1`` is true when expr1 is false.

Example:

```cpp
"some test"_test = []{
  check(equals(1, 1) && equals(2, 2));
  check(equals(1, 1) || equals(1, 2));
  check(!equals(1, 2));
  check((equals(1, 1) && equals(2, 1)) || (!equals(1, 2)));
};
```

## Integrations with build systems

### build2

build2 is a modern C++ build system. cpptest is built over build2 itself.
Integrations with build2 is easy.

Let's assume we have a test directory where we want to put all our unit
tests (to get single test binary). There should be only one file in these
files which contains the main function that calls run function.

Content of ``buildfile`` inside test directory can be like:

```
tags =

exe{driver}: {hxx ixx txx cxx}{**}
exe{driver}: test.arguments = $tags
```

Note: we are assuming that cpptest.hxx is available. We can use build2 for
getting cpptest but those lines has been hidden from example.

With these contents we can run tests like this:

```
b test
```

This builds(if updated) and runs all the tests in tests directory (and its sudirectories).

```
b tags="with2 with3" test
```

This runs only those tests in test directory (and its subdirectories) that are
tagged with either with2 or with3 or both.
