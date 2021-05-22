# cpptest

A feature-rich Unit Testing Framework for modern C++20 codebase.

![](https://user-images.githubusercontent.com/26287448/119237528-eba76900-bb5a-11eb-8c6d-f8f1f3ea260a.gif)

C++20 is a major release of C++ after C++11. It would highly impact the way of
writing codes in future. Adoption of C++20 in codes highly depends on available
toolchains for it. Unit Testing library and build system is the first thing
that comes to my mind.

Older macro based libraries' macros doesn't follow the scoping rules. So, I
decided to write a full unit-testing framework with modern C++ features like
lambdas, literals, templates...

## Features

- No Depedency (A C++20 supported compiler is only requirement)
- Single header/module (Easy to integrate)
- Automatic test registration
- Macro free
- Higly Extensible and customizable (Change loggers, printers or even handlers)
- Parameterized tests
- Also Support for TDD/BDD workflows
- Easy integration with build systems (build2, cmake)

## Inspired From

  - Catch2
  - doctest
  - boost.test
  - ut
  and many more

## Documentation

### Usage

- [Tutorial (A quick start)](docs/Tutorial.md)
