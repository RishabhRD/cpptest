#pragma once
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <source_location>
#include <string>
#include <tuple>

namespace cpptest {

template <class T> concept Printable = requires(std::ostream &os, T a) {
  { os << a }
  ->std::same_as<std::ostream &>;
};

template <typename T, typename U> concept EqualComparable = requires(T t, U u) {
  { t == u }
  ->std::same_as<bool>;
};

template <typename T, typename U>
concept InequalComparable = requires(T t, U u) {
  { t != u }
  ->std::same_as<bool>;
};

struct test_state {
  bool _to_break = false;
  bool _already_failed = false;
  std::string name;

  test_state(test_state &&state) = default;
  test_state &operator=(const test_state &state) = default;
  test_state(const test_state &state) = default;
  test_state &operator=(test_state &&state) = default;
  test_state() = default;
};
struct test_suite_state {

  std::ostream &os;

  unsigned int _total_tests = 0;
  unsigned int _total_tests_failed = 0;
  unsigned int _total_assertions = 0;
  unsigned int _assertion_failed = 0;
  unsigned int _assertion_passed = 0;
  unsigned int _unhandled_exceptions = 0;

  test_state _test_state;

  test_suite_state(const test_suite_state &) = delete;
  test_suite_state(test_suite_state &&) = delete;
  test_suite_state &operator=(const test_suite_state &) = delete;
  test_suite_state &operator=(test_suite_state &&) = delete;

  bool to_break() { return _test_state._to_break; }

  void assertion_passed() { _assertion_passed++; }

  void assertion_added() { _total_assertions++; }

  void assertion_failed(bool to_break, bool exception = false) {
    if (to_break) {
      _test_state._to_break = true;
    }
    _assertion_failed++;
    if (!_test_state._already_failed) {
      _test_state._already_failed = true;
      _total_tests_failed++;
    }
    if (exception) {
      _unhandled_exceptions++;
    }
  }

  static test_suite_state &instance(std::ostream &os = std::cout) {
    static test_suite_state state(os);
    return state;
  }

private:
  test_suite_state(std::ostream &os) : os(os) {}
};

inline std::string get_line_content(const char *file_name, int line) {
  std::ifstream file(file_name);
  if (file.is_open()) {
    file.seekg(std::ios::beg);
    for (int i = 0; i < line - 1; ++i) {
      file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }
    std::string line;
    std::getline(file, line);
    return line;
  } else {
    return "";
  }
}

namespace logging {

inline void log_error(const std::string_view message,
                      const std::source_location &where) {
  test_suite_state &state = test_suite_state::instance();
  state.os << "\n\n\n"
           << message << ':' << '\n'
           << "Test Case: " << state._test_state.name << '\n'
           << where.file_name() << "(" << where.line() << ":" << where.column()
           << ") `" << '\n';
  std::string line = get_line_content(where.file_name(), where.line());
  if (!line.empty()) {
    state.os << "---> " << get_line_content(where.file_name(), where.line())
             << '\n';
  }
}

template <Printable Expected, Printable Actual>
void log_error(const std::string_view message, Expected &&expected,
               Actual &&actual, const std::source_location &where) {
  test_suite_state &state = test_suite_state::instance();
  state.os << "\n\n\n"
           << message << ':' << '\n'
           << "Test Case: " << state._test_state.name << '\n'
           << "Expected: " << expected << '\n'
           << "Actual: " << actual << '\n'
           << where.file_name() << "(" << where.line() << ":" << where.column()
           << ") `" << '\n';
  std::string line = get_line_content(where.file_name(), where.line());
  if (!line.empty()) {
    state.os << "---> " << get_line_content(where.file_name(), where.line())
             << '\n';
  }
}

template <typename Expected, typename Actual>
void log(const std::string_view message, Expected &&exptected, Actual &&actual,
         const std::source_location &where) {
  if constexpr (Printable<Expected> && Printable<Actual>) {
    log_error(message, exptected, actual, where);
  } else {
    log_error(message, where);
  }
}
}; // namespace logging

namespace assertions {

template <typename... Args, typename Lambda>
requires std::invocable<Lambda, const bool, Args...,
                        const std::source_location &> inline constexpr auto
make_assert(const bool to_break, Lambda &&lambda) {
  return [to_break, lambda = std::forward<Lambda>(lambda)](
             Args &&...args, const std::source_location &where =
                                 std::source_location::current()) {
    auto &state = test_suite_state::instance();
    state.assertion_added();
    if (to_break) {
      if (state.to_break()) {
        return;
      }
    }
    lambda(to_break, std::forward<Args>(args)..., where);
  };
}

inline void _assert(const bool to_break, const bool expr,
                    const std::source_location &where) {
  auto &state = test_suite_state::instance();
  try {
    if (!expr) {
      state.assertion_failed(to_break);
      logging::log_error("Assertion Failed", where);
    } else {
      state.assertion_passed();
    }
    return;
  } catch (const std::exception &ex) {
    std::string msg = "Uncaught Exception with message ";
    msg += ex.what();
    logging::log_error(msg, where);
  } catch (...) {
    logging::log_error("Uncaught Exception", where);
  }
  state.assertion_failed(to_break, true);
}

inline void _assert_false(const bool to_break, const bool expr,
                          const std::source_location &where) {
  _assert(to_break, !expr, where);
}

inline void assert_throws(const bool to_break, const std::invocable auto lambda,
                          const std::source_location &where) {
  auto &state = test_suite_state::instance();
  try {
    lambda();
  } catch (...) {
    state.assertion_passed();
    return;
  }
  state.assertion_failed(to_break);
  logging::log_error("Required Throws", where);
}

inline void assert_no_throws(const bool to_break,
                             const std::invocable auto lambda,
                             const std::source_location &where) {
  auto &state = test_suite_state::instance();
  try {
    lambda();
    state.assertion_passed();
    return;
  } catch (...) {
  }
  state.assertion_failed(to_break);
  logging::log_error("Required NoThrow", where);
}

template <typename ExceptionType>
void assert_throws_with(
    const bool to_break, std::invocable auto &&lambda,
    const std::source_location &where = std::source_location::current()) {
  auto &state = test_suite_state::instance();
  try {
    lambda();
  } catch (ExceptionType &&ex) {
    return;
    state.assertion_passed();
  } catch (...) {
  }
  state.assertion_failed(to_break);
  logging::log_error("Didn't throw required exception", where);
}

template <typename Expected, typename Actual>
requires(EqualComparable<Expected, Actual>) inline void assert_equals(
    const bool to_break, Expected&& expected, Actual&& actual,
    const std::source_location &where) {
  auto &state = test_suite_state::instance();
  try {
    if (!(expected == actual)) {
      state.assertion_failed(to_break);
      logging::log("Equality Assertion Failed", expected, actual, where);
    } else {
      state.assertion_passed();
    }
    return;
  } catch (const std::exception &ex) {
    std::string msg = "Uncaught Exception with message ";
    msg += ex.what();
    logging::log(msg, expected, actual, where);
  } catch (...) {
    logging::log("Uncaught Exception", expected, actual, where);
  }
  state.assertion_failed(to_break, true);
}

template <typename Expected, typename Actual>
requires(InequalComparable<Expected, Actual>) inline void assert_not_equals(
    bool to_break, Expected &&expected, Actual &&actual,
    const std::source_location &where) {
  auto &state = test_suite_state::instance();
  try {
    if (!(expected != actual)) {
      state.assertion_failed(to_break);
      logging::log("Inequality Assertion Failed", expected, actual, where);
    } else {
      state.assertion_passed();
    }
    return;
  } catch (const std::exception &ex) {
    std::string msg = "Uncaught Exception with message ";
    msg += ex.what();
    logging::log(msg, expected, actual, where);
  } catch (...) {
    logging::log("Uncaught Exception", expected, actual, where);
  }
  state.assertion_failed(to_break, true);
}

inline constexpr auto require = make_assert<const bool>(true, _assert);

inline constexpr auto check = make_assert<const bool>(false, _assert);

inline constexpr auto require_false =
    make_assert<const bool>(true, _assert_false);

inline constexpr auto check_false =
    make_assert<const bool>(false, _assert_false);

template <std::invocable Lambda>
inline constexpr auto require_throws = make_assert<Lambda>(true,
                                                              assert_throws<Lambda>);
template <std::invocable Lambda>
inline constexpr auto check_throws = make_assert<Lambda>(false,
                                                            assert_throws<Lambda>);

template <std::invocable Lambda>
inline constexpr auto
    require_no_throws = make_assert<Lambda>(true, assert_no_throws<Lambda>);

template <std::invocable Lambda>
inline constexpr auto
    check_no_throws = make_assert<Lambda>(false, assert_no_throws<Lambda>);

template <typename ExceptionType, std::invocable Lambda>
inline constexpr auto
    require_throws_with = make_assert<Lambda>(true, assert_throws_with<Lambda>);

template <typename ExceptionType, std::invocable Lambda>
inline constexpr auto
    check_throws_with = make_assert<Lambda>(false, assert_throws_with<Lambda>);

template <typename Expected, typename Actual>
inline constexpr auto
    require_equals = make_assert<Expected, Actual>(true, assert_equals<Expected, Actual>);

template <typename Expected, typename Actual>
inline constexpr auto
    check_equals = make_assert<Expected, Actual>(false, assert_equals<Expected, Actual>);

template <typename Expected, typename Actual>
inline constexpr auto require_not_equals =
    make_assert<Expected, Actual>(true, assert_not_equals<Expected, Actual>);

template <typename Expected, typename Actual>
inline constexpr auto check_not_equals =
    make_assert<Expected, Actual>(false, assert_not_equals<Expected, Actual>);
}; // namespace assertions

struct _test {
  std::ostream &os;
  std::function<void()> func;
  test_suite_state &state;
  std::string name;
  test_state _test_state;

  template <std::invocable Func>
  _test(const std::string &test_name, Func &&func, std::ostream &os = std::cout)
      : os(os), func(std::forward<Func>(func)),
        state(test_suite_state::instance(os)), name(test_name) {
    _test_state.name = name;
    state._total_tests++;
  }

  template <std::invocable Func>
  _test(std::string &&test_name, Func &&func, std::ostream &os = std::cout)
      : os(os), func(std::forward<Func>(func)),
        state(test_suite_state::instance(os)), name(std::move(test_name)) {
    _test_state.name = name;
    state._total_tests++;
  }

  void operator()() const { func(); }
};

struct test_set {
  void add_test(const _test &test_case) { tests.push_back(test_case); }

  void add_test(_test &&test_case) { tests.push_back(std::move(test_case)); }

  void run_all_tests() {
    for (auto &current_test : tests) {
      state._test_state = std::move(current_test._test_state);
      current_test();
      current_test._test_state = std::move(state._test_state);
    }
    print_test_results();
  }

  test_set(const test_set &) = delete;
  test_set(test_set &&) = delete;
  test_set &operator=(const test_set &) = delete;
  test_set &operator=(test_set &&) = delete;

  static test_set &instance() {
    static test_set ts;
    return ts;
  }

private:
  test_set(){};
  test_suite_state &state = test_suite_state::instance();
  std::vector<_test> tests;

  void print_test_results() {
    state.os << "\n\n";
    state.os << "Total tests : " << state._total_tests << "\n";
    state.os << "Total Assertions : " << state._total_assertions << "\n";
    state.os << "Assertions Passed: " << state._assertion_passed << "\n";
    state.os << "Assertions Failed: " << state._assertion_failed << "\n";
    state.os << "Total Unhanded Exceptions: " << state._unhandled_exceptions
             << "\n";
  }
};

template <std::invocable Func>
void test_case(const std::string &desc, Func &&func,
               std::ostream &os = std::cout) {
  auto current_test_case = _test(desc, std::forward<Func>(func), os);
  test_set::instance().add_test(std::move(current_test_case));
}

template <std::invocable Func>
void test_case(std::string &&desc, Func &&func, std::ostream &os = std::cout) {
  auto current_test_case = _test(std::move(desc), std::forward<Func>(func), os);
  test_set::instance().add_test(std::move(current_test_case));
}

template <std::invocable Func> struct _test_suite {

  _test_suite(Func &&func) : func(std::move(func)) {}
  _test_suite(const Func &func) : func(func) {}

  void operator()() { func(); }

private:
  Func func;
};

struct test_suite {
  template <std::invocable Func> test_suite(Func &&func) {
    auto suite = _test_suite(std::forward<Func>(func));
    suite();
  }
};

inline void run() { test_set::instance().run_all_tests(); }

} // namespace cpptest
