#pragma once
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <source_location>
#include <string>
#include <tuple>

namespace cpptest {

struct test_state {
  unsigned int _total_assertions = 0;
  unsigned int _assertion_failed = 0;
  unsigned int _assertion_passed = 0;
  unsigned int _assertion_skipped = 0;
  unsigned int _unhandled_exceptions = 0;
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
  unsigned int _total_assertions = 0;
  unsigned int _assertion_failed = 0;
  unsigned int _assertion_skipped = 0;
  unsigned int _assertion_passed = 0;
  unsigned int _unhandled_exceptions = 0;

  test_state _test_state;

  test_suite_state(const test_suite_state &) = delete;
  test_suite_state(test_suite_state &&) = delete;
  test_suite_state &operator=(const test_suite_state &) = delete;
  test_suite_state &operator=(test_suite_state &&) = delete;

  static test_suite_state &instance(std::ostream &os = std::cout) {
    static test_suite_state state(os);
    return state;
  }

private:
  test_suite_state(std::ostream &os) : os(os) {}
};

template <bool b, std::enable_if_t<b>> void test_assert() {}

std::string get_line_content(const char *file_name, int line) {
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

void log_error(std::string_view message, const std::source_location &where) {
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

/* template <typename T, typename U> */
/* void log_error(std::string_view message, T&& expected, U&& actual, const
 * std::source_location &where){ */
/*   std::cout << "\n\n\n" */
/*             << message << '\n' */
/*             << where.file_name() << "(" << where.line() << ":" <<
 * where.column() */
/*             << ") `" << '\n' */
/* } */

void _update_test_state(const bool expr, const std::source_location &where =
                                             std::source_location::current()) {
  auto &state = test_suite_state::instance();
  try {
    if (!expr) {
      state._assertion_failed++;
      state._test_state._assertion_failed++;
      log_error("Assertion Failed", where);
    } else {
      state._assertion_passed++;
      state._test_state._assertion_passed++;
    }
    return;
  } catch (const std::exception &ex) {
    std::string msg = "Uncaught Exception with message ";
    msg += ex.what();
    log_error(msg, where);
  } catch (...) {
    log_error("Uncaught Exception", where);
  }
  state._assertion_failed++;
  state._test_state._assertion_failed++;
  state._unhandled_exceptions++;
  state._test_state._unhandled_exceptions++;
}

void require(const bool expr, const std::source_location &where =
                                  std::source_location::current()) {
  auto &state = test_suite_state::instance();
  state._total_assertions++;
  state._test_state._total_assertions++;
  if (state._test_state._assertion_failed > 0 ||
      state._test_state._unhandled_exceptions > 0) {
    state._assertion_skipped++;
    state._test_state._assertion_skipped++;
    return; // Don't test more if the prev condition was failed
  }
  _update_test_state(expr, where);
}

void check(const bool expr, const std::source_location &where =
                                std::source_location::current()) {
  auto &state = test_suite_state::instance();
  state._total_assertions++;
  state._test_state._total_assertions++;
  _update_test_state(expr, where);
}

void require_false(const bool expr, const std::source_location &where =
                                        std::source_location::current()) {
  auto &state = test_suite_state::instance();
  require(!expr, where);
}

void check_false(const bool expr, const std::source_location &where =
                                      std::source_location::current()) {
  auto &state = test_suite_state::instance();
  check(!expr, where);
}

void require_throws(
    const std::invocable auto lambda,
    const std::source_location &where = std::source_location::current()) {
  auto &state = test_suite_state::instance();
  state._total_assertions++;
  state._test_state._total_assertions++;
  try {
    lambda();
  } catch (...) {
    return;
  }
  state._assertion_failed++;
  state._test_state._assertion_failed++;
  log_error("Required Throws", where);
}

void require_no_throws(
    const std::invocable auto lambda,
    const std::source_location &where = std::source_location::current()) {
  auto &state = test_suite_state::instance();
  state._total_assertions++;
  state._test_state._total_assertions++;
  try {
    lambda();
    return;
  } catch (...) {
  }
  state._assertion_failed++;
  state._test_state._assertion_failed++;
  log_error("Required NoThrow", where);
}

template <typename ExceptionType>
void require_throws_with(
    std::invocable auto lambda,
    const std::source_location &where = std::source_location::current()) {
  auto &state = test_suite_state::instance();
  state._total_assertions++;
  state._test_state._total_assertions++;
  try {
    lambda();
  } catch (ExceptionType &&ex) {
    return;
  } catch (...) {
  }
  state._assertion_failed++;
  state._test_state._assertion_failed++;
  log_error("Didn't throw required exception", where);
}

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
    state.os << "Assertions Skipped: " << state._assertion_skipped << "\n";
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

void run() { test_set::instance().run_all_tests(); }

} // namespace cpptest
