#pragma once
#include <functional>
#include <iostream>
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
  std::string_view name;

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

void _update_test_state(const bool expr, const std::source_location &where =
                                             std::source_location::current()) {
  auto &state = test_suite_state::instance();
  try {
    if (!expr) {
      state._assertion_failed++;
      state._test_state._assertion_failed++;
      // TODO: logging
    } else {
      state._assertion_passed++;
      state._test_state._assertion_passed++;
    }
    return;
  } catch (const std::exception &ex) {
    // TODO: logging
  } catch (...) {
    // TODO: logging
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
  // TODO: log here
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
  // TODO: log here
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
  // TODO: log here
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
    state._total_tests++;
  }

  template <std::invocable Func>
  _test(std::string &&test_name, Func &&func, std::ostream &os = std::cout)
      : os(os), func(std::forward<Func>(func)),
        state(test_suite_state::instance(os)), name(std::move(test_name)) {
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

  void print_test_results() {}
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
