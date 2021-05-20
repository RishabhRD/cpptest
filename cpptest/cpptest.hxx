#pragma once
#include <algorithm>
#include <filesystem>
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

template <typename Expected, typename Actual, typename Compare>
concept Comparable = requires(Expected &&ex, Actual &&ac, Compare &&comp) {
  { comp(ex, ac) }
  ->std::same_as<bool>;
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

struct equal_to {
  template <typename T, typename U>
  requires EqualComparable<T, U> bool operator()(const T &t, const U &u) {
    return t == u;
  }
};

struct not_equal_to {
  template <typename T, typename U>
  requires InequalComparable<T, U> bool operator()(T &&t, U &&u) {
    return t != u;
  }
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

namespace logging {

namespace fs = std::filesystem;

inline void log_error(const std::string_view name,
                      const std::string_view message,
                      const std::source_location &where) {
  test_suite_state &state = test_suite_state::instance();
  state.os << "\n\n\n"
           << name << " " << message << ':' << '\n'
           << "Test Case: " << state._test_state.name << '\n'
           << fs::path(where.file_name()).filename().generic_string() << ":"
           << where.line() << '\n';
}

inline void log_error(const std::string_view message,
                      const std::source_location &where) {
  test_suite_state &state = test_suite_state::instance();
  state.os << "\n\n\n"
           << message << ':' << '\n'
           << "Test Case: " << state._test_state.name << '\n'
           << fs::path(where.file_name()).filename().generic_string() << ":"
           << where.line() << '\n';
}

template <Printable Expected, Printable Actual>
void log_error(const std::string_view name, const std::string_view message,
               Expected &&expected, Actual &&actual,
               const std::source_location &where) {
  test_suite_state &state = test_suite_state::instance();
  state.os << "\n\n\n"
           << name << " " << message << ':' << '\n'
           << "Test Case: " << state._test_state.name << '\n'
           << fs::path(where.file_name()).filename().generic_string() << ":"
           << where.line() << '\n'
           << "Expected: " << expected << '\n'
           << "Actual: " << actual << '\n';
}

template <typename Expected, typename Actual>
void log(const std::string_view name, const std::string_view message,
         Expected &&exptected, Actual &&actual,
         const std::source_location &where) {
  if constexpr (Printable<Expected> && Printable<Actual>) {
    log_error(name, message, exptected, actual, where);
  } else {
    log_error(name, message, where);
  }
}

}; // namespace logging

namespace assertions {

template <typename Expected, typename Actual>
inline void assert_boilerplate(const bool to_break, const bool expr,
                               Expected &&expected, Actual &&actual,
                               std::string_view name,
                               const std::source_location &where) {
  auto &state = test_suite_state::instance();
  state.assertion_added();
  if (to_break) {
    if (state.to_break()) {
      return;
    }
  }
  try {
    if (!expr) {
      state.assertion_failed(to_break);
      logging::log(name, "Failed", std::forward<Expected>(expected),
                   std::forward<Actual>(actual), where);
    } else {
      state.assertion_passed();
    }
    return;
  } catch (const std::exception &ex) {
    std::string msg = "Exception caught with message ";
    msg += ex.what();
    logging::log(name, msg, expected, actual, where);
  } catch (...) {
    logging::log(name, "Exception caught", expected, actual, where);
  }
  state.assertion_failed(to_break, true);
}

inline void _assert(const bool to_break, const bool expr,
                    const std::source_location &where) {
  auto &state = test_suite_state::instance();
  state.assertion_added();
  if (to_break) {
    if (state.to_break()) {
      return;
    }
  }
  try {
    if (!expr) {
      state.assertion_failed(to_break);
      logging::log_error("Failed", where);
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
  state.assertion_added();
  if (to_break) {
    if (state.to_break()) {
      return;
    }
  }
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
  state.assertion_added();
  if (to_break) {
    if (state.to_break()) {
      return;
    }
  }
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
  state.assertion_added();
  if (to_break) {
    if (state.to_break()) {
      return;
    }
  }
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

template <typename Expected, typename Actual, typename Compare>
requires(Comparable<Expected, Actual, Compare>) inline void assert_equals(
    const bool to_break, Expected &&expected, Actual &&actual,
    Compare &&compare, const std::source_location &where) {
  assert_boilerplate(to_break, compare(expected, actual),
                     std::forward<Expected>(expected),
                     std::forward<Actual>(actual),
                     to_break ? "[require_equals]" : "[check_equals]", where);
}

template <typename Expected, typename Actual, typename Compare>
requires(Comparable<Expected, Actual, Compare>) inline void assert_not_equals(
    bool to_break, Expected &&expected, Actual &&actual, Compare &&compare,
    const std::source_location &where) {
  assert_boilerplate(
      to_break, compare(expected, actual), std::forward<Expected>(expected),
      std::forward<Actual>(actual),
      to_break ? "[require_not_equals]" : "[check_not_equals]", where);
}

inline void
require(const bool expr,
        const std::source_location &where = std::source_location::current()) {
  _assert(true, expr, where);
}

inline void
check(const bool expr,
      const std::source_location &where = std::source_location::current()) {
  _assert(false, expr, where);
}

inline void require_false(
    const bool expr,
    const std::source_location &where = std::source_location::current()) {
  _assert_false(true, expr, where);
}

inline void check_false(const bool expr, const std::source_location &where =
                                             std::source_location::current()) {
  _assert_false(false, expr, where);
}

template <std::invocable Lambda>
inline void require_throws(
    Lambda &&lambda,
    const std::source_location &where = std::source_location::current()) {
  assert_throws(true, std::forward<Lambda>(lambda), where);
}

template <std::invocable Lambda>
inline void check_throws(Lambda &&lambda, const std::source_location &where =
                                              std::source_location::current()) {
  assert_throws(false, std::forward<Lambda>(lambda), where);
}

template <std::invocable Lambda>
inline void require_no_throws(
    Lambda &&lambda,
    const std::source_location &where = std::source_location::current()) {
  assert_no_throws(true, std::forward<Lambda>(lambda), where);
}

template <std::invocable Lambda>
inline void check_no_throws(
    Lambda &&lambda,
    const std::source_location &where = std::source_location::current()) {
  assert_no_throws(false, std::forward<Lambda>(lambda), where);
}

template <typename ExceptionType, std::invocable Lambda>
inline void require_throws_with(
    Lambda &&lambda,
    const std::source_location &where = std::source_location::current()) {
  assert_throws_with(true, std::forward<Lambda>(lambda), where);
}

template <typename ExceptionType, std::invocable Lambda>
inline void check_throws_with(
    Lambda &&lambda,
    const std::source_location &where = std::source_location::current()) {
  assert_throws_with(false, std::forward<Lambda>(lambda), where);
}

template <typename Expected, typename Actual, typename Compare = equal_to>
inline void require_equals(
    Expected &&expected, Actual &&actual, Compare &&compare = equal_to(),
    const std::source_location &where = std::source_location::current()) {
  assert_equals(true, std::forward<Expected>(expected),
                std::forward<Actual>(actual), std::forward<Compare>(compare),
                where);
}

template <typename Expected, typename Actual, typename Compare = equal_to>
inline void check_equals(
    Expected &&expected, Actual &&actual, Compare &&compare = equal_to(),
    const std::source_location &where = std::source_location::current()) {
  assert_equals(false, std::forward<Expected>(expected),
                std::forward<Actual>(actual), std::forward<Compare>(compare),
                where);
}

template <typename Expected, typename Actual, typename Compare = not_equal_to>
inline auto require_not_equals(
    Expected &&expected, Actual &&actual, Compare &&compare = not_equal_to(),
    const std::source_location &where = std::source_location::current()) {
  assert_not_equals(true, std::forward<Expected>(expected),
                    std::forward<Actual>(actual),
                    std::forward<Compare>(compare), where);
}
template <typename Expected, typename Actual, typename Compare = not_equal_to>
inline auto check_not_equals(
    Expected &&expected, Actual &&actual, Compare &&compare = not_equal_to(),
    const std::source_location &where = std::source_location::current()) {
  assert_not_equals(false, std::forward<Expected>(expected),
                    std::forward<Actual>(actual),
                    std::forward<Compare>(compare), where);
}
}; // namespace assertions

namespace details {
struct tag {
  std::vector<std::string_view> tags;

  bool contains(std::string_view tag_name) {
    return std::find(tags.begin(), tags.end(), tag_name) == tags.end();
  }

  bool is_empty() { return tags.empty(); }

  void add(std::string_view tag_name) { tags.push_back(tag_name); }

  auto begin() { return tags.begin(); }

  auto end() { return tags.end(); }

  auto begin() const { return tags.begin(); }

  auto end() const { return tags.end(); }

  auto satisfies(const tag &t) {
    return std::find_if(t.begin(), t.end(), [this](std::string_view tag_name) {
             return contains(tag_name);
           }) != std::end(t);
  }
};

}; // namespace details

namespace events {
struct test_suite {
  std::function<void()> tests;
};

struct test {
  std::ostream &os;
  details::tag test_tag;
  std::function<void()> func;
  std::string_view name;
  test_suite_state &state;
  test_state _test_state;

  template <std::invocable Func, std::same_as<details::tag> Tag>
  test(const std::string_view test_name, Tag &&test_tag, Func &&func,
       std::ostream &os = std::cout)
      : os(os), func(std::forward<Func>(func)),
        state(test_suite_state::instance(os)), name(test_name),
        test_tag(std::forward<Tag>(test_tag)) {
    _test_state.name = name;
    state._total_tests++;
  }

  void operator()() const { func(); }
};

struct test_skipped {
  std::string_view name;
};

struct test_begin {
  std::string_view name;
};

struct test_run {
  std::string_view test;
};

struct test_end {
  std::string_view name;
};

struct summary {};

template <typename Expr> struct assertion_added {
  Expr expr;
  std::source_location location;
};

template <typename Expr> struct assertion_failed {
  Expr expr;
  std::source_location location;
};

template <typename Expr> struct assertion_passed {
  Expr expr;
  std::source_location location;
};

template <typename Msg> struct log { Msg msg; };

struct exception{
  const char* msg;
  exception(const char* msg) : msg(msg){}
  exception(const std::exception& ex) : msg(ex.what()){}
  const char* what(){
    return msg;
  }
};


}; // namespace events

namespace handlers {

template <typename Logger> class test_event_handler {
public:
  test_event_handler() = default;

  test_event_handler(const Logger &logger) : logger(logger) {}

  test_event_handler(Logger &&logger) : logger(std::move(logger)) {}

  ~test_event_handler() { logger.on(events::summary{}); }

  void on(events::test_suite suite) {
    test_suites.push_back(std::move(suite.tests));
  }

  void on(events::test &test) {
    if (test.test_tag.contains("disable")) {
      on(events::test_skipped{test.name});
    }
    if (test.test_tag.satisfies(tags)) {
      on(events::test_begin{test.name});
      on(events::test_run{test.name});
      try {
        test();
      } catch (const std::exception &ex) {
        on(events::exception{ex});
      } catch (...) {
        on(events::exception("Unknown Exception"));
      }
      on(events::test_end{test.name});
    } else {
      on(events::test_skipped{test.name});
    }
  }

  void on(events::exception ex){
    current_test_failed = true;
    logger.on(ex);
  }

  void on(events::test_skipped test){
    logger.on(test);
  }

  void on(events::test_begin test){
    if (!(test_level++)) {
      logger.on(test);
    }
  }

  void on(events::test_end test){
    if (!(--test_level)){
      logger.on(test);
    }
  }

  void on(events::test_run test){
    logger.on(test);
  }

  void run(std::vector<std::string_view> tags) {
    this->tags = tags;
    for (auto &suite : test_suites) {
      suite();
    }
  }

  void run(std::vector<std::string_view> &&tags) {
    this->tags = std::move(tags);
    for (auto &suite : test_suites) {
      suite();
    }
  }

private:
  Logger logger;
  std::size_t test_level{};
  std::vector<std::function<void()>> test_suites;
  details::tag tags;
  bool current_test_failed{false};
};

}; // namespace handlers

namespace details {

inline void on(...) {}

struct test {
  std::string_view name;
  tag test_tag;

  test(const char *name) : name(name) {}

  test(std::string_view name) : name(name) {}

  test(const char *name, decltype(sizeof("")) size) : name{name, size} {}

  template <std::invocable Func> void operator=(Func &&func) {
    auto current_test_case =
        events::test(name, std::move(test_tag), std::forward<Func>(func));
    on(current_test_case);
  }
};

template <std::invocable Func> struct _test_suite {

  _test_suite(Func &&func) : func(std::move(func)) {}
  _test_suite(const Func &func) : func(func) {}

  void operator()() { func(); }

private:
  Func func;
};

constexpr auto subtest = [](const auto name) { return test(name); };

} // namespace details

namespace operators {
inline details::tag operator+(const details::tag &firstTag,
                              const details::tag &secondTag) {
  details::tag t;
  auto insert_tags = [&t](std::string_view name) {
    if (!t.contains(name)) {
      t.add(name);
    }
  };
  std::for_each(firstTag.begin(), firstTag.end(), insert_tags);
  std::for_each(secondTag.tags.begin(), secondTag.tags.end(), insert_tags);
  return t;
}

inline details::test operator+(const details::tag &first,
                               const details::test &second) {
  details::test t(second.name);
  auto insert_tags = [&t](std::string_view name) {
    if (!t.test_tag.contains(name)) {
      t.test_tag.add(name);
    }
  };
  std::for_each(first.begin(), first.end(), insert_tags);
  return t;
}
inline auto operator""_test(const char *name, decltype(sizeof("")) size) {
  return details::test{name, size};
}
}; // namespace operators

inline details::tag tag(const char *name) { return details::tag{{name}}; }

using details::subtest;
using details::test;
using operators::operator""_test;
using operators::operator+;

} // namespace cpptest
