#pragma once
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <source_location>
#include <sstream>
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
  details::tag test_tag;
  std::function<void()> func;
  std::string_view name;
  std::source_location where;

  template <std::invocable Func, std::same_as<details::tag> Tag>
  test(const std::string_view test_name, Tag &&test_tag, Func &&func,
       const std::source_location &where)
      : func(std::forward<Func>(func)), name(test_name),
        test_tag(std::forward<Tag>(test_tag)), where(where) {}

  void operator()() const { func(); }
};

struct test_skipped {
  std::string_view name;
  std::source_location where;
};

struct test_begin {
  std::string_view name;
  std::source_location where;
};

struct test_run {
  std::string_view test;
  std::source_location where;
};

struct test_end {
  std::string_view name;
  std::source_location where;
};

struct summary {};

template <typename Expr> struct assertion_added {
  Expr expr;
  std::source_location where;
};

template <typename Expr> struct assertion_failed {
  Expr expr;
  std::source_location where;
};

template <typename Expr> struct assertion_passed {
  Expr expr;
  std::source_location where;
};

template <typename Expr> struct assertion_exception {
  Expr expr;
  std::source_location where;
  const char *msg;
};

template <typename Msg> struct log { Msg msg; };

struct exception {
  const char *msg;
  exception(const char *msg) : msg(msg) {}
  exception(const std::exception &ex) : msg(ex.what()) {}
  const char *what() { return msg; }
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
      on(events::test_skipped{test.name, test.where});
    }
    if (test.test_tag.satisfies(tags)) {
      on(events::test_begin{test.name, test.where});
      on(events::test_run{test.name, test.where});
      try {
        test();
      } catch (const std::exception &ex) {
        on(events::exception{ex});
      } catch (...) {
        on(events::exception("Unknown Exception"));
      }
      on(events::test_end{test.name, test.where});
    } else {
      on(events::test_skipped{test.name, test.where});
    }
  }

  void on(events::exception ex) {
    current_test_failed = true;
    logger.on(ex);
  }

  void on(events::test_skipped test) { logger.on(test); }

  void on(events::test_begin test) { logger.on(test); }

  void on(events::test_end test) { logger.on(test); }

  void on(events::test_run test) { logger.on(test); }

  template <typename Msg> void on(events::log<Msg> msg) { logger.on(msg); }

  template <typename Expr> void on(events::assertion_added<Expr> assertion) {
    logger.on(assertion);
  }

  template <typename Expr> void on(events::assertion_failed<Expr> assertion) {
    current_test_failed = true;
    logger.on(assertion);
  }

  template <typename Expr> void on(events::assertion_passed<Expr> assertion) {
    logger.on(assertion);
  }

  template <typename Expr>
  void on(events::assertion_exception<Expr> assertion) {
    logger.on(assertion);
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
  std::vector<std::function<void()>> test_suites;
  details::tag tags;
  bool current_test_failed{false};
};

template <typename Printer> class logger {
  logger() = default;
  logger(const Printer &p) : printer(p) {}
  logger(Printer &&p) : printer(std::move(p)) {}

  template <typename Expr> void on(events::assertion_added<Expr>) {
    total_assertions++;
  }

  template <typename Expr> void on(events::assertion_failed<Expr> assertion) {
    assertion_failed++;
    if (!already_failed) {
      test_failed++;
    }
    print_dash();
    printer << basename(test_stack.front().where.file_name()) << ':'
            << test_stack.front().where.line() << ':' << '\n'
            << printer.colors.warning
            << "TEST CASE:  " << printer.colors.normal;
    print_test_case_names();
    printer << basename(assertion.where.file_name()) << ':'
            << assertion.where.line() << ':';
    printer << printer.colors.error << "ERROR: " << printer.colors.normal;
    printer << "[ ";
    printer << assertion.expr;
    printer << " ] is not correct\n";
    print_dash();
  }

  template <typename Expr>
  void on(events::assertion_exception<Expr> assertion) {
    assertion_failed++;
    if (!already_failed) {
      test_failed++;
    }
    print_dash();
    printer << basename(test_stack.front().where.file_name()) << ':'
            << test_stack.front().where.line() << ':' << '\n'
            << printer.colors.warning
            << "TEST CASE:  " << printer.colors.normal;
    print_test_case_names();
    printer << basename(assertion.where.file_name()) << ':'
            << assertion.where.line() << ':';
    printer << printer.colors.error << "ERROR: " << printer.colors.normal;
    printer << "[ ";
    printer << assertion.expr;
    printer << " ] failed with exception\n"
            << "Exception : " << assertion.msg << '\n';
    print_dash();
  }

  template <typename Expr> void on(events::assertion_passed<Expr> assertion) {
    assertion_passed++;
  }

  void on(events::test_begin test) {
    test_stack.push_back(test);
    already_failed = false;
  }

  void on(events::test_end test) { test_stack.pop_back(); }

  void on(events::test_skipped test) { test_skipped++; }

  void on(events::test_run) {}

  void on(events::summary) {
    printer << printer.colors.heading << "[cpptest] " << printer.colors.normal
            << "Test Cases: " << test_failed + test_failed + test_skipped
            << "\t|\t" << printer.colors.passed << test_passed
            << " passed\t|\t ";
    if (test_failed) {
      printer << printer.colors.failed;
    }
    printer << test_failed << " failed\t|\t";
    if (test_failed) {
      printer << printer.colors.normal;
    }
    if (test_skipped) {
      printer << printer.colors.warning;
    }
    printer << test_skipped << " skipped\n";
    if (test_skipped) {
      printer << printer.colors.normal;
    }

    printer << printer.colors.heading << "[cpptest] " << printer.colors.normal
            << "Assertions: " << assertion_passed + assertion_failed << "\t|\t"
            << printer.colors.passed << assertion_passed
            << printer.colors.normal << "\t|\t";
    if (assertion_failed > 0) {
      printer << printer.colors.failed;
    }
    printer << assertion_failed << '\n';
    if (assertion_failed > 0) {
      printer << printer.colors.normal;
    }
    printer << "Status : ";
    if (test_failed > 0) {
      printer << printer.colors.failed;
      printer << "Failed" << '\n';
      printer << printer.colors.normal;
    } else {
      printer << "Passed" << '\n';
    }

    std::cout << printer;
    std::cout.flush();
  }

  template <typename Msg> void on(events::log<Msg> msg) {
    printer << msg << '\n';
  }

private:
  inline void print_dash() {
    printer << printer.colors.warn;
    printer << "\n========================================================"
               "=======================\n";
    printer << printer.colors.normal;
  }

  inline auto basename(const char *file_name) {
    namespace fs = std::filesystem;
    return fs::path(file_name).filename().generic_string();
  }

  inline void print_test_case_names() {
    for (auto name : test_stack) {
      printer << name << "\n\t";
    }
  }

private:
  Printer printer;
  bool already_failed = false;
  std::size_t test_level{};
  std::size_t assertion_failed{};
  std::size_t assertion_passed{};
  std::size_t total_assertions{};
  std::size_t test_failed{};
  std::size_t test_passed{};
  std::size_t test_skipped{};
  std::stringstream buffer;
  std::vector<events::test_begin> test_stack;
};

}; // namespace handlers

namespace details {

inline void on(...) {}

struct test {
  std::string_view name;
  tag test_tag;
  std::source_location where;

  test(const char *name,
       const std::source_location &where = std::source_location::current())
      : name(name), where(where) {}

  test(std::string_view name,
       const std::source_location &where = std::source_location::current())
      : name(name), where(where) {}

  test(const char *name, decltype(sizeof("")) size,
       const std::source_location &where = std::source_location::current())
      : name{name, size}, where(where) {}

  template <std::invocable Func> void operator=(Func &&func) {
    auto current_test_case = events::test(name, std::move(test_tag),
                                          std::forward<Func>(func), where);
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
