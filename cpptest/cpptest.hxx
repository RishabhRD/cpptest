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

namespace concepts {
template <typename Expected, typename Actual, typename Compare>
concept Comparable = requires(Expected &&ex, Actual &&ac, Compare &&comp) {
  { comp(ex, ac) }
  ->std::same_as<bool>;
};

template <typename Expr, typename Compare>
concept UniOperable = requires(Expr &&ex, Compare &&comp) {
  { comp(ex) }
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

template <typename T, typename U>
concept GreaterComparable = requires(T t, U u) {
  { t > u }
  ->std::same_as<bool>;
};

template <typename T, typename U>
concept GreaterEqualComparable = requires(T t, U u) {
  { t >= u }
  ->std::same_as<bool>;
};

template <typename T, typename U>
concept LesserComparable = requires(T t, U u) {
  { t < u }
  ->std::same_as<bool>;
};

template <typename T, typename U>
concept LesserEqualComparable = requires(T t, U u) {
  { t <= u }
  ->std::same_as<bool>;
};

template <typename T, typename U> concept AndComparable = requires(T t, U u) {
  { bool(t) && bool(u) }
  ->std::same_as<bool>;
};

template <typename T, typename U> concept OrComparable = requires(T t, U u) {
  { bool(t) || bool(u) }
  ->std::same_as<bool>;
};

template <typename T> concept NotComparable = requires(T t) {
  { !bool(t) }
  ->std::same_as<bool>;
};


} // namespace concepts

namespace details {
struct tag {
  std::vector<std::string_view> tags;

  bool contains(std::string_view tag_name) {
    return std::find(tags.begin(), tags.end(), tag_name) != tags.end();
  }

  bool is_empty() { return tags.empty(); }

  void add(std::string_view tag_name) { tags.push_back(tag_name); }

  auto begin() { return tags.begin(); }

  auto end() { return tags.end(); }

  auto begin() const { return tags.begin(); }

  auto end() const { return tags.end(); }

  auto satisfies(const tag &t) {
    if (t.tags.size() == 0) {
      return true;
    }
    for (auto str : t) {
      if (contains(str)) {
        return true;
      }
    }
    return false;
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

template <typename Msg> struct log { Msg msg; };

struct exception {
  const char *msg;
  exception(const char *msg) : msg(msg) {}
  exception(const std::exception &ex) : msg(ex.what()) {}
  const char *what() { return msg; }
};

} // namespace events

namespace comparators {
struct equal_to {
  template <typename T, typename U>
  requires concepts::EqualComparable<T, U> bool operator()(const T &t,
                                                           const U &u) {
    return t == u;
  }
};

struct not_equal_to {
  template <typename T, typename U>
  requires concepts::InequalComparable<T, U> bool operator()(const T &t,
                                                             const U &u) {
    return t != u;
  }
};

struct greater_than {
  template <typename T, typename U>
  requires concepts::GreaterComparable<T, U> bool operator()(const T &t,
                                                             const U &u) {
    return t > u;
  }
};

struct greater_equal_to {
  template <typename T, typename U>
  requires concepts::GreaterEqualComparable<T, U> bool operator()(const T &t,
                                                                  const U &u) {
    return t >= u;
  }
};

struct lesser_than {
  template <typename T, typename U>
  requires concepts::LesserComparable<T, U> bool operator()(const T &t,
                                                            const U &u) {
    return t < u;
  }
};

struct lesser_equal_to {
  template <typename T, typename U>
  requires concepts::LesserEqualComparable<T, U> bool operator()(const T &t,
                                                                 const U &u) {
    return t <= u;
  }
};

struct and_ {
  template <typename T, typename U>
  requires concepts::AndComparable<T, U> bool operator()(const T &t,
                                                         const U &u) {
    return bool(t) && bool(u);
  }
};

struct or_ {
  template <typename T, typename U>
  requires concepts::OrComparable<T, U> bool operator()(const T &t,
                                                        const U &u) {
    return bool(t) || bool(u);
  }
};

struct not_ {
  template <typename T>
  requires concepts::NotComparable<T> bool operator()(const T &t) {
    return !(bool(t));
  }
};
} // namespace comparators

namespace expressions {
struct expression {};

template <typename LHS, typename RHS, typename Compare>
requires(
    concepts::Comparable<LHS, RHS, Compare> &&std::is_copy_constructible_v<LHS>
        &&std::is_copy_constructible_v<RHS>) struct comparator
    : public expression {
  comparator(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : _lhs(lhs), _rhs(rhs), _value(compare(lhs, rhs)) {}

  constexpr operator bool() const { return _value; }

  constexpr auto lhs() const { return _lhs; }

  constexpr auto rhs() const { return _rhs; }

private:
  const bool _value;
  const LHS _lhs;
  const RHS _rhs;
};

template <typename LHS, typename RHS, typename Compare>
struct equals_ : public comparator<LHS, RHS, Compare> {
  equals_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare>
struct not_equals_ : public comparator<LHS, RHS, Compare> {
  not_equals_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare>
struct greater_ : public comparator<LHS, RHS, Compare> {
  greater_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare>
struct greater_equals_ : public comparator<LHS, RHS, Compare> {
  greater_equals_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare>
struct lesser_ : public comparator<LHS, RHS, Compare> {
  lesser_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare>
struct lesser_equals_ : public comparator<LHS, RHS, Compare> {
  lesser_equals_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare>
struct and_ : public comparator<LHS, RHS, Compare> {
  and_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare>
struct or_ : public comparator<LHS, RHS, Compare> {
  or_(const LHS &lhs, const RHS &rhs, Compare &&compare)
      : comparator<LHS, RHS, Compare>(lhs, rhs,
                                      std::forward<Compare>(compare)) {}
};

template <typename LHS, typename RHS, typename Compare = comparators::equal_to>
auto equals(const LHS &lhs, const RHS &rhs,
            Compare &&compare = comparators::equal_to{}) {
  return equals_<LHS, RHS, Compare>(lhs, rhs, std::forward<Compare>(compare));
}

template <typename LHS, typename RHS,
          typename Compare = comparators::not_equal_to>
auto not_equals(const LHS &lhs, const RHS &rhs,
                Compare &&compare = comparators::not_equal_to{}) {
  return not_equals_<LHS, RHS, Compare>(lhs, rhs,
                                        std::forward<Compare>(compare));
}

template <typename LHS, typename RHS,
          typename Compare = comparators::greater_than>
auto greater(const LHS &lhs, const RHS &rhs,
             Compare &&compare = comparators::greater_than{}) {
  return greater_<LHS, RHS, Compare>(lhs, rhs, std::forward<Compare>(compare));
}

template <typename LHS, typename RHS,
          typename Compare = comparators::greater_equal_to>
auto greater_eq(const LHS &lhs, const RHS &rhs,
                Compare &&compare = comparators::greater_equal_to{}) {
  return greater_eq_<LHS, RHS, Compare>(lhs, rhs,
                                        std::forward<Compare>(compare));
}

template <typename LHS, typename RHS,
          typename Compare = comparators::lesser_than>
auto lesser(const LHS &lhs, const RHS &rhs,
            Compare &&compare = comparators::lesser_than{}) {
  return lesser_<LHS, RHS, Compare>(lhs, rhs, std::forward<Compare>(compare));
}

template <typename LHS, typename RHS,
          typename Compare = comparators::lesser_equal_to>
auto lesser_eq(const LHS &lhs, const RHS &rhs,
               Compare &&compare = comparators::lesser_equal_to{}) {
  return lesser_eq_<LHS, RHS, Compare>(lhs, rhs,
                                       std::forward<Compare>(compare));
}

template <std::derived_from<expression> LHS, std::derived_from<expression> RHS>
auto operator&&(const LHS &lhs, const RHS &rhs) {
  return and_<LHS, RHS, comparators::and_>(lhs, rhs, comparators::and_{});
}

template <std::derived_from<expression> LHS, std::derived_from<expression> RHS>
auto operator||(const LHS &lhs, const RHS &rhs) {
  return or_<LHS, RHS, comparators::or_>(lhs, rhs, comparators::or_{});
}

template <typename Expr, typename Compare>
requires(std::is_copy_constructible_v<Expr>
             &&concepts::UniOperable<Expr, Compare>) struct not_
    : public expression {

  not_(const Expr &ex, Compare &&compare) : _expr(ex), _value(compare(ex)) {}

  constexpr operator bool() const { return _value; }

  constexpr auto value() const { return _expr; }

  Expr _expr;
  bool _value;
};

template <std::derived_from<expression> Expr> auto operator!(const Expr &expr) {
  return not_<Expr, comparators::not_>(expr, comparators::not_{});
}

template <typename Expr> auto not_of(const Expr &expr) {
  return not_<Expr, comparators::not_>(expr, comparators::not_{});
}
} // namespace expressions

namespace handlers {

template <class T> concept Printable = requires(std::stringstream &os, T a) {
  {os << a};
};

class printer {
public:
  auto &operator<<(std::string_view str) {
    out << str;
    return *this;
  }

  auto &operator<<(char c) {
    out << c;
    return *this;
  }

  template <Printable Element> auto &operator<<(Element &&ele) {
    out << ele;
    return *this;
  }

  template <typename Element> auto &operator<<(Element &&ele) {
    out << "[UNKNOWN]";
    return *this;
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::equals_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " == " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::not_equals_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " != " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::greater_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " > " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::greater_equals_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " >= " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::lesser_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " < " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::lesser_equals_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " <= " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::and_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " && " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::or_<LHS, RHS, Compare> ele) {
    return *this << '(' << ele.lhs() << " || " << ele.rhs() << ')';
  }

  template <typename Expr, typename Compare>
  auto &operator<<(expressions::not_<Expr, Compare> ele) {
    return *this << "(!" << ele.value() << ')';
  }

  auto &operator<<(bool b) {
    if (b)
      return *this << "true";
    else
      return *this << "false";
  }

  auto &operator<<(const std::string &str) {
    out << str;
    return *this;
  }

  auto str() const { return out.str(); }

public:
  struct color_set {
    std::string_view heading = "\x1b[34m";
    std::string_view warning = "\x1b[33m";
    std::string_view failed = "\033[31m";
    std::string_view passed = "\033[32m";
    std::string_view normal = "\033[0m";
  };

  color_set colors;

  friend std::ostream &operator<<(std::ostream &os, const printer &pr) {
    return os << pr.out.str();
  }

private:
  std::stringstream out{};
};

template <typename Printer = printer> class logger {
public:
  template <typename Expr> void on(events::assertion_added<Expr>) {
    total_assertions++;
  }

  template <typename Expr> void on(events::assertion_failed<Expr> assertion) {
    assertion_failed++;
    if (!already_failed) {
      already_failed = true;
      test_failed++;
    }
    if (!dash_printed) {
      print_dash();
    }
    printer << basename(test_stack.front().where.file_name()) << ':'
            << test_stack.front().where.line() << ':' << '\n'
            << printer.colors.warning
            << "TEST CASE:  " << printer.colors.normal;
    print_test_case_names();
    printer << basename(assertion.where.file_name()) << ':'
            << assertion.where.line() << ':';
    printer << printer.colors.failed << " ERROR: " << printer.colors.normal;
    printer << "[ ";
    printer << assertion.expr;
    printer << " ] is not correct\n";
    print_dash();
  }

  template <typename Expr> void on(events::assertion_passed<Expr> assertion) {
    assertion_passed++;
  }

  void on(events::test_begin test) {
    test_stack.push_back(test);
    already_failed = false;
  }

  void on(events::test_end test) {
    if (!already_failed) {
      test_passed++;
    }
    test_stack.pop_back();
  }

  void on(events::test_skipped test) { test_skipped++; }

  void on(events::test_run) {}

  void on(events::summary) {
    printer << printer.colors.heading << "[cpptest] " << printer.colors.normal
            << "Test Cases: " << test_failed + test_passed + test_skipped
            << " | " << printer.colors.passed << test_passed << " passed"
            << printer.colors.normal << " | ";
    if (test_failed) {
      printer << printer.colors.failed;
    }
    printer << test_failed << " failed | ";
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
            << "Assertions: " << assertion_passed + assertion_failed << " | "
            << printer.colors.passed << assertion_passed << " passed"
            << printer.colors.normal << " | ";
    if (assertion_failed > 0) {
      printer << printer.colors.failed;
    }
    printer << assertion_failed << " failed\n";
    if (assertion_failed > 0) {
      printer << printer.colors.normal;
    }
    printer << "Status : ";
    if (test_failed > 0) {
      printer << printer.colors.failed;
      printer << "Failed" << '\n';
      printer << printer.colors.normal;
    } else {
      printer << printer.colors.passed;
      printer << "Passed" << '\n';
      printer << printer.colors.failed;
    }

    std::cout << printer;
    std::cout.flush();
  }

  template <typename Msg> void on(events::log<Msg> msg) { printer << msg.msg; }

  void on(events::exception ex) {}

private:
  inline void print_dash() {
    dash_printed = true;
    printer << printer.colors.warning;
    printer << "\n========================================================"
               "=======================\n\n";
    printer << printer.colors.normal;
  }

  inline auto basename(const char *file_name) {
    namespace fs = std::filesystem;
    return fs::path(file_name).filename().generic_string();
  }

  inline void print_test_case_names() {
    for (auto name : test_stack) {
      printer << name.name << "\n ";
    }
  }

private:
  Printer printer;
  bool already_failed = false;
  bool dash_printed = false;
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

template <typename Logger = logger<printer>> class test_event_handler {
public:
  ~test_event_handler() { logger.on(events::summary{}); }

  void on(events::test_suite suite) {
    test_suites.push_back(std::move(suite.tests));
  }

  void on(events::test &test) {
    if (test.test_tag.contains("disable")) {
      on(events::test_skipped{test.name, test.where});
    } else {
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

  void run(const details::tag &tags) {
    this->tags = tags;
    for (auto &suite : test_suites) {
      suite();
    }
  }

  void run(details::tag &&tags) {
    this->tags = std::move(tags);
    for (auto &suite : test_suites) {
      suite();
    }
  }

  void run() {
    for (auto &suite : test_suites) {
      suite();
    }
  }

  inline constexpr bool current_test_passed() const {
    return !current_test_failed;
  }

private:
  Logger logger;
  std::vector<std::function<void()>> test_suites;
  details::tag tags;
  bool current_test_failed{false};
};

}; // namespace handlers

struct default_config {};

template <typename = default_config>
auto runner = handlers::test_event_handler<>{};

inline void run() { runner<default_config>.run(); }

inline void run(int argc, char **argv) {
  // TODO: generate tags from arguments
  details::tag tags;
  runner<default_config>.run(std::move(tags));
}

inline void run(const std::vector<std::string_view> &tags) {
  details::tag t{tags};
  runner<default_config>.run(std::move(t));
}

inline void run(std::vector<std::string_view> &&tags) {
  details::tag t{std::move(tags)};
  runner<default_config>.run(std::move(t));
}

namespace details {
template <typename Event> inline void on(Event &&event) {
  runner<default_config>.on(event);
}
inline bool current_test_passed() {
  return runner<default_config>.current_test_passed();
}
} // namespace details

namespace assertions {
template <typename Expr>
requires std::is_convertible_v<Expr, bool> void
require(Expr &&expr,
        const std::source_location &where = std::source_location::current()) {
  if (!details::current_test_passed()) {
    return;
  }
  if (expr) {
    details::on(events::assertion_passed{expr, where});
  } else {
    details::on(events::assertion_failed{expr, where});
  }
}

template <typename Expr>
requires std::is_convertible_v<Expr, bool> void
check(Expr &&expr,
      const std::source_location &where = std::source_location::current()) {
  if (expr) {
    details::on(events::assertion_passed{expr, where});
  } else {
    details::on(events::assertion_failed{expr, where});
  }
}
} // namespace assertions

namespace details {

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

struct test_suite {
  template <std::invocable Func> test_suite(Func &&func) {
    on(events::test_suite{std::forward<Func>(func)});
  }
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
using details::test_suite;
using operators::operator""_test;
using operators::operator+;
using expressions::operator!;
using expressions::operator&&;
using expressions::operator||;
using assertions::check;
using assertions::require;
using expressions::equals;
using expressions::greater;
using expressions::greater_eq;
using expressions::lesser;
using expressions::lesser_eq;
using expressions::not_equals;

} // namespace cpptest
