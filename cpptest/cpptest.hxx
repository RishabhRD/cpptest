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
  comparator(const LHS &lhs, const RHS &rhs, Compare &compare)
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
struct equals_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare>
struct not_equals_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare>
struct greater_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare>
struct greater_equals_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare>
struct lesser_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare>
struct lesser_equals_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare>
struct and_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare>
struct or_ : public comparator<LHS, RHS, Compare> {};

template <typename LHS, typename RHS, typename Compare = comparators::equal_to>
auto equals(const LHS &lhs, const RHS &rhs,
            const Compare &compare = comparators::equal_to{}) {
  return equals_(lhs, rhs, compare);
}

template <typename LHS, typename RHS,
          typename Compare = comparators::not_equal_to>
auto not_equals(const LHS &lhs, const RHS &rhs,
                const Compare &compare = comparators::not_equal_to{}) {
  return not_equals_(lhs, rhs, compare);
}

template <typename LHS, typename RHS,
          typename Compare = comparators::greater_than>
auto greater(const LHS &lhs, const RHS &rhs,
             const Compare &compare = comparators::greater_than{}) {
  return greater_(lhs, rhs, compare);
}

template <typename LHS, typename RHS,
          typename Compare = comparators::greater_equal_to>
auto greater_eq(const LHS &lhs, const RHS &rhs,
                const Compare &compare = comparators::greater_equal_to{}) {
  return greater_eq_(lhs, rhs, compare);
}

template <typename LHS, typename RHS,
          typename Compare = comparators::lesser_than>
auto lesser(const LHS &lhs, const RHS &rhs,
            const Compare &compare = comparators::lesser_than{}) {
  return lesser_(lhs, rhs, compare);
}

template <typename LHS, typename RHS,
          typename Compare = comparators::lesser_equal_to>
auto lesser_eq(const LHS &lhs, const RHS &rhs,
               const Compare &compare = comparators::lesser_equal_to{}) {
  return lesser_eq_(lhs, rhs, compare);
}

template <std::derived_from<expression> LHS, std::derived_from<expression> RHS>
auto operator&&(const LHS &lhs, const RHS &rhs) {
  return and_(lhs, rhs, comparators::and_{});
}

template <std::derived_from<expression> LHS, std::derived_from<expression> RHS>
auto operator||(const LHS &lhs, const RHS &rhs) {
  return or_(lhs, rhs, comparators::or_{});
}

template <typename Expr, typename Compare>
requires(std::is_copy_constructible_v<Expr>
             &&concepts::UniOperable<Expr, Compare>) struct not_
    : public expression {

  not_(const Expr &ex, Compare &compare) : _expr(ex), _value(compare(ex)) {}

  constexpr operator bool() const { return _value; }

  constexpr auto value() const { return _expr; }

  Expr _expr;
  bool _value;
};

template <std::derived_from<expression> Expr> auto operator!(const Expr &expr) {
  return not_(expr, comparators::not_{});
}

template <typename Expr> auto not_of(const Expr &expr) {
  return not_(expr, comparators::not_{});
}
} // namespace expressions

namespace handlers {

template <class T> concept Printable = requires(std::stringstream &os, T a) {
  { os << a }
  ->std::same_as<std::stringstream &>;
};

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
public:
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

class printer {
  auto &operator<<(std::string_view str) {
    out << str;
    return *this;
  }

  auto &operator<<(char c) {
    out << c;
    return *this;
  }

  template <std::ranges::range Container> auto &operator<<(Container &&c) {
    *this << '{';
    bool first = true;
    for (const auto &ele : c) {
      out << (first ? "" : ", ") << ele;
    }
    *this << '}';
    return *this;
  }

  template <Printable Element> auto &operator<<(Element &&ele) { out << ele; }

  template <typename Element> auto &operator<<(Element &&ele) {
    out << "[UNKNOWN]";
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::equals_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " == " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::not_equals_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " != " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::greater_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " > " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::greater_equals_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " >= " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::lesser_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " < " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::lesser_equals_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " <= " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::and_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " && " << ele.rhs() << ')';
  }

  template <typename LHS, typename RHS, typename Compare>
  auto &operator<<(expressions::or_<LHS, RHS, Compare> ele) {
    *this << '(' << ele.lhs() << " || " << ele.rhs() << ')';
  }

  template <typename Expr, typename Compare>
  auto &operator<<(expressions::not_<Expr, Compare> ele) {
    *this << "(!" << ele.value() << ')';
  }

private:
  std::stringstream out{};
};

}; // namespace handlers

namespace details {
inline void on(...) {}
inline bool current_test_passed();
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
