#ifndef NXC_FUNCS_INCLUDED
#define NXC_FUNCS_INCLUDED

#include "cpp_utils/common.hpp"
#include "cpp_utils/unicode.hpp"

#include <algorithm>
#include <array> 
#include <cassert>
#include <chrono>
#include <cmath>
#include <compare>
#include <concepts>
#include <iterator>
#include <limits>
#include <ranges>
#include <string>
#include <tuple>
#include <type_traits>

namespace nxc
{
  template <typename F, typename ... I>
  concept indirectly_invocable =
    std::copy_constructible<F> &&
    (std::indirectly_readable<I> && ...) &&
    std::invocable<F&, std::iter_value_t<I>& ...> &&
    std::invocable<F&, std::iter_reference_t<I> ...> &&
    std::invocable<F&, std::iter_common_reference_t<I> ...> &&
    std::common_reference_with<
    std::invoke_result_t<F&, std::iter_value_t<I>& ...>,
    std::invoke_result_t<F&, std::iter_reference_t<I> ...>>;

}

#if __has_include(<boost/hana/fwd/integral_constant.hpp>) \
  && __has_include(<boost/hana/fwd/value.hpp>)

#include <boost/hana/fwd/integral_constant.hpp>
#include <boost/hana/fwd/value.hpp>
#include <boost/hana/concept/integral_constant.hpp>

namespace nxc
{
  template <typename T, typename S>
    requires boost::hana::IntegralConstant<S>::value
  constexpr T safe_cast(const S _src)
  {
    return safe_cast<T>(boost::hana::value(_src));
  }
}

#endif

#if __has_include(<boost/regex/pending/unicode_iterator.hpp>)
#include <boost/regex/pending/unicode_iterator.hpp>

namespace nxc
{
  inline auto u32_view(const std::u8string_view str)
  {
    using u32_iterator = boost::u8_to_u32_iterator<decltype(cbegin(str)), char32_t>;
    return std::ranges::subrange(u32_iterator{ cbegin(str) }, u32_iterator{ cend(str) });
  }
}

#endif

#if __has_include(<boost/bimap.hpp>)
#include <boost/bimap.hpp>

namespace nxc
{
  template <typename Bimap>
  constexpr auto
    make_bimap(const std::initializer_list<typename Bimap::value_type>& list)
  {
    return Bimap(list.begin(), list.end());
  }

  template <typename L, typename R>
  constexpr auto
    make_bimap(const std::initializer_list<typename boost::bimap<L, R>::value_type>& list)
  {
    return make_bimap<boost::bimap<L, R>>(list);
  }
}

#endif

namespace nxc
{
  template<typename ExecutionPolicy, std::forward_iterator It,
    typename OutIt,
    std::indirect_unary_predicate<It> Predicate>

    requires std::output_iterator<OutIt, It>

    constexpr void find_all(ExecutionPolicy&& expo, It it, const It end, OutIt out,
      Predicate predicate)
  {
    using namespace std::ranges;

    for (it = std::find_if(expo, it, end, predicate);
      it != end;
      it = std::find_if(expo, it, end, predicate))
    {
      *out++ = it++;
    }
  }

  template <std::forward_iterator It>
  constexpr auto remove_iterator(const It pos, const It end)
  {
    assert(pos != end);
    const auto it = std::ranges::move(pos + 1, end, pos).out;
    assert(it == end - 1);
    return it;
  }

  template <std::forward_iterator It>
  constexpr auto remove_iterator(const std::reverse_iterator<It> pos, const It end)
  {
    assert(&*(pos.base() - 1) == &*pos);
    return remove_iterator(pos.base() - 1, end);
  }

  template <std::forward_iterator It, std::ranges::bidirectional_range Range>
  requires std::convertible_to<std::ranges::range_reference_t<Range>, It>
    constexpr auto remove_sorted_iterators(It end, Range&& sortedRemovedIterators)
  {
    using namespace std::ranges;
    // in the descending order ...
    for_each(std::forward<Range>(sortedRemovedIterators) | views::reverse,
      [&end](const It it)
      {
        // .. we can remove iterators without invalidating the others
        end = remove_iterator(it, end);
      });

    return end;
  }

  template <typename Execution, typename Range,
    typename Proj = std::identity, typename Relation = std::ranges::equal_to>

    requires (std::ranges::forward_range<Range>&&
      std::indirect_equivalence_relation<
      Relation,
      std::projected<std::ranges::iterator_t<Range>, Proj>,
      std::projected<std::ranges::iterator_t<Range>, Proj>>)

    constexpr bool is_unique(Execution&& ex, Range&& sortedRange, Relation rel = {}, Proj proj = {})
  {
    using namespace std::ranges;
    auto view = std::forward<Range>(sortedRange) | views::transform(proj);
    return end(view) == std::adjacent_find(std::forward<Execution>(ex),
      begin(view), end(view), std::ref(rel));
  }

  template <typename Enum>
  requires std::is_enum_v<Enum>
  constexpr auto to_underlying(const Enum e)
  {
    return std::underlying_type_t<Enum>(e);
  }

  template <typename Func, typename R, typename ... Rs>
  requires (std::ranges::input_range<R> && (std::ranges::input_range<Rs> && ...))
    && indirectly_invocable<Func, std::ranges::iterator_t<R>, std::ranges::iterator_t<Rs>...>
    constexpr void for_each(Func func, R&& range, Rs&& ... ranges)
  {
    using namespace std::ranges;

    if constexpr (forward_range<R> && (forward_range<Rs> && ...) &&
      sized_range<R> && (sized_range<Rs> && ...))
      assert(((size(range) == size(ranges)) && ...));

    auto begins = std::make_tuple(begin(range), begin(ranges)...);
    const auto ends = std::make_tuple(end(range), end(ranges)...);

    while (std::get<0>(begins) != std::get<0>(ends))
    {
      std::apply([&func](auto&... it)
        {
          std::invoke(func, *it ...);
          (++it, ...);
        },
        begins);
    }
  }

  template <std::ranges::input_range R>
  constexpr auto efficient_find(R&& range, const auto& key)
  {
    return std::ranges::find(std::forward<R>(range), key);
  }

  template <std::ranges::input_range R, typename Key>
  requires requires (R&& range, const Key& key) { std::forward<R>(range).find(key); }
  constexpr auto efficient_find(R&& range, const Key& key)
  {
    return std::forward<R>(range).find(key);
  }

  namespace detail
  {
    template <std::size_t N>
    using constant = std::integral_constant<std::size_t, N>;
  }

  template <std::size_t N>
  constexpr auto make_const() { return detail::constant<N>{}; }

  template <typename Container>
  constexpr auto remove_constness(Container& c,
    const std::ranges::iterator_t<const Container> it)
    requires (!std::is_const_v<Container>)
  {
    // https://stackoverflow.com/a/10669041/7949231
    return c.erase(it, it);
  }

  template <typename Container>
  constexpr auto remove_constness(Container& c,
    const std::ranges::iterator_t<Container> it)
    requires (!std::is_const_v<Container>)
  {
    return it;
  }

  template<size_t...Ix>
  constexpr auto
    make_array_from_sequence(std::index_sequence<Ix...>, auto generator)
  {
    return std::array{ generator(make_const<Ix>())... };
  }

  template <std::size_t N>
  constexpr auto make_array(auto gen)
  {
    return make_array_from_sequence(std::make_index_sequence<N>(), gen);
  }

  namespace detail
  {
    template <std::size_t Start, std::size_t... Seq>
    constexpr auto sub_tuple(const auto& tuple,
      detail::constant<Start>,
      std::index_sequence<Seq...>)
    {
      return std::make_tuple(std::get<Start + Seq>(tuple)...);
    }
  }

  template <std::size_t Start, std::size_t Size>
  constexpr auto sub_tuple(const auto& tuple)
  {
    return detail::sub_tuple(tuple, make_const<Start>(),
      std::make_index_sequence<Size>());
  }

  template <std::size_t Index, typename Tuple>
  constexpr auto remove(const Tuple& t)
  {
    const auto tupleSize = std::tuple_size_v<Tuple>;

    const auto beforeRemovedPart = sub_tuple<0U, Index>(t);
    const auto behindRemovedPart =
      sub_tuple<Index + 1U, tupleSize - (Index + 1U)>(t);

    return tuple_cat(beforeRemovedPart, behindRemovedPart);
  }

  namespace detail
  {
    template <typename SizesTuple>
    constexpr auto make_multi_array(auto generator,
      const SizesTuple sizes,
      const auto idx)
    {
      if constexpr (1U < std::tuple_size_v<SizesTuple>)
      {
        // not the last dimension
        constexpr auto currentDimensionSize = std::get<0U>(sizes);

        return make_array<currentDimensionSize>(
          [generator, &idx, &sizes](const auto index)
          {
            constexpr auto indices = tuple_cat(idx, make_tuple(index));
            return make_multi_array(generator, remove<0U>(sizes), indices);
          });
      }
      else
      {
        // last dimension
        static_assert(1U == std::tuple_size_v<SizesTuple>);
        const auto lastDimensionSize = std::get<0U>(sizes);

        return make_array<lastDimensionSize>(
          [generator, &idx](const auto index)
          {
            const auto indices = tuple_cat(idx, make_tuple(index));
            return apply(generator, indices);
          });
      }
    }
  }

  template <auto Size, auto ... Sizes>
  constexpr auto make_multi_array(auto generator)
  {
    constexpr auto sizesTuple = std::make_tuple(make_const<Sizes>() ...);
    return make_array<Size>(
      [generator, &sizesTuple](const auto index)
      {
        return detail::make_multi_array(generator, sizesTuple, std::make_tuple(index));
      });
  }

  template <typename Variable, typename Coef, typename ... Coefs>
  constexpr auto polynomial(Variable x, Coef coef, Coefs... coefs)
  {
    constexpr auto degree = sizeof...(coefs);
    if constexpr (degree > 0U)
      return coef * std::pow(x, degree) + polynomial(x, coefs...);
    else
      return coef;
  }

  enum class sign_t { negative, zero, positive };

  template <typename T>
  sign_t signum(const T value)
  {
    if (0 == std::abs(value))  return sign_t::zero;
    if (value > 0)             return sign_t::positive;
    assert(value < 0);         return sign_t::negative;
    static_assert(!(-0.0 < 0));
  }

  // f shall not invalidate the iterators
  template <std::ranges::forward_range Range1, std::ranges::forward_range Range2,
    std::regular_invocable<std::ranges::iterator_t<Range1>, std::ranges::iterator_t<Range2>> Func>
    void for_each_combination(Range1&& range1, Range2&& range2, Func f)
  {
    for (auto it1 = std::ranges::begin(range1);
      it1 != std::ranges::end(range1);
      ++it1)
    {
      for (auto it2 = std::ranges::begin(range2);
        it2 != std::ranges::end(range2);
        ++it2)
      {
        // f will not modify iterators
        std::invoke(f, std::as_const(it1), std::as_const(it2));
      }
    }
  }

  // https://en.cppreference.com/w/cpp/utility/variant/visit
  template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
  template<class... Ts> overloaded(Ts...)->overloaded<Ts...>;

  template <std::integral T, std::intmax_t Num, std::intmax_t Den>
  constexpr auto operator/(const T number, const std::ratio<Num, Den> ratio)
  {
    return number * Den / Num;
  }

  template <std::integral T, std::intmax_t Num, std::intmax_t Den>
  constexpr auto operator*(const T number, const std::ratio<Num, Den> ratio)
  {
    return number * Num / Den;
  }

  using uni = std::ratio<1, 1>;

  constexpr overloaded si_ratio_sign
  {
    [](uni) { return ' '; },
    [](std::kilo) { return 'k'; },
    [](std::mega) { return 'M'; }
  };
}

#if __has_include(<format>)
#include <format>

namespace nxc
{
  template <std::integral I, std::ranges::output_range<const char&> Range>
    requires std::ranges::forward_range<Range>
  void to_significant_chars(const I number, Range& buffer)
  {
    using namespace std::string_view_literals;
    using namespace std::ranges;

    const auto write =
      [&buffer, number](const auto ratio)
    {
      const auto result = std::format_to_n(begin(buffer), size(buffer), "{}{}\0"sv,
        number / ratio,
        si_ratio_sign(typename decltype(ratio)::type{})
      );

      return static_cast<unsigned long long>(result.size) <= size(buffer);
    };

    if (write(uni{}))
      return;
    if (write(std::kilo{}))
      return;
    if (write(std::mega{}))
      return;

    std::format_to_n(begin(buffer), size(buffer), ":-(\0"sv);
  }
}
#endif

namespace nxc
{
  template <typename T>
  concept integral_or_enum = std::integral<T> || std::is_enum_v<T>;

  template <integral_or_enum Target, typename Source>
  Target round_cast(const Source value)
  {
    return static_cast<Target>(std::round(value));
  }

  inline void ensure(const bool condition,
    const std::u8string_view message)
  {
    if (not condition)
      nxc::error(message);
  }

  template <std::integral Target, std::integral Source>
  Target safe_cast(const Source val)
  {
    using namespace std;
    ensure(cmp_greater_equal(val, numeric_limits<Target>::min()), u8"hodnota je větší než umožňuje cílový typ"s);
    ensure(cmp_less_equal(val, numeric_limits<Target>::max()), u8"hodnota je menší než umožňuje cílový typ"s);

    return static_cast<Target>(val);
  }

  template <typename Target, typename Source>
  Target safe_cast(const Source val)
  {
    using namespace std;
    const auto result = static_cast<Target>(val);

    ensure(result == val, u8"chyba konverze");

    return result;
  }

}

#endif
