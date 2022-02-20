#ifndef NXC_FORMATTER_INCLUDED
#define NXC_FORMATTER_INCLUDED

#include "cpp_utils/unicode.hpp"

#include <concepts>
#include <format>
#include <ranges>

// prints comma separated values
template<std::ranges::input_range R>
requires (not std::convertible_to<R, std::basic_string_view<std::ranges::range_value_t<R>>>)
struct std::formatter<R>
{
  constexpr auto parse(format_parse_context& ctx)
  {
    return std::ranges::end(ctx);
  }

  template <typename FormatContext>
  auto format(const R& v, FormatContext& ctx)
  {
    auto&& out = ctx.out();

    auto it = std::ranges::begin(v);
    const auto end = std::ranges::end(v);
    if (end != it)
    {
      vformat_to(out, "{}", std::make_format_args(*it));
      ++it;

      for (; it != end; ++it)
        vformat_to(out, ", {}", std::make_format_args(*it));
    }
    return out;
  }

  template <typename FormatContext>
  auto format(R v, FormatContext& ctx) requires std::ranges::view<R>
  {
    auto&& out = ctx.out();

    auto it = std::ranges::begin(v);
    const auto end = std::ranges::end(v);
    if (end != it)
    {
      vformat_to(out, "{}", std::make_format_args(*it));
      ++it;

      for (; it != end; ++it)
        vformat_to(out, ", {}", std::make_format_args(*it));
    }
    return out;
  }
};

// formatter for all char8_t string types
//
// example
// std::cout << nxc::fu8<char>(u8"{}", u8"hello");
//
// TODO remove this when proper char8_t support available
template <std::convertible_to<std::u8string_view> S>
struct std::formatter<S>
{
  constexpr auto parse(format_parse_context& ctx)
  {
    return std::ranges::end(ctx);
  }

  template <typename FormatContext>
  auto format(const std::u8string_view v, FormatContext& ctx)
  {
    auto&& out = ctx.out();

    vformat_to(out, "{}", std::make_format_args(
      nxc::from_u8string_view(v)));

    return out;
  }
};

namespace nxc
{
  // same as std::format, only allowing char8_t format string
  template <std::integral C = char8_t, typename ... Args>
  auto fu8(const std::u8string_view s, Args&&... args);
  
  template <std::integral C, typename ... Args>
  requires std::same_as<std::remove_cvref_t<C>, char>
  auto fu8(const std::u8string_view s, Args&&... args)
  {
    return std::format(nxc::from_u8string_view(s), std::forward<Args>(args)...);
  }
  
  template <std::integral C, typename ... Args>
  auto fu8(const std::u8string_view s, Args&&... args)
  {
    return from_string(fu8<char>(s, std::forward<Args>(args)...));
  }
  
  template <std::integral C = char8_t, std::output_iterator<const C&> It, typename ... Args>
  void fu8_to(It out, const std::u8string_view s, Args&&... args)
  {
    struct
    {
      It out;
      void operator()(const char c) { *out++ = reinterpret_cast<const C&>(c); }
    } helper{ out };
  
    std::format_to(invoking_iterator(helper), nxc::from_u8string_view(s), std::forward<Args>(args)...);
  }
}

#endif