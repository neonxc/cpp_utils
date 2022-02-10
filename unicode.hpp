#ifndef NXC_UNICODE_INCLUDED
#define NXC_UNICODE_INCLUDED

#include <concepts>
#include <optional>
#include <string>

namespace nxc
{
  
  inline auto from_string(const std::string_view s)
  {
    return std::u8string(begin(s), end(s));
  }
  
  inline auto from_string_view(const std::string_view s)
  {
    const auto ptr = reinterpret_cast<const char8_t*>(s.data());
    return std::u8string_view(ptr, ptr + size(s));
  }
  
  inline auto from_u8string_view(const std::u8string_view s)
  {
    const auto ptr = reinterpret_cast<const char*>(s.data());
    return std::string_view(ptr, ptr + size(s));
  }
  
  template <typename Traits, typename Allocator>
  class unicode_exception
  {
    const std::basic_string<char8_t, Traits, Allocator> message;
  public:
    unicode_exception(std::basic_string<char8_t, Traits, Allocator> s)
      : message{move(s)}
    { }
    unicode_exception(const unicode_exception&) = default;
    unicode_exception(unicode_exception&&) = default;
  
    const auto& what() const { return message; }
  
    virtual ~unicode_exception() = default;
  };
}

#endif
