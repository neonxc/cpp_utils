#ifndef NXC_VISIT_INCLUDED
#define NXC_VISIT_INCLUDED

// name-based visitation infrastructure
// example of use
//
// struct A
// {
//     int a;
//     char b;
// };
// 
// inline const auto& members(A&) // <-- mandatory name
// {
//   static const auto obj = nxc::members_t<A> // <-- mandatory type
//   {
//     nxc::named_member{&A::a, u8"a"},
//     nxc::named_member{&A::b, u8"b"},
//   };
// 
//    return obj;
// }
// 
// #include <iostream>
// 
// int main()
// {
//   A a{1, 'c'};
//   const auto visitSuccesful =
//     nxc::visit
//     (
//       [](auto&& member){ std::cout << member;},
//       a,
//       u8"b"
//     );
// 
//   // the program prints 'c' to cout
// }

#include "cpp_utils/funcs.hpp"
#include "cpp_utils/unicode.hpp"

#if __has_include(<boost/hana.hpp>) && __has_include(<boost/pfr.hpp>)

#include <boost/hana/range.hpp>
#include <boost/hana/set.hpp>
#include <boost/hana/type.hpp>
#include <boost/hana/tuple.hpp>
#include <boost/hana/product.hpp>
#include <boost/hana/fwd/core/to.hpp>
#include <boost/hana/fwd/core/make.hpp>
#include <boost/pfr.hpp>

#include <any>
#include <array>
#include <cassert>
#include <ranges>
#include <string_view>
#include <type_traits>
#include <unordered_set>
#include <variant>

namespace nxc
{

template <typename T, typename Member>
requires (std::is_class_v<T> && !std::is_reference_v<Member>)
struct mem_ptr
{
  using type = Member T::*;
};

template <typename T, typename Member>
using mem_ptr_t = typename mem_ptr<T, Member>::type;

template <typename Base, typename Derived>
concept base = std::is_base_of_v<Base, Derived>;

template <typename T>
concept aggregate = std::is_aggregate_v<T>;

namespace detail
{
  template<class T>
  struct any_base {
    operator T() = delete;
    template<base<T> U> operator U();
  };

  template<aggregate, class = void> struct has_any_base : std::false_type {};
  template<aggregate T>
  struct has_any_base <T, std::void_t<decltype(T{ any_base<T>{} }) >> : std::true_type {};
}

// courtesy https://stackoverflow.com/a/46196936/7949231
template <typename T>
requires std::is_class_v<T>
struct has_any_base : detail::has_any_base<std::remove_cvref_t<T>>
{};

namespace detail
{
  template <typename T>
  concept simple_aggregate = aggregate<T> && !has_any_base<T>::value;
}

// no reference members, no bases
template <typename T>
concept simple_aggregate = detail::simple_aggregate<std::remove_cvref_t<T>>;

template <typename Ptr>
requires std::is_member_pointer_v<Ptr>
struct type_from_mem_ptr;

template <typename C, typename M>
struct type_from_mem_ptr<M C::*>
{
  using type = M;
};

template <typename Ptr>
using type_from_mem_ptr_t = typename type_from_mem_ptr<Ptr>::type;

template <typename Ptr>
requires std::is_member_pointer_v<Ptr>
struct class_from_mem_ptr;

template <typename C, typename M>
struct class_from_mem_ptr<M C::*>
{
  using type = C;
};

template <typename Ptr>
using class_from_mem_ptr_t = typename class_from_mem_ptr<Ptr>::type;

namespace detail
{
  template <typename T>
  struct vector_rank;

  template <typename T>
  struct vector_rank<std::vector<T>>;
}

template <typename T>
static constexpr std::size_t vector_rank_v =
detail::vector_rank<std::remove_cvref_t<T>>::value;

template <typename T>
struct detail::vector_rank
{
  static constexpr std::size_t value = 0;
};

template <typename T>
struct detail::vector_rank<std::vector<T>>
{
  static constexpr std::size_t value = 1 +
    vector_rank_v<T>;
};

  template <typename Ptr>
  struct named_member
  {
    using names_t = std::array<std::u8string_view,
      vector_rank_v<type_from_mem_ptr_t<Ptr>> +1>;

    const Ptr pointer;
    const names_t names;

    template <typename ... Names>
    requires ((std::convertible_to<Names, std::u8string_view> && ...)
      // allow only exact number of (nested) names
      && (sizeof...(Names) == std::tuple_size_v<names_t>))
      constexpr named_member(const Ptr ptr, Names&& ... names)
      : pointer{ ptr }, names{ std::forward<Names>(names)... }
    { }
  };

  template <typename Ptr, typename ... Names>
  named_member(Ptr, Names ...)->named_member<Ptr>;

  namespace detail
  {
    template <typename T>
    consteval auto member_ptrs_variant()
    {
      using namespace boost::hana;

      const auto range = range_c<std::size_t, 0, boost::pfr::tuple_size<T>::value>;
      const auto make_set = [](const auto ... stuff)
      {
        return to<set_tag>(make_tuple(type_c<mem_ptr_t<T, boost::pfr::tuple_element_t<value(stuff), T>>> ...));
      };

      const auto set = unpack(range, make_set);

      const auto make_variant = [](const auto ... stuff)
      {
        return type_c<std::variant<typename decltype(+stuff)::type...>>;
      };

      return unpack(set, make_variant);
    };

    template <typename T>
    consteval auto named_members_variant()
    {
      using namespace boost::hana;

      const auto allIndices = range_c<std::size_t, 0, boost::pfr::tuple_size<T>::value>;
      const auto make_set = [](const auto ... indices)
      {
        return to<set_tag>
          (
            make_tuple
            (
              type_c<named_member<mem_ptr_t<T, boost::pfr::tuple_element_t<value(indices), T>>>> ...
            )
          );
      };

      const auto set = unpack(allIndices, make_set);

      const auto make_variant = [](const auto ... stuff)
      {
        return type_c<std::variant<typename decltype(+stuff)::type...>>;
      };

      return unpack(set, make_variant);
    };
  }

  template <simple_aggregate T>
  struct member_ptrs_variant
  {
  private:
    using raw_type = std::remove_cvref_t<T>;
  public:
    using type = typename decltype(
      +detail::member_ptrs_variant<raw_type>())
      ::type;
  };

  template <simple_aggregate T>
  using member_ptrs_variant_t = typename member_ptrs_variant<T>::type;

  template <simple_aggregate T>
  struct named_members_variant
  {
  private:
    using raw_type = std::remove_cvref_t<T>;
  public:
    using type = typename decltype(
      +detail::named_members_variant<raw_type>())
      ::type;
  };

  namespace detail
  {
    template <typename Var, typename Cl>
    constexpr bool is_named_member_variant_for()
    {
      using namespace boost::hana;

      const auto allIndices = range_c<std::size_t, 0, std::variant_size_v<Var>>;
      const auto areVarAlternativesPointersToMembersOfCl = [](const auto ... indices)
      {
        constexpr bool allPointersAreToMembersOfCl = product<>
          (
            make_tuple
            (
              integral_c
              <
                int,
                std::same_as
                <
                  Cl,
                  std::remove_const_t
                  <
                    class_from_mem_ptr_t
                    <
                      std::remove_const_t
                      <
                        decltype(std::variant_alternative_t<value(indices), Var>::pointer)
                      >
                    >
                  >
                >
              > ...
            )
          );

        return allPointersAreToMembersOfCl;
      };
      return unpack(allIndices, areVarAlternativesPointersToMembersOfCl);
    }

    template <typename T>
    struct is_variant : std::false_type
    { };

    template <typename ... Args>
    struct is_variant<std::variant<Args...>> : std::true_type
    {};
  }

  template <typename T>
  concept is_variant = requires (T& t) { std::get<0>(t); };

  template <typename Var, typename Cl>
  concept named_member_variant_for = is_variant<Var> && detail::is_named_member_variant_for<Var, std::remove_const_t<Cl>>();

  template <typename Var>
  concept named_member_var = named_member_variant_for
    <
      Var,
      class_from_mem_ptr_t
      <
        std::remove_const_t
        <
          decltype(std::variant_alternative_t<0, Var>::pointer)
        >
      >
    >;

  template <simple_aggregate T>
  using named_members_variant_t = typename named_members_variant<T>::type;

  namespace detail
  {
    constexpr void names(auto callback, const named_member_var auto& variant)
    {
      std::u8string_view name;
      std::visit
      (
        [&name, &callback](const auto& alternative)
        {
          const auto& names = alternative.names;
          std::invoke(callback, names);
        },
        variant
      );
    }
  }

  template <std::size_t I = 0>
  constexpr auto name(const named_member_var auto& v)
  {
    std::u8string_view name;

    detail::names(
      [&name]<std::size_t N>
      (const std::array<std::u8string_view, N>&names)
    {
      static_assert(I < N);
      name = names[I];
    }, v);

    return name;
  }

  constexpr auto names(const named_member_var auto& v)
  {
    std::ranges::subrange<const std::u8string_view*> view;
    static_assert(std::ranges::sized_range<decltype(view)>);

    detail::names(
      [&view]<std::size_t N>
      (const std::array<std::u8string_view, N>& names)
    {
      view = std::ranges::subrange(names.data(), names.data() + N);
    }, v);

    return view;
  }

  template <typename T>
  constexpr void visit(auto func, T&& object,
    const named_member_variant_for<std::remove_reference_t<T>> auto& v)
  {
    std::visit([&object, &func](const auto& alternative)
      {
        std::invoke(func,
          std::invoke(alternative.pointer, std::forward<T>(object)));
      },
      v);
  }

  namespace detail
  {
    template <typename T>
    struct hash
    {
      using is_transparent = void;

      auto operator()(const std::u8string_view s) const
      {
        return std::hash<std::u8string_view>{}(s);
      }
      auto operator()(const named_members_variant_t<T>& v) const
      {
        return operator()(name(v));
      }
    };

    template <typename T>
    struct equal_to
    {
      using is_transparent = void;

      constexpr auto operator()(const std::u8string_view s, const named_members_variant_t<T>& v) const
      {
        return s == name(v);
      }
      constexpr auto operator()(const named_members_variant_t<T>& v, const std::u8string_view s) const
      {
        return operator()(s, v);
      }

      auto operator()(const named_members_variant_t<T>& v1, const named_members_variant_t<T>& v2) const
      {
        return name(v1) == name(v2);
      }
    };
  }

  template <simple_aggregate T>
  using members_t = std::unordered_set<named_members_variant_t<T>, detail::hash<T>, detail::equal_to<T>>;

  template <typename T, typename Name>
  concept named_member_visitable = simple_aggregate<T>
    && requires (T & a, Name& n)
  {
    { *nxc::efficient_find(members(a), n) }
      -> std::convertible_to<named_members_variant_t<T>>;
  };

  template <typename Name, typename T>
  requires 
  (
    named_member_visitable<T, Name>
    && not is_variant<Name>
  )
  constexpr bool visit(auto f, T&& obj, const Name& str)
  {
    using raw_type = std::remove_cvref_t<T>;

    const auto& mems = members(obj);
    const auto namedMemberVarIt = nxc::efficient_find(mems, str);

    if (namedMemberVarIt == end(mems))
      return false;

    bool visited = false;

    boost::pfr::for_each_field(obj,
      [&]<typename M>(M&&)
    {
      if (!visited)
      {
        using std::invoke;
        using ptr_t = mem_ptr_t<raw_type, std::remove_cvref_t<M>>;

        auto* namedMemberPtr = std::get_if<nxc::named_member<ptr_t>>(&*namedMemberVarIt);
        if (namedMemberPtr)
        {
          invoke(f, invoke(namedMemberPtr->pointer, obj));
          visited = true;
        }
      }
    });

    assert(visited);

    return true;
  }

}

#endif // __has_include

#endif