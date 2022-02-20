#ifndef NXC_VIEWS_INCLUDED
#define NXC_VIEWS_INCLUDED

#include <boost/stl_interfaces/iterator_interface.hpp>

#include <memory>
#include <ranges>
#include <utility>

namespace nxc
{

  template <std::ranges::random_access_range V>
    requires std::ranges::view<V>
  class adjacent_circular_view : public std::ranges::view_interface<adjacent_circular_view<V>>
  {
    struct iterator
      : boost::stl_interfaces::
      proxy_iterator_interface<iterator,
      std::random_access_iterator_tag,
      std::pair<std::ranges::range_value_t<V>, std::ranges::range_value_t<V>>,
      std::pair<std::ranges::range_reference_t<V>, std::ranges::range_reference_t<V>>>
    {
    private:
      using base_t = boost::stl_interfaces::
        proxy_iterator_interface<iterator,
        std::random_access_iterator_tag,
        std::pair<std::ranges::range_value_t<V>, std::ranges::range_value_t<V>>,
        std::pair<std::ranges::range_reference_t<V>, std::ranges::range_reference_t<V>>>;

      adjacent_circular_view* parent;
      std::ranges::iterator_t<V> it;

      constexpr bool last() const
      {
        return it == std::ranges::end(parent->range) - 1;
      }
      constexpr auto size() const
      {
        return std::ranges::size(parent->range);
      }
    public:

      constexpr iterator(adjacent_circular_view& range,
        const auto index)
        : parent{ std::addressof(range) },
        it{ std::ranges::begin(parent->range) + index }
      { assert(size() >= 2U); }

      constexpr typename base_t::reference operator*() const
      {
        //assert(it != std::ranges::end(range));
        return { *it,
          *(last() ? std::ranges::begin(parent->range) : it + 1) };
      }

      constexpr auto& operator+=(
        const typename std::ranges::range_difference_t<V> i)
      {
        it += i;
        return *this;
      }

      constexpr auto operator-(const iterator right)
      {
        return it - right.it;
      }
    };

    V range;

  public:

    constexpr adjacent_circular_view(V range)
      : range{ std::move(range) }
    {}
    constexpr auto begin() { return iterator(*this, 0U); }
    constexpr auto end() { return iterator(*this, std::ranges::size(range)); }
  };

  template <typename R>
  adjacent_circular_view(R&&)->adjacent_circular_view<std::ranges::views::all_t<R>>;

  // view of items selected by indices
  // example:
  
  // #include "cpp_utils/formatter.hpp"
	 
  // #include <array>
  // #include <iostream>
  // #include <string_view>
  // 
  // int main()
  // {
  //     using namespace std;
  //     static constexpr auto indices = std::array{0,2,3};
  //     constexpr nxc::picked_view v("char"sv, indices);
  //     
  //     // prints "c, a, r"
  //     std::cout << fu8<char>(u8"{}", v);
  // }
  
  template <std::ranges::random_access_range V, std::ranges::random_access_range I>
    requires (std::ranges::view<V>&& std::ranges::view<I>)
  class picked_view : public std::ranges::view_interface<picked_view<V, I>>
  {
    struct iterator
      : boost::stl_interfaces::proxy_iterator_interface
      <
      picked_view::iterator,
      std::random_access_iterator_tag,
      std::ranges::range_value_t<V>,
      std::ranges::range_reference_t<V>,
      std::ranges::range_difference_t<V>
      >
    {
    private:
      using base_t = boost::stl_interfaces::proxy_iterator_interface
        <
        picked_view::iterator,
        std::random_access_iterator_tag,
        std::ranges::range_value_t<V>,
        std::ranges::range_reference_t<V>,
        std::ranges::range_difference_t<V>
        >;

      picked_view* parent{};
      typename base_t::difference_type index{};

    public:
      constexpr iterator() = default;

      constexpr iterator(picked_view& parent,
        const decltype(index) index)
        : parent{ std::addressof(parent) },
        index{ index }
      { }

      constexpr typename base_t::reference operator*() const
      {
        //assert(it != std::ranges::end(range));
        return parent->value(index);
      }

      constexpr iterator& operator+=(
        const typename base_t::difference_type i)
      {
        index += i;
        return *this;
      }

      constexpr typename base_t::difference_type operator-(const iterator right) const
      {
        return index - right.index;
      }

      constexpr typename base_t::reference operator[](typename base_t::difference_type i) const
      {
        return *(this + i);
      }
    };

    static_assert(std::sized_sentinel_for<iterator, iterator>);
    static_assert(std::random_access_iterator<iterator>);

    V range;
    I indices;

    constexpr decltype(auto) value(const auto index)
    {
      return range[indices[index]];
    }

  public:
    // default constructor part of the view concept
    constexpr picked_view() = default;
    constexpr picked_view(V range, I indices)
      : range{ std::move(range) }, indices{ std::move(indices) }
    {}

    constexpr auto begin() { return iterator(*this, 0U); }
    constexpr auto end() { return iterator(*this, std::ranges::size(indices)); }
  };

  template <typename R, typename I>
  picked_view(R&&, I&&)->picked_view<std::ranges::views::all_t<R>, std::ranges::views::all_t<I>>;

  template <std::ranges::random_access_range V>
    requires (std::ranges::view<V>)
  class indexed_view : public std::ranges::view_interface<indexed_view<V>>
  {
    struct iterator
      : boost::stl_interfaces::proxy_iterator_interface
      <
      indexed_view::iterator,
      std::random_access_iterator_tag,
      std::pair<std::ranges::range_reference_t<V>, std::ranges::range_difference_t<V>>,
      std::pair<std::ranges::range_reference_t<V>, std::ranges::range_difference_t<V>>,
      std::ranges::range_difference_t<V>
      >
    {
    private:
      using base_t = boost::stl_interfaces::proxy_iterator_interface
        <
        indexed_view::iterator,
        std::random_access_iterator_tag,
        std::pair<std::ranges::range_reference_t<V>, std::ranges::range_difference_t<V>>,
        std::pair<std::ranges::range_reference_t<V>, std::ranges::range_difference_t<V>>,
        std::ranges::range_difference_t<V>
        >;

      indexed_view* parent{};
      typename base_t::difference_type index{};

    public:
      constexpr iterator() = default;

      constexpr iterator(indexed_view& parent, const decltype(index) index)
        : parent{ std::addressof(parent) }, index{ index }
      { }

      constexpr typename base_t::reference operator*() const
      {
        //assert(it != std::ranges::end(range));
        return { parent->range[index], index };
      }

      constexpr iterator& operator+=(
        const typename base_t::difference_type i)
      {
        index += i;
        return *this;
      }

      constexpr typename base_t::difference_type operator-(const iterator right) const
      {
        return index - right.index;
      }

      constexpr typename base_t::reference operator[](typename base_t::difference_type i) const
      {
        return *(*this + i);
      }
    };

    static_assert(std::sized_sentinel_for<iterator, iterator>);
    static_assert(std::random_access_iterator<iterator>);

    V range;

  public:
    // default constructor part of the view concept
    constexpr indexed_view(V range)
      : range{ std::move(range) }
    {}

    constexpr auto begin() { return iterator(*this, 0); }
    constexpr auto end() { return iterator(*this, std::ranges::size(range)); }
  };

  template <typename R>
  indexed_view(R&&)->indexed_view<std::ranges::views::all_t<R>>;

}

#endif
