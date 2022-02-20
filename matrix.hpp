#ifndef NXC_MATRIX_INCLUDED
#define NXC_MATRIX_INCLUDED

#include <boost/stl_interfaces/iterator_interface.hpp>

#include <concepts>

namespace nxc
{
  template<typename Matrix>
  class matrix_iterator
    : public boost::stl_interfaces::
    iterator_interface<matrix_iterator<Matrix>,
    std::random_access_iterator_tag,
    std::ranges::range_value_t<std::ranges::range_value_t<Matrix>>,
    std::ranges::range_reference_t<std::ranges::range_reference_t<Matrix>>>
  {
    template <typename Matrix>
    friend class matrix_iterator;

    using index_t = std::ptrdiff_t;
    index_t index;
    Matrix* matrix;

    constexpr auto partial_indices() const
    {
      constexpr auto colsNumber = std::tuple_size_v<std::ranges::range_value_t<Matrix>>;
      return std::pair<index_t, index_t>
      {
        index / colsNumber, // i
          index% colsNumber  // j
      };
    }

  public:
    constexpr matrix_iterator() = default;
    constexpr matrix_iterator(Matrix& matrix, const index_t index)
      : index{ index }, matrix{ &matrix }
    { }

    // converting ctor from iterator to convertible type
    template<typename Matrix2>
      requires std::convertible_to<std::ranges::range_value_t<Matrix2>*, std::ranges::range_value_t<Matrix>*>
    constexpr matrix_iterator(
      matrix_iterator<Matrix2> other) noexcept
      :
      index{ other.index },
      matrix{ other.matrix }
    {}

    constexpr auto operator-(const matrix_iterator it) const
    {
      return index - it.index;
    }
    constexpr auto& operator+=(const index_t offset)
    {
      index += offset;
      return *this;
    }

    constexpr auto& operator*() const
    {
      const auto [i, j] = partial_indices();
      return (*matrix)[i][j];
    }
  };

  template <typename T, auto rows_n, auto cols_n>
  class matrix
  {
  public:
    static constexpr auto cols_number{ cols_n };
    static constexpr auto rows_number{ rows_n };

  private:
    std::array<std::array<T, cols_number>, rows_number> items;

  public:
    using iterator = matrix_iterator<decltype(items)>;
    using const_iterator = matrix_iterator<const decltype(items)>;

    constexpr matrix() = default;

    constexpr matrix(auto generator)
      : items{ make_multi_array<rows_number, cols_number>(generator) }
    {
    }

    constexpr iterator begin()
    {
      return { items, 0U };
    }
    constexpr iterator end()
    {
      return { items, cols_number * rows_number };
    }

    constexpr const_iterator begin() const { return { items, 0U }; }
    constexpr const_iterator end() const
    {
      return { items, cols_number * rows_number };
    }

    friend constexpr iterator remove_constness(matrix& m, const_iterator it)
    {
      return m.begin() + std::distance(static_cast<const matrix&>(m).begin(), it);
    }
  };

  template <typename T, auto rows_n, auto cols_n>
  constexpr auto begin(const matrix<T, rows_n, cols_n>& m) { return m.begin(); }
  template <typename T, auto rows_n, auto cols_n>
  constexpr auto end(const matrix<T, rows_n, cols_n>& m) { return m.end(); }
  template <typename T, auto rows_n, auto cols_n>
  constexpr auto begin(matrix<T, rows_n, cols_n>& m) { return m.begin(); }
  template <typename T, auto rows_n, auto cols_n>
  constexpr auto end(matrix<T, rows_n, cols_n>& m) { return m.end(); }

  template <auto rows_number, auto cols_number, typename Generator>
  constexpr auto make_matrix(Generator gen)
    -> matrix<decltype(gen(make_const<rows_number>(), make_const<cols_number>())),
    rows_number, cols_number>
  {
    return { gen };
  }
}

#endif
