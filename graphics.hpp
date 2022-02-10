#ifndef NXC_GRAPHICS_INCLUDED
#define NXC_GRAPHICS_INCLUDED

#include "cpp_utils/funcs.hpp"
#include "cpp_utils/physics.hpp"
#include "cpp_utils/views.hpp"

#include <boost/operators.hpp>

#include <algorithm>
#include <cassert>
#include <iterator>
#include <ranges>
#include <stdexcept>

namespace nxc::graphics_2d
{
  enum class direction_t { none, up, down, left, right };

  template <typename Offset>
  struct vector : boost::additive1<vector<Offset>>
  {
    using offset_t = Offset;

    offset_t x{};
    offset_t y{};

    constexpr vector() = default;
    constexpr vector(offset_t x, offset_t y) : x{x}, y{y} { }

    constexpr vector& operator-=(const vector right)
    {
      x -= right.x;
      y -= right.y;
      return *this;
    }

    constexpr vector& operator+=(const vector right)
    {
      x += right.x;
      y += right.y;
      return *this;
    }

    constexpr vector operator-() const
    {
      return vector{ -x, -y };
    }
  };

  template <typename Offset>
  vector(Offset, Offset) -> vector<Offset>;

  template <typename Angle>
  inline static constexpr auto rotate_factory = [](const physics::radians<Angle> angle)
  {
    // no need to compute sin & cos repeatedly for one angle
    return[sin = sin(angle), cos = cos(angle)]
    (auto& v)
    {
      v = { v.x * cos + v.y * sin, v.y * cos - v.x * sin };
    };
  };

  template <typename Position>
  struct point
  {
    using position_t = Position;
    using offset_t = decltype(position_t{} - position_t{});
    using vec_t = vector<offset_t>;

    position_t x;
    position_t y;

    constexpr point& operator += (const vec_t o)
    {
      x += o.x;
      y += o.y;
      return *this;
    }

    constexpr point operator + (const vec_t o) const
    {
      return point{ *this } += o;
    }

    constexpr point operator -() const
    {
      return { -x, -y };
    }

    constexpr vec_t operator - (const point o) const
    {
      return { x - o.x, y - o.y };
    }
  };

  template <typename Position>
  point(Position, Position) -> point<Position>;

  template <typename Offsets,
    typename Position = typename std::ranges::range_value_t<Offsets>::offset_t>
  struct shape
  {
    using offset_t = typename std::ranges::range_value_t<Offsets>::offset_t;
    using position_t = Position;
    using point_t = point<position_t>;
    using vec_t = vector<offset_t>;

    point_t c;
    Offsets points_offsets;

    constexpr auto& centre() { return c; }
    constexpr auto centre() const { return c; }
    constexpr const Offsets& points_offsets_to_centre() const
    {
      return points_offsets;
    }

    constexpr shape(const point_t centre, const Offsets& pointsOffsetsToCentre)
      : c{ centre }, points_offsets{ pointsOffsetsToCentre }
    {}
  };

  template <typename Shape, typename Angle>
  constexpr void rotate(Shape& sh, const physics::radians<Angle> angle)
  {
    sh.angle() += angle;
  }

  template <typename Shape>
  constexpr void move(Shape& sh, const typename Shape::vec_t move_offset)
  {
    sh.centre() += move_offset;
  }

  template <typename Position, typename Angle>
  struct rectangle : shape<std::array<vector<Position>, 4U>>
  {
    using shape_t = shape<std::array<vector<Position>, 4U>>;
    using typename shape_t::offset_t;
    using typename shape_t::position_t;
    using typename shape_t::vec_t;
    using typename shape_t::point_t;

  private:
    static constexpr auto
      calculate_points_offsets(const offset_t width, const offset_t height)
    {
      return std::array
      {
        vec_t{ -width / 2, -height / 2 },
        vec_t{ width / 2, -height / 2 },
        vec_t{ -width / 2,  height / 2 },
        vec_t{ width / 2,  height / 2 }
      };
    }

  public:
    physics::radians<Angle> a{};

    constexpr rectangle(const point_t centre,
      const offset_t width, const offset_t height,
      const physics::radians<Angle> angle = {})
      : shape_t{ centre, calculate_points_offsets(width, height) },
      a{angle}
    {}
    physics::radians<Angle>& angle() { return a; }
    physics::radians<Angle> angle() const { return a; }

    auto vertices() const
    {
      using namespace std::ranges::views;
      const auto rotate = rotate_factory<Angle>(a);

      return this->points_offsets | transform([this, rotate](auto offset)
        {
          rotate(offset);
          return this->centre() + offset;
        });
    }

    constexpr auto width() const { return this->offsets[1].x * 2; }
    constexpr auto height() const { return this->offsets[2].y * 2; }
  };

  template <typename Position, typename Angle>
  class progress_bar
  {
    // order of initialization dependency!
    const rectangle<Position, Angle> border;
    const rectangle<Position, Angle> bar;
    rectangle<Position, Angle> fill;

    constexpr rectangle<Position, Angle> compute_bar_fill(const Position value)
    {
      const point<Position> centre
      {
        this->centre().x - border.width() / 2 + (value * bar.width / (100*2)),
        this->centre().y
      };
      return {centre, this->centre().x - bar.width() / 2 + value * bar.width() / 100, bar.height() };
    }

  public:
    constexpr progress_bar(const rectangle<Position, Angle> border, const Position value = 0)
      : border{ border },
        bar{ border.centre(),
          border.width() - 10,
          border.height() - 10 },
        fill{ compute_bar_fill(value) }
    {}

    constexpr void update(const Position value)
    {
      fill = compute_bar_fill(value);
    }

    constexpr point<Position> centre() const { return border.centre(); }
  };

  template <typename Offset>
  inline auto length(const vector<Offset> v)
  {
    return std::hypot(v.x, v.y);
  }

  template <typename Offset>
  inline constexpr auto dot(const vector<Offset> u, const vector<Offset> v)
  {
    return u.x * v.x + u.y * v.y;
  }

  template <typename Offset>
  inline auto cos(const vector<Offset> u, const vector<Offset> v)
  {
    return dot(u, v) / (length(u) * length(v));
  }

  template <typename Offset, typename Angle>
  inline physics::radians<Angle> angle(const vector<Offset> u, const vector<Offset> v)
  {
    return std::acos(cos(u, v));
  }

  template <typename Offset>
  inline auto projection(const vector<Offset> u, const vector<Offset> v)
  {
    return dot(u, v) / length(v);
  }

  template <typename Offset>
  inline auto normal(const vector<Offset> v)
  {
    return vector<Offset>{ v.y, -v.x };
  }

  template <std::ranges::viewable_range Range, auto indices = std::array{ 0,1,3,2 } >
  requires std::ranges::random_access_range<Range>
  constexpr auto rectangle_edges_view(Range&& vertices)
  {
    using namespace std::ranges;
    auto iv = nxc::picked_view(std::forward<Range>(vertices), indices);
    static_assert(random_access_range<decltype(iv)>);
    static_assert(view<decltype(iv)>);
    return nxc::adjacent_circular_view(iv);
  }

  // zero sign returned if the set of vertices lies on the same line as edge,
  // or if it lies on both half-spaces divided by the edge
  // guaranteed to return same sign for same inputs
  // guaranteed to return different signs for two sets of vertices,
  // lying in different half-spaces divided by the edge
  template <typename Point>
  auto sign(const std::pair<Point, Point>& edge, const std::ranges::range auto& vertices)
  {
    const auto [first, second] = edge;
    const auto normal = graphics_2d::normal(second - first);

    auto sign = sign_t::zero;
    for (const auto vertex : vertices)
    {
      const auto help = vertex - first;
      const auto dot = graphics_2d::dot(normal, help);
      const auto currentSign = signum(dot);

      if (sign_t::zero == sign)
      {
        sign = currentSign;
      }

      if (sign_t::zero != currentSign)
      {
        if (currentSign != sign)
        {
          sign = sign_t::zero;
          break;
        }
      }
    }

    return sign;
  }

  bool first_has_dividing_edge(const auto& shape, const auto& other)
  {
    auto vertices = shape.vertices();

    for (const auto edge : rectangle_edges_view( vertices ))
    {
      const auto ownShapeSign = graphics_2d::sign(edge, vertices);

      if (sign_t::zero == ownShapeSign)
      {
        throw std::runtime_error{ "unexpected shape" };
      }

      const auto otherShapeSign = graphics_2d::sign(edge, other.vertices());
      if (sign_t::zero != otherShapeSign && otherShapeSign != ownShapeSign)
      {
        return true;
      }
    }

    return false; // no vertex within screen
  }

  bool is_intersection(const auto& shape1, const auto& shape2)
  {
    // https://stackoverflow.com/a/115520/7949231

    return !first_has_dividing_edge(shape1, shape2)
      && !first_has_dividing_edge(shape2, shape1);
  }
}

#endif
