#ifndef NXC_PHYSICS_INCLUDED
#define NXC_PHYSICS_INCLUDED

#include "cpp_utils/common.hpp"

#include <cmath>
#include <concepts>
#include <chrono>
#include <functional>
#include <numbers>

namespace nxc::physics
{
  template <typename Angle>
  constexpr auto pi = std::numbers::pi_v<Angle>;

  template <typename Angle>
  constexpr Angle full_angle = 2 * pi<Angle>;

  template <typename Angle>
  requires (std::signed_integral<Angle> || std::floating_point<Angle>)
  struct radians
  {
  private:
    Angle m_value; // always [0;2*pi)
  public:
    
    radians() = default;
    radians(const Angle angle)
      : m_value{ fmod(angle, full_angle<Angle>) }
    {}

    radians& operator+=(const radians right)
    {
      return *this = (m_value + right.m_value);
    }

    radians& operator-=(const radians right)
    {
      return *this = (m_value - right.m_value + full_angle<Angle>);
    }

    radians operator+(const radians right) const
    {
      return radians{ *this } += right;
    }

    radians operator-(const radians right) const
    {
      return radians{ *this } -= right;
    }

    constexpr Angle value() const { return m_value; }
  };

  template <typename Angle>
  inline auto sin(const radians<Angle> angle)
  {
    return ::sin(angle.value());
  }

  template <typename Angle>
  inline auto cos(const radians<Angle> angle)
  {
    return ::cos(angle.value());
  }

  template <typename T>
  concept chrono_duration =
    std::same_as<const volatile T, const volatile std::chrono::duration<typename T::rep, typename T::period>>;

  template <typename T>
  concept chrono_time_point =
    std::same_as<const volatile T, const volatile std::chrono::time_point<typename T::clock, typename T::duration>>;

  template <chrono_duration Duration>
  constexpr auto freq_to_period(const auto hz)
  {
    using reduced_period = typename Duration::period::type;
    return Duration
    {
      static_cast<typename Duration::rep>( reduced_period::den / (reduced_period::num * hz))
    };
  }

  constexpr auto millis_count(const chrono_duration auto duration)
  {
    using namespace std::chrono;
    return duration_cast<milliseconds>(duration).count();
  }

  template <typename Clock>
  auto millis_from_epoch()
  {
    using namespace std::chrono;
    return millis_count(Clock::now().time_since_epoch());
  }

  template <typename Angle>
  constexpr radians<Angle> phase(const chrono_duration auto time,
    const chrono_duration auto period)
  {
    // phi = 2*pi*t/T
    return (full_angle<Angle> * time) / period;
  }

  template <typename Angle>
  auto sine_wave(
    const chrono_duration auto timeStamp, 
    const chrono_duration auto period,
    const radians<Angle> phaseOffset = {})
  {
    return sin(phase<Angle>(timeStamp, period) - phaseOffset);
  }

  template <typename Angle>
  auto sine_wave(
    const chrono_duration auto timeStamp,
    const chrono_duration auto period,
    const chrono_duration auto offset)
  {
    return sine_wave<Angle>(timeStamp - offset, period);
  }

  template <typename Angle>
  auto cosine_wave(
    const chrono_duration auto timeStamp,
    const chrono_duration auto period,
    const radians<Angle> phaseOffset = {})
  {
    return cos(phase<Angle>(timeStamp, period) - phaseOffset);
  }

  template <typename Angle>
  auto cosine_wave(
    const chrono_duration auto t, const chrono_duration auto period,
    const chrono_duration auto offset)
  {
    return cosine_wave<Angle>(t - offset, period);
  }

  // pwm generator
  //
  //        |<offset><---------period--------><duty>
  // true:  |        _____                    _____                
  // false: |________|    |___________________|    |___________________
  //        0                                           --> t
  //
  constexpr bool pwm(const chrono_duration auto period,
    const chrono_duration auto dutyDuration,
    const chrono_duration auto offset,
    const chrono_time_point auto t)
  {
    using namespace std::chrono_literals;
    const auto currentTimePoint = (t - offset).time_since_epoch();
    return abs(currentTimePoint % period) < dutyDuration;
  }

  template <std::totally_ordered T>
  class hysteresis
  {
    enum class status_t { alpha, beta, k };

    T alpha_limit;
    T beta_limit;

    bool last_status;

    template <typename Val>
    status_t compare(const Val& x)
    {
      if (std::less_equal{}(x, alpha_limit))
        return status_t::alpha;
      if (std::less{}(x, beta_limit))
        return status_t::k;

      return status_t::beta;
    }
  public:

    constexpr hysteresis(const T& alphaLimit, const T& betaLimit,
      const bool previousStatus = false)
      : alpha_limit{ alphaLimit }, beta_limit{ betaLimit },
      last_status{ previousStatus }
    {}

    template <typename Val>
    constexpr bool update(const Val& x)
    {
      switch (compare(x))
      {
      case status_t::alpha:
        return (last_status = false);
      case status_t::k:
        return last_status;
      case status_t::beta:
        return (last_status = true);
      default:
        assert(false);
      }
    }
    constexpr bool status() const
    {
      return last_status;
    }
  };
}

#endif