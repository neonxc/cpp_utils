// custom coroutine infrastructure providing nesting and execution via operator()
// example of use:
//
//  struct A
//  {
//      int a;
//      char b;
//  };
//  
//  // convenience wrapper choosing std::chrono::system_clock as implicit waiting clock
//  template <typename Rep, typename Period>
//  auto operator co_await(const std::chrono::duration<Rep, Period> duration)
//  {
//      using nxc::operator co_await;
//      return nxc::operator co_await<std::chrono::system_clock>(duration);
//  }
//  
//  void nxc::error(const std::u8string_view sv)
//  {
//      throw nxc::unicode_exception{std::u8string{sv}};
//  }
//  
//  using namespace std::chrono_literals;
//  
//  nxc::polled_task<A> nested()
//  {
//      co_await 1s;
//      co_return A{1, 'c'};
//  };
//  
//  nxc::polled_task<A> f()
//  {
//      auto nestedTask = nested();
//      co_await 3s;
//  
//      co_return co_await nestedTask;
//  }
//  
//  #include <iostream>
//  
//  int main()
//  {
//    auto task = f();
//    while (not task()) //  <-- this is the actual polling and executing of the coroutines 
//    { }
//  
//    std::cout << task.get().b;
//  }

#ifndef NXC_POLLED_TASK_INCLUDED
#define NXC_POLLED_TASK_INCLUDED

#include "cpp_utils/common.hpp"

#include <concepts>
#include <chrono>
#include <memory>
#include <new>
#include <optional>
#include <utility>

#if __has_include(<coroutine>)

#include <coroutine>

namespace nxc
{
  namespace coro_ns = std;
}

#elif __has_include(<experimental/coroutine>)
#include <experimental/coroutine>

namespace nxc
{
  namespace coro_ns = std::experimental;
}

#endif

#if not __has_include(<Arduino.h>)

namespace nxc
{
  template <typename T>
  using default_allocator = std::allocator<T>;
}

#else // arduino

namespace nxc
{

template <typename T>
class default_allocator
{
    public:
    using value_type = T;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using propagate_on_container_move_assignment = std::true_type;
    using is_always_equal = std::true_type;

    [[nodiscard]] constexpr T* allocate( std::size_t n )
    {
        return ::new T[n];
    }

    constexpr void deallocate( T* p, std::size_t n )
    {
        delete p[];
    }

    friend bool operator==(const default_allocator&) = default;
};

}

#endif

namespace nxc
{

  template <typename T = void, typename Alloc = default_allocator<unsigned char>>
  class polled_task;
  
  template <typename T, typename Alloc>
  class polled_task_promise;
  
  namespace detail
  {
    template <typename Alloc>
    struct polled_task_promise_base
    {
      template <typename Promise = void>
      using handle_t = coro_ns::coroutine_handle<Promise>;
      using AllocatorForU = typename std::allocator_traits<Alloc>::template rebind_alloc<unsigned char>;
  
      handle_t<> caller_handle{};
      polled_task_promise_base* caller_promise{};
  
      coro_ns::coroutine_handle<> nested_handle{};
  
      auto nested() const { return nested_handle; }
  
      template<typename... Args>
      static void* operator new  (const std::size_t sz, std::allocator_arg_t, Alloc oldAlloc = {}, Args&& ...)
      {
        AllocatorForU allocator{ std::move(oldAlloc) };
  
        // Round up sz to next multiple of ALLOCATOR alignment
        std::size_t allocatorOffset =
          (sz + alignof(AllocatorForU) - 1u) & ~(alignof(AllocatorForU) - 1u);
  
        // Call onto allocator to allocate space for coroutine frame.
        void* ptr = std::allocator_traits<AllocatorForU>::allocate(allocator, allocatorOffset + sizeof(AllocatorForU));
  
        // Take a copy of the allocator (assuming noexcept copy constructor here)
        new (((char*)ptr) + allocatorOffset) AllocatorForU(std::move(allocator));
  
        return ptr;
      }
  
      static void* operator new  (std::size_t sz)
      {
        return operator new(sz, std::allocator_arg);
      }
  
      static void* operator new  (std::size_t sz)
        requires std::same_as<AllocatorForU, default_allocator<unsigned char>>
      {
        // no need to store the allocator when stateless
        return new unsigned char[sz];
      }
  
      static void operator delete(void* ptr, std::size_t sz)
      {
        std::size_t allocatorOffset =
          (sz + alignof(AllocatorForU) - 1u) & ~(alignof(AllocatorForU) - 1u);
  
        AllocatorForU& allocator = *reinterpret_cast<AllocatorForU*>(
          ((char*)ptr) + allocatorOffset);
  
        // Move allocator to local variable first so it isn't freeing its
        // own memory from underneath itself.
        // Assuming allocator move-constructor is noexcept here.
        AllocatorForU allocatorCopy = std::move(allocator);
  
        // But don't forget to destruct allocator object in coroutine frame
        allocator.~AllocatorForU();
  
        // Finally, free the memory using the allocator.
        allocatorCopy.deallocate(reinterpret_cast<unsigned char*>(ptr), allocatorOffset + sizeof(AllocatorForU));
      }
  
      static void operator delete(void* ptr, std::size_t)
        requires std::same_as<AllocatorForU, default_allocator<unsigned char>>
      {
        delete[] reinterpret_cast<unsigned char*>(ptr);
      }
    };
  
    template <typename T, typename Actual, typename Alloc>
    class shared_polled_task_promise
    {
      template <typename Promise = void>
      using handle_t = coro_ns::coroutine_handle<Promise>;
    public:
      coro_ns::suspend_always initial_suspend() { return {}; }
  
      struct final_awaitable {
        bool await_ready() const noexcept { return false; }
  
        template <typename PROMISE>
        handle_t<> await_suspend(
          handle_t<PROMISE> coro) noexcept
        {
          const auto calling_coro = coro.promise().caller_handle;
          if (calling_coro)
            return calling_coro;
          return coro_ns::noop_coroutine();
        }
  
        void await_resume() noexcept {}
      };
  
      static auto& get_first_promise(polled_task_promise_base<Alloc>& promise)
      {
        auto* uplink_promise = &promise;
  
        while (uplink_promise->caller_handle)
        {
          uplink_promise = uplink_promise->caller_promise;
        }
  
        return *uplink_promise;
      }
  
      Actual& actual()
      {
        return static_cast<Actual&>(*this);
      }
  
      const Actual& actual() const
      {
        return static_cast<const Actual&>(*this);
      }
  
      auto final_suspend() noexcept
      {
        if (actual().caller_handle)
        {
          auto& first_promise = get_first_promise(*actual().caller_promise);
          if (&first_promise != actual().caller_promise)
            first_promise.nested_handle = actual().caller_handle;
          else
            first_promise.nested_handle = nullptr;
        }
  
        return final_awaitable{};
      }
  
      void unhandled_exception() noexcept { error(u8"unhandled"); }
  
      template <typename Promise>
      void set_continuation(handle_t<Promise> continuation)
      {
        using actual_type = polled_task_promise<T, Alloc>;
  
        actual().caller_handle = continuation;
        actual().caller_promise = &continuation.promise();
        auto& first_promise = get_first_promise(*actual().caller_promise);
        first_promise.nested_handle =
          handle_t<actual_type>::from_promise(static_cast<actual_type&>(*this));
      }
    };
  
    template <typename T, typename Actual, typename Alloc>
    class valued_polled_task_promise : public shared_polled_task_promise<T, Actual, Alloc>
    {
      using base_t = detail::shared_polled_task_promise<T, Actual, Alloc>;
  
    public:
      using value_type = std::remove_reference_t<T>;
      using reference_type = std::conditional_t<std::is_reference_v<T>, T, T&>;
  
      using base_t::shared_polled_task_promise;
  
      valued_polled_task_promise(const valued_polled_task_promise& other) = delete;
  
      // int f(){ return c; return std::move(c); }
      void return_value(std::remove_reference_t<T>& value)
        noexcept(std::is_nothrow_move_constructible_v<T>)
        requires (!std::is_rvalue_reference_v<T>)
      {
        new(&m_value) value_type{ std::move(value) };
        m_result_present = true;
      }
  
      // int f(){ return const c; }
      void return_value(const std::remove_reference_t<T>& value)
        noexcept(std::is_nothrow_copy_constructible_v<T>)
        requires (!std::is_rvalue_reference_v<T>)
      {
        new(&m_value) value_type{ value };
        m_result_present = true;
      }
  
      // int&& f() { return c; return std::move(c); return const c;? }
      void return_value(std::remove_reference_t<T>&& value)
        noexcept(std::is_nothrow_move_constructible_v<T>)
      {
        new(&m_value) value_type{ std::move(value) };
        m_result_present = true;
      }
  
      reference_type result() const noexcept
      {
        return reinterpret_cast<reference_type>(m_value);
      }
  
      ~valued_polled_task_promise()
      {
        if (m_result_present)
          result().~value_type();
      }
  
    private:
      mutable std::aligned_storage_t<sizeof(value_type), alignof(value_type)> m_value;
      bool m_result_present{ false };
    };
  
    template <typename T, typename Actual, typename Alloc>
    class valued_polled_task_promise<T&, Actual, Alloc> : public shared_polled_task_promise<T&, Actual, Alloc>
    {
      using base_t = detail::shared_polled_task_promise<T&, Actual, Alloc>;
  
    public:
  
      using value_type = T;
      using reference_type = T&;
      using pointer_type = value_type*;
  
      using shared_polled_task_promise<T&, Actual, Alloc>::shared_polled_task_promise;
  
      // int& f() { return c; }
      // const int& f() { return c; }
      // const int& f() { return as_const(c); }
      // const int& f() { return move(c); }
      void return_value(T& value) noexcept
      {
        m_pointer = std::addressof(value);
      }
  
      reference_type result() const noexcept
      {
        return *m_pointer;
      }
  
    private:
      pointer_type m_pointer;
    };
  
    template <typename Actual, typename Alloc>
    class void_polled_task_promise : public shared_polled_task_promise<void, Actual, Alloc>
    {
    public:
      void return_void()
      {
      }
    };
  
    template <typename T, typename Actual, typename Alloc>
    class polled_task_promise_common : public polled_task_promise_base<Alloc>
    {
    public:
      polled_task<T> get_return_object() noexcept;
    };
  }
  
  template <typename T, typename Alloc>
  class polled_task_promise final
    : public detail::valued_polled_task_promise<T, polled_task_promise<T, Alloc>, Alloc>,
    public detail::polled_task_promise_common<T, polled_task_promise<void, Alloc>, Alloc>
  {
  };
  
  template <typename Alloc>
  class polled_task_promise<void, Alloc> final
    : public detail::void_polled_task_promise<polled_task_promise<void, Alloc>, Alloc>,
    public detail::polled_task_promise_common<void, polled_task_promise<void, Alloc>, Alloc>
  {
  };
  
}

template <typename T, typename Alloc, typename ...Args>
requires (!std::same_as<typename nxc::polled_task_promise<T, Alloc>::AllocatorForU, nxc::default_allocator<unsigned char> >)
struct nxc::coro_ns::coroutine_traits<nxc::polled_task<T, Alloc>, std::allocator_arg_t, Alloc, Args...>
{
  using promise_type = nxc::polled_task_promise<T, Alloc>;
};

template <typename T, typename ...Args>
struct nxc::coro_ns::coroutine_traits<nxc::polled_task<T>, Args...>
{
  using promise_type = nxc::polled_task_promise<T, nxc::default_allocator<unsigned char>>;
};

namespace nxc
{

  template <typename T, typename Alloc>
  class [[nodiscard]] polled_task
  {
    using promise_type = polled_task_promise<T, Alloc>;
  public:
    using handle_type = coro_ns::coroutine_handle<promise_type>;
  
    using value_type = T;
  
  private:
    struct awaitable_base
    {
      handle_type m_coroutine;
  
      awaitable_base(handle_type coroutine) noexcept : m_coroutine(coroutine) {}
  
      bool await_ready() const noexcept
      {
        return m_coroutine.done();
      }
  
      template <typename P>
      coro_ns::coroutine_handle<> await_suspend(
        coro_ns::coroutine_handle<P> awaitingCoroutine) noexcept {
        m_coroutine.promise().set_continuation(awaitingCoroutine);
        return m_coroutine;
      }
    };
  
  public:
    constexpr polled_task() noexcept = default;
  
    explicit polled_task(handle_type coroutine) : m_coroutine(coroutine) {}
  
    polled_task(polled_task&& t) noexcept
      : m_coroutine(t.m_coroutine)
    {
      t.m_coroutine = nullptr;
    }
  
    /// Disable copy construction/assignment.
    polled_task(const polled_task&) = delete;
    polled_task& operator=(const polled_task&) = delete;
  
    /// Frees resources used by this task.
    ~polled_task()
    {
      if (m_coroutine) {
        m_coroutine.destroy();
      }
    }
  
    polled_task& operator=(polled_task&& other) noexcept
    {
      if (&other != this) {
        if (m_coroutine) {
          m_coroutine.destroy();
        }
  
        m_coroutine = other.m_coroutine;
        other.m_coroutine = nullptr;
      }
  
      return *this;
    }
  
    constexpr operator bool() const
    {
      return static_cast<bool>(m_coroutine);
    }
  
    bool operator()()
    {
      auto nested = m_coroutine.promise().nested();
      if (nested)
      {
        nested.resume();
      }
      else if (not m_coroutine.done())
      {
        m_coroutine.resume();
      }
      return m_coroutine.done();
    }
  
    /// \brief
    /// Query if the task result is complete.
    ///
    /// Awaiting a task that is ready is guaranteed not to block/suspend.
    bool done() const noexcept { return !m_coroutine || m_coroutine.done(); }
  
    auto operator co_await() const noexcept
    {
      struct awaitable : awaitable_base
      {
        const polled_task* tis;
  
        decltype(auto) await_resume()
        {
          if constexpr (std::is_void_v<T>)
            return;
          else
          {
            return tis->get();
          }
        }
      };
  
      return awaitable{ {m_coroutine}, this };
    }
  
    T get() const
      noexcept (std::is_nothrow_move_constructible_v<T>)
      requires (!std::is_void_v<T> && !std::is_lvalue_reference_v<T>)
    {
      const auto& promise = this->m_coroutine.promise();
      return std::move(promise.result());
    }
  
    T get() const
      noexcept
      requires (!std::is_void_v<T> && std::is_lvalue_reference_v<T>)
    {
      const auto& promise = this->m_coroutine.promise();
      return promise.result();
    }
  
  private:
    coro_ns::coroutine_handle<promise_type> m_coroutine{};
  };
  
  template <typename T, typename Actual, typename Alloc>
  polled_task<T>
  detail::polled_task_promise_common<T, Actual, Alloc>::get_return_object() noexcept
  {
    using actual_type = polled_task_promise<T, Alloc>;
    return polled_task<T>{
      coro_ns::coroutine_handle<actual_type>::from_promise(static_cast<actual_type&>(*this))};
  }
  
  template <typename Clock, typename Rep, typename Period>
  polled_task<void> wait(const std::chrono::duration<Rep, Period> duration)
  {
    using namespace std::chrono;
    const auto begin = Clock::now();
    while (begin + duration > Clock::now())
    {
      co_await coro_ns::suspend_always{};
    }
  }
  
  template <typename Clock, typename Duration>
  polled_task<void> wait(const std::chrono::time_point<Clock, Duration> timePoint)
  {
    while (timePoint > Clock::now())
    {
      co_await coro_ns::suspend_always{};
    }
  }
  
  template <typename T>
  T synchronize(polled_task<T>& task)
  {
    while (not task())
    { }
  
    if constexpr (std::is_void_v<T>)
      return;
    else
      return std::move(task).get();
  }
  
  namespace impl
  {
    struct awaitable
    {
      polled_task<void> task;
  
      awaitable(polled_task<void> t) noexcept : task{ std::move(t) } {}
  
      bool await_ready() const noexcept
      {
        return task.done();
      }
  
      template <typename Promise>
      coro_ns::coroutine_handle<> await_suspend(
        coro_ns::coroutine_handle<Promise> awaitingCoroutine) noexcept {
        auto a = task.operator co_await();
        return a.await_suspend(awaitingCoroutine);
      }
  
      void await_resume()
      {
      }
    };
  }
  
  template <typename Clock, typename Rep, typename Period>
  auto operator co_await(const std::chrono::duration<Rep, Period> duration)
  {
    return impl::awaitable{ wait<Clock>(duration) };
  }
  
  template <typename Clock, typename Duration>
  auto operator co_await(const std::chrono::time_point<Clock, Duration> timePoint)
  {
    return impl::awaitable{ wait<Clock>(timePoint) };
  }
  
}

#endif