#ifndef NXC_FIXED_BUFFER
#define NXC_FIXED_BUFFER

#include <array>
#include <atomic>
#include <concepts>
#include <optional>

namespace nxc
{
	
  // TODO: add (atomic_signal)_fences
	
  template <std::semiregular T>
  class fixed_buffer
  {
    using raw_t = std::remove_volatile_t<T>;
    using buffer_t = std::array
      <
        T,
  	    // fixed buffer size to enable index wrapping for best performance 
        256
      >;
    using index_t = std::atomic<unsigned char>;
  
    buffer_t buffer;
  
    index_t head{ 0 };
    index_t tail{ 0 };
  
    std::atomic<bool> full_flag{ false };
    bool empty() const { return head == tail && not full(); }
  
    void increment(index_t& index)
    {
      // no need to be atomic
      index = index+1; // auto wrap
    }
  
  public:
    constexpr bool full() const { return full_flag; }
  
    bool push(raw_t val)
    {
      if (full())
      {
        return false;
      }
      
      buffer[head] = std::move(val);
  
      increment(head);
  
      if (head == tail)
      {
        full_flag = true;
      }
      return true;
    }
  
    std::optional<raw_t> pop()
    {
      if (empty())
        return std::nullopt;
  
      auto val = std::move(buffer[tail]);
      full_flag = false;
      increment(tail);
  
      return std::move(val);
    }
  };
}

#endif
