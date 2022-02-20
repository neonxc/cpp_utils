#ifndef NXC_COMMON_INCLUDED
#define NXC_COMMON_INCLUDED

#include <string_view>

#if __has_include("Arduino.h")

#include "Arduino.h"
#undef abs
#undef radians
#undef max
#undef min
#undef round

#endif

namespace nxc
{
  void error(const std::u8string_view);
}

#endif
