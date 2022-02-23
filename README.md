# cpp_utils

library: various utilities to reuse code in my personal projects. main feature is genericity and portability. one should not find himself being dragged somewhere using this code, in fact many hooks are installed to provide customization (allocator awareness, exceptions support). my use covers PC apps as well as embedded devices. 

dependencies: c++ boost library for full functionality. C++20 compiler required.

how to use: simply #include respective header files. all new entitites are declared in namespace 'nxc', apart from that new specializations of std::format are declared in formatter.hpp.
