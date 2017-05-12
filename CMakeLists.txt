cmake_minimum_required(VERSION 3.7)
project(Kaleidoscope)

set(CMAKE_CXX_STANDARD 11)

SET(LLVM_HOME /Users/wufan/llvm)

include_directories(${LLVM_HOME}/include)

set(SOURCE_FILES main.cpp make_unique.h)
add_executable(Kaleidoscope ${SOURCE_FILES})

add_subdirectory(ch02)

add_subdirectory(ch03)


