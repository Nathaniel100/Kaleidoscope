cmake_minimum_required(VERSION 3.7)
project(Kaleidoscope)

set(CMAKE_CXX_STANDARD 11)

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)

#set(SOURCE_FILES main.cpp make_unique.h)
#add_executable(Kaleidoscope ${SOURCE_FILES})

add_subdirectory(ch02)

add_subdirectory(ch03)


