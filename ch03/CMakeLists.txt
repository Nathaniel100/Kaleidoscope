
SET(LLVM_CXXFLAGS "-fPIC -fvisibility-inlines-hidden -Werror=date-time -std=c++11 -Wall -W -Wno-unused-parameter -Wwrite-strings -Wcast-qual -Wmissing-field-initializers -pedantic -Wno-long-long -Wcovered-switch-default -Wnon-virtual-dtor -Wdelete-non-virtual-dtor -Wstring-conversion -g  -fno-exceptions -fno-rtti -DLLVM_BUILD_GLOBAL_ISEL -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -Wl,-search_paths_first -Wl,-headerpad_max_install_names")
SET(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} ${LLVM_CXXFLAGS})

link_directories(/Users/wufan/llvm/lib)
add_executable(Kaleidoscope-03 main.cpp)
target_link_libraries(Kaleidoscope-03 LLVMCore LLVMSupport LLVMDemangle curses z m)

