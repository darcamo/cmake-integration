add_library(functions SHARED
  functions.h
  functions.cpp
)
target_include_directories(functions PUBLIC ${CMAKE_CURRENT_LIST_DIR})

add_executable(main main.cpp)
target_link_libraries(main functions)
