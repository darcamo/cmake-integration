project(TestCmakeIntegration)

add_library(somelib
  funcs.h
  funcs.cpp
)

add_executable(main main.cpp)
set_target_properties(main
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
target_link_libraries(main somelib)
