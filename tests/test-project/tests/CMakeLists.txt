
find_package(Catch2 REQUIRED)

add_executable(tests test_functions.cpp)
# target_link_libraries(tests catch2::catch2)
target_link_libraries(tests PUBLIC Catch2::Catch2WithMain functions)

include(CTest)
include(Catch)
catch_discover_tests(tests)
