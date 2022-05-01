
find_package(catch2)

add_executable(tests test_functions.cpp)
# target_link_libraries(tests catch2::catch2)
target_link_libraries(tests PUBLIC catch2::Catch2WithMain functions)

include(CTest)
include(Catch)
catch_discover_tests(tests)
