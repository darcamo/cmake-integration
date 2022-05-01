project(TestCmakeIntegration)
cmake_minimum_required(VERSION 3.23)

# list(APPEND CMAKE_PREFIX_PATH "/home/darlan/git_files/cmake-integration/tests/test-project/build/")
# list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

# We run conan from the build folder, which is where it will save the
# generated files to find the packages installed with conan
# list(APPEND CMAKE_PREFIX_PATH ${CMAKE_BINARY_DIR})

# message(STATUS "Conteúdo do CMAKE_BIBNARY_DIR")
# message(STATUS ${CMAKE_BINARY_DIR})

# message(STATUS "Conteúdo do CMAKE_PREFIX_PATH")
# message(STATUS ${CMAKE_PREFIX_PATH})

# message(STATUS "Conteúdo do CMAKE_MODULE_PATH")
# message(STATUS ${CMAKE_MODULE_PATH})

# message(STATUS "CATCH FOUND?")
# message(STATUS ${PACKAGE_FIND_NAME})


# message(STATUS $CATCH_FOUND)

add_subdirectory(src)
add_subdirectory(tests)
