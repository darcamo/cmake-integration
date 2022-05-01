
#include <catch2/catch.hpp>
#include "functions.h"

TEST_CASE("Test the functions", "[functions]") {
    double d = 3.1415;
    REQUIRE(doubleInput(d) == 2 * d);
}
