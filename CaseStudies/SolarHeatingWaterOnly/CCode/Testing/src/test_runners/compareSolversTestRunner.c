#include "unity.h"
#include "unity_fixture.h"

TEST_GROUP_RUNNER(CompareSolvers){
    RUN_TEST_CASE(CompareSolvers, testCompareSolvers1);
    RUN_TEST_CASE(CompareSolvers, testCompareSolvers2);
    RUN_TEST_CASE(CompareSolvers, testCompareSolvers3);
    RUN_TEST_CASE(CompareSolvers, testCompareSolvers4);
}
