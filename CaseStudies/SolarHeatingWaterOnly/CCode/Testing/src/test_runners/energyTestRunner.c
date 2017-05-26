#include "unity.h"
#include "unity_fixture.h"

TEST_GROUP_RUNNER(Energy){
    RUN_TEST_CASE(Energy, testEnergy1W);
    RUN_TEST_CASE(Energy, testEnergy1P);
    RUN_TEST_CASE(Energy, testEnergy2W);
    RUN_TEST_CASE(Energy, testEnergy2P);
    RUN_TEST_CASE(Energy, testEnergy3W);
    RUN_TEST_CASE(Energy, testEnergy3P);
}
