#include "unity.h"
#include "unity_fixture.h"

TEST_GROUP_RUNNER(VerifyOutput){
    RUN_TEST_CASE(VerifyOutput, testVerifyOutput1);
    RUN_TEST_CASE(VerifyOutput, testVerifyOutput2);
    RUN_TEST_CASE(VerifyOutput, testVerifyOutput3);
    RUN_TEST_CASE(VerifyOutput, testVerifyOutput4);
}
