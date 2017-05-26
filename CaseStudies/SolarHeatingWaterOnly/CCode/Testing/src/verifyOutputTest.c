#include "unity.h"
#include "unity_fixture.h"
#include "verify_output.h"
#include "load_params.h"
#include "parameters.h"

TEST_GROUP(VerifyOutput);

TEST_SETUP(VerifyOutput){
}

TEST_TEAR_DOWN(VerifyOutput){
}

TEST(VerifyOutput, testVerifyOutput1){
    struct parameters params;
    params = load_params("test.in");
    double time[4] = {0, 10, 20, 30};
    double tempW[4] = {40, 42, 44, 46};
    double tempP[4] = {40, 41.9, 43.8, 45.7};
    double eW[4] = {0, 1000, 2000, 19800};
    double eP[4] = {0, 1000, 2000, 5400};
    int sizeOfResults = sizeof(tempW) / sizeof(tempW[0]);
    int warnings = verify_output(time, tempW, tempP, eW, eP, params, sizeOfResults);
    TEST_ASSERT_EQUAL_INT(0, warnings);
}

TEST(VerifyOutput, testVerifyOutput2){
    struct parameters params;
    params = load_params("test.in");
    double time[4] = {0, 10, 20, 30};
    double tempW[4] = {40, 42, 44, 46};
    double tempP[4] = {40, 41.9, 43.8, 45.7};
    double eW[4] = {0, 1000, 2000, 19800};
    double eP[4] = {0, 1000, 2000, 3000};
    int sizeOfResults = sizeof(tempW) / sizeof(tempW[0]);
    int warnings = verify_output(time, tempW, tempP, eW, eP, params, sizeOfResults);
    TEST_ASSERT_EQUAL_INT(2, warnings);
}

TEST(VerifyOutput, testVerifyOutput3){
    struct parameters params;
    params = load_params("test.in");
    double time[4] = {0, 10, 20, 30};
    double tempW[4] = {40, 42, 44, 46};
    double tempP[4] = {40, 41.9, 43.8, 45.7};
    double eW[4] = {0, 1000, 2000, 3000};
    double eP[4] = {0, 1000, 2000, 5400};
    int sizeOfResults = sizeof(tempW) / sizeof(tempW[0]);
    int warnings = verify_output(time, tempW, tempP, eW, eP, params, sizeOfResults);
    TEST_ASSERT_EQUAL_INT(1, warnings);
}

TEST(VerifyOutput, testVerifyOutput4){
    struct parameters params;
    params = load_params("test.in");
    double time[4] = {0, 10, 20, 30};
    double tempW[4] = {40, 42, 44, 46};
    double tempP[4] = {40, 41.9, 43.8, 45.7};
    double eW[4] = {0, 1000, 2000, 3000};
    double eP[4] = {0, 1000, 2000, 3000};
    int sizeOfResults = sizeof(tempW) / sizeof(tempW[0]);
    int warnings = verify_output(time, tempW, tempP, eW, eP, params, sizeOfResults);
    TEST_ASSERT_EQUAL_INT(3, warnings);
}
