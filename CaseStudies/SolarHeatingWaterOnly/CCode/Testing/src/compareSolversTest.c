#include "unity.h"
#include "unity_fixture.h"
#include "PCM_Error.h"
#include <stdlib.h>


TEST_GROUP(CompareSolvers);

TEST_SETUP(CompareSolvers){
}

TEST_TEAR_DOWN(CompareSolvers){
}

TEST(CompareSolvers, testCompareSolvers1){
    double errTw, errTp, errEw, errEp;
    double delta = 0.0001;
    errTw = PCM_ErrorC("C01.out", "Testing/src/compareSolvers/ark01.out", "TWat");
    errTp = PCM_ErrorC("C01.out", "Testing/src/compareSolvers/ark01.out", "TPCM");
    errEw = PCM_ErrorC("C01.out", "Testing/src/compareSolvers/ark01.out", "EWat");
    errEp = PCM_ErrorC("C01.out", "Testing/src/compareSolvers/ark01.out", "EPCM");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareSolvers, testCompareSolvers2){
    double errTw, errTp, errEw, errEp;
    double delta = 0.0001;
    errTw = PCM_ErrorC("C02.out", "Testing/src/compareSolvers/ark02.out", "TWat");
    errTp = PCM_ErrorC("C02.out", "Testing/src/compareSolvers/ark02.out", "TPCM");
    errEw = PCM_ErrorC("C02.out", "Testing/src/compareSolvers/ark02.out", "EWat");
    errEp = PCM_ErrorC("C02.out", "Testing/src/compareSolvers/ark02.out", "EPCM");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}


TEST(CompareSolvers, testCompareSolvers3){
    double errTw, errTp, errEw, errEp;
    double delta = 0.0001;
    errTw = PCM_ErrorC("C03.out", "Testing/src/compareSolvers/ark03.out", "TWat");
    errTp = PCM_ErrorC("C03.out", "Testing/src/compareSolvers/ark03.out", "TPCM");
    errEw = PCM_ErrorC("C03.out", "Testing/src/compareSolvers/ark03.out", "EWat");
    errEp = PCM_ErrorC("C03.out", "Testing/src/compareSolvers/ark03.out", "EPCM");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}


TEST(CompareSolvers, testCompareSolvers4){
    double errTw, errTp, errEw, errEp;
    double delta = 0.0001;
    errTw = PCM_ErrorC("C04.out", "Testing/src/compareSolvers/ark04.out", "TWat");
    errTp = PCM_ErrorC("C04.out", "Testing/src/compareSolvers/ark04.out", "TPCM");
    errEw = PCM_ErrorC("C04.out", "Testing/src/compareSolvers/ark04.out", "EWat");
    errEp = PCM_ErrorC("C04.out", "Testing/src/compareSolvers/ark04.out", "EPCM");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}
