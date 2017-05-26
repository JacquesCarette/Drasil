#include "unity.h"
#include "unity_fixture.h"
#include <stdlib.h>
#include "PCM_Error.h"
#include "parameters.h"
#include "load_params.h"

TEST_GROUP(CompareFortran);

TEST_SETUP(CompareFortran){
}

TEST_TEAR_DOWN(CompareFortran){
}

TEST(CompareFortran, testCompareFortran1){
    struct parameters params;
    params = load_params("C01.in");
    double errTw, errTp, errEw, errEp;
    double delta =  0.002;
    errTw = PCM_ErrorF("Testing/src/compareFortran/F01.out", "C01.out", "TWat", params);
    errTp = PCM_ErrorF("Testing/src/compareFortran/F01.out", "C01.out", "TPCM", params);
    errEw = PCM_ErrorF("Testing/src/compareFortran/F01.out", "C01.out", "EWat", params);
    errEp = PCM_ErrorF("Testing/src/compareFortran/F01.out", "C01.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareFortran, testCompareFortran2){
    struct parameters params;
    params = load_params("C02.in");
    double errTw, errTp, errEw, errEp;
    double delta =  0.002;
    errTw = PCM_ErrorF("Testing/src/compareFortran/F02.out", "C02.out", "TWat", params);
    errTp = PCM_ErrorF("Testing/src/compareFortran/F02.out", "C02.out", "TPCM", params);
    errEw = PCM_ErrorF("Testing/src/compareFortran/F02.out", "C02.out", "EWat", params);
    errEp = PCM_ErrorF("Testing/src/compareFortran/F02.out", "C02.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareFortran, testCompareFortran3){
    struct parameters params;
    params = load_params("C03.in");
    double errTw, errTp, errEw, errEp;
    double delta =  0.002;
    errTw = PCM_ErrorF("Testing/src/compareFortran/F03.out", "C03.out", "TWat", params);
    errTp = PCM_ErrorF("Testing/src/compareFortran/F03.out", "C03.out", "TPCM", params);
    errEw = PCM_ErrorF("Testing/src/compareFortran/F03.out", "C03.out", "EWat", params);
    errEp = PCM_ErrorF("Testing/src/compareFortran/F03.out", "C03.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareFortran, testCompareFortran4){
    struct parameters params;
    params = load_params("C04.in");
    double errTw, errTp, errEw, errEp;
    double delta =  0.002;
    errTw = PCM_ErrorF("Testing/src/compareFortran/F04.out", "C04.out", "TWat", params);
    errTp = PCM_ErrorF("Testing/src/compareFortran/F04.out", "C04.out", "TPCM", params);
    errEw = PCM_ErrorF("Testing/src/compareFortran/F04.out", "C04.out", "EWat", params);
    errEp = PCM_ErrorF("Testing/src/compareFortran/F04.out", "C04.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareFortran, testCompareFortran5){
    struct parameters params;
    params = load_params("C05.in");
    double errTw, errEw;
    double delta =  0.002;
    errTw = PCM_ErrorF("Testing/src/compareFortran/F01.out", "C05.out", "TWatNoP", params);
    errEw = PCM_ErrorF("Testing/src/compareFortran/F01.out", "C05.out", "EWatNoP", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
}
