#include "unity.h"
#include "unity_fixture.h"
#include "PCM_Error.h"
#include "parameters.h"
#include "load_params.h"

TEST_GROUP(CompareMatlab);

TEST_SETUP(CompareMatlab){
}

TEST_TEAR_DOWN(CompareMatlab){
}

TEST(CompareMatlab, testCompareMatlab1){
    struct parameters params;
    params = load_params("Testing/src/inputFiles/C01.in");
    double errTw, errTp, errEw, errEp;
    double delta = 0.00001;
    errTw = PCM_ErrorM("Testing/src/compareMatlab/M01.out", "C01.out", "TWat", params);
    errTp = PCM_ErrorM("Testing/src/compareMatlab/M01.out", "C01.out", "TPCM", params);
    errEw = PCM_ErrorM("Testing/src/compareMatlab/M01.out", "C01.out", "EWat", params);
    errEp = PCM_ErrorM("Testing/src/compareMatlab/M01.out", "C01.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareMatlab, testCompareMatlab2){
    struct parameters params;
    params = load_params("Testing/src/inputFiles/C02.in");
    double errTw, errTp, errEw, errEp;
    double delta = 0.00001;
    errTw = PCM_ErrorM("Testing/src/compareMatlab/M02.out", "C02.out", "TWat", params);
    errTp = PCM_ErrorM("Testing/src/compareMatlab/M02.out", "C02.out", "TPCM", params);
    errEw = PCM_ErrorM("Testing/src/compareMatlab/M02.out", "C02.out", "EWat", params);
    errEp = PCM_ErrorM("Testing/src/compareMatlab/M02.out", "C02.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareMatlab, testCompareMatlab3){
    struct parameters params;
    params = load_params("Testing/src/inputFiles/C03.in");
    double errTw, errTp, errEw, errEp;
    double delta = 0.00001;
    errTw = PCM_ErrorM("Testing/src/compareMatlab/M03.out", "C03.out", "TWat", params);
    errTp = PCM_ErrorM("Testing/src/compareMatlab/M03.out", "C03.out", "TPCM", params);
    errEw = PCM_ErrorM("Testing/src/compareMatlab/M03.out", "C03.out", "EWat", params);
    errEp = PCM_ErrorM("Testing/src/compareMatlab/M03.out", "C03.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}

TEST(CompareMatlab, testCompareMatlab4){
    struct parameters params;
    params = load_params("Testing/src/inputFiles/C04.in");
    double errTw, errTp, errEw, errEp;
    double delta = 0.00001;
    errTw = PCM_ErrorM("Testing/src/compareMatlab/M04.out", "C04.out", "TWat", params);
    errTp = PCM_ErrorM("Testing/src/compareMatlab/M04.out", "C04.out", "TPCM", params);
    errEw = PCM_ErrorM("Testing/src/compareMatlab/M04.out", "C04.out", "EWat", params);
    errEp = PCM_ErrorM("Testing/src/compareMatlab/M04.out", "C04.out", "EPCM", params);
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTw, "Water temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errTp, "PCM temperature");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEw, "Water energy");
    TEST_ASSERT_FLOAT_WITHIN_MESSAGE(delta, 0, errEp, "PCM energy");
}
