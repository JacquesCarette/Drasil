#include "unity.h"
#include "unity_fixture.h"
#include "energy1.h"
#include "energy2.h"
#include "energy3.h"
#include "load_params.h"
#include "parameters.h"

TEST_GROUP(Energy);

TEST_SETUP(Energy){
}

TEST_TEAR_DOWN(Energy){
}

TEST(Energy, testEnergy1W){
    struct parameters params;
    params = load_params("test.in");
    double temp[5] = {40, 41, 42, 43, 44};
    double expE[5] = {0, 627795.09369793726918140921263406, 1255590.1873958745383628184252681, 1883385.2810938118075442276379022, 2511180.3747917490767256368505363};
    int i;
    double ener[5];
    for(i = 0; i < 5; i++){
        ener[i] = energy1Wat(temp[i], params);
    }
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[0], ener[0], "Energy1 Water : ener[0]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[1], ener[1], "Energy1 Water : ener[1]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[2], ener[2], "Energy1 Water : ener[2]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[3], ener[3], "Energy1 Water : ener[3]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[4], ener[4], "Energy1 Water : ener[4]");
}

TEST(Energy, testEnergy1P){
    struct parameters params;
    params = load_params("test.in");
    double temp[5] = {40, 41, 42, 43, 44};
    double expE[5] = {0, 88616.0, 177232.0, 265848.0, 354464.0};
    int i;
    double ener[5];
    for(i = 0; i < 5; i++){
        ener[i] = energy1PCM(temp[i], params);
    }
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[0], ener[0], "Energy1 PCM : ener[0]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[1], ener[1], "Energy1 PCM : ener[1]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[2], ener[2], "Energy1 PCM : ener[2]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[3], ener[3], "Energy1 PCM : ener[3]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[4], ener[4], "Energy1 PCM : ener[4]");
}

TEST(Energy, testEnergy2W){
    struct parameters params;
    params = load_params("test.in");
    double temp[5] = {44.2, 44.3, 44.4, 44.5, 44.6};
    double expE[5] = {2636739.3935313365305619186930631, 2699518.9029011302574800596143265, 2762298.4122709239843982005355899, 2825077.9216407177113163414568533, 2887857.4310105114382344823781167};
    int i;
    double ener[5];
    for(i = 0; i < 5; i++){
        ener[i] = energy2Wat(temp[i], params);
    }
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[0], ener[0], "Energy2 Water : ener[0]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[1], ener[1], "Energy2 Water : ener[1]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[2], ener[2], "Energy2 Water : ener[2]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[3], ener[3], "Energy2 Water : ener[3]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[4], ener[4], "Energy2 Water : ener[4]");
}

TEST(Energy, testEnergy2P){
    struct parameters params;
    params = load_params("test.in");
    double latentHeat[5] = {372000, 423000, 474000, 525000, 576000};
    double expE[5] = {744187.2, 795187.2, 846187.2, 897187.2, 948187.2};
    int i;
    double ener[5];
    for(i = 0; i < 5; i++){
        ener[i] = energy2PCM(latentHeat[i], params);
    }
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[0], ener[0], "Energy2 PCM : ener[0]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[1], ener[1], "Energy2 PCM : ener[1]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[2], ener[2], "Energy2 PCM : ener[2]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[3], ener[3], "Energy2 PCM : ener[3]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[4], ener[4], "Energy2 PCM : ener[4]");
}

TEST(Energy, testEnergy3W){
    struct parameters params;
    params = load_params("test.in");
    double temp[5] = {45, 46, 47, 48, 49};
    double expE[5] = {3138975.4684896863459070460631703, 3766770.5621876236150884552758044, 4394565.6558855608842698644884384, 5022360.7495834981534512737010725, 5650155.8432814354226326829137066};
    int i;
    double ener[5];
    for(i = 0; i < 5; i++){
        ener[i] = energy3Wat(temp[i], params);
    }
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[0], ener[0], "Energy3 Water : ener[0]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[1], ener[1], "Energy3 Water : ener[1]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[2], ener[2], "Energy3 Water : ener[2]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[3], ener[3], "Energy3 Water : ener[3]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[4], ener[4], "Energy3 Water : ener[4]");
}

TEST(Energy, testEnergy3P){
    struct parameters params;
    params = load_params("test.in");
    double temp[5] = {45, 46, 47, 48, 49};
    double expE[5] = {11117682.8, 11231977.3, 11346271.8, 11460566.3, 11574860.8};
    int i;
    double ener[5];
    for(i = 0; i < 5; i++){
        ener[i] = energy3PCM(temp[i], params);
    }
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[0], ener[0], "Energy3 PCM : ener[0]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[1], ener[1], "Energy3 PCM : ener[1]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[2], ener[2], "Energy3 PCM : ener[2]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[3], ener[3], "Energy3 PCM : ener[3]");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(expE[4], ener[4], "Energy3 PCM : ener[4]");
}
