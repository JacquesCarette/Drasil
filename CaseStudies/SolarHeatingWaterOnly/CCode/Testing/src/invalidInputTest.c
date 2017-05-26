#include "unity.h"
#include "unity_fixture.h"
#include "verify_params.h"
#include "load_params.h"
#include "parameters.h"

TEST_GROUP(InvalidInput);

TEST_SETUP(InvalidInput){
}

TEST_TEAR_DOWN(InvalidInput){
}

TEST(InvalidInput, testFI01){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI01.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(1, err);
}

TEST(InvalidInput, testFI02){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI02.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(1, err);
}

TEST(InvalidInput, testFI03){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI03.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(2, err);
}

TEST(InvalidInput, testFI04){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI04.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(2, err);
}

TEST(InvalidInput, testFI05){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI05.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(3, err);
}

TEST(InvalidInput, testFI06){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI06.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(3, err);
}

TEST(InvalidInput, testFI07){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI07.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(4, err);
}

TEST(InvalidInput, testFI08){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI08.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(4, err);
}

TEST(InvalidInput, testFI09){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI09.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(5, err);
}

TEST(InvalidInput, testFI10){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI10.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(5, err);
}

TEST(InvalidInput, testFI11){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI11.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(6, err);
}

TEST(InvalidInput, testFI12){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI12.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(6, err);
}

TEST(InvalidInput, testFI13){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI13.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(7, err);
}

TEST(InvalidInput, testFI14){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI14.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(7, err);
}

TEST(InvalidInput, testFI15){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI15.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(7, err);
}

TEST(InvalidInput, testFI16){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI16.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(7, err);
}

TEST(InvalidInput, testFI17){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI17.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(10, err);
}

TEST(InvalidInput, testFI18){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI18.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(10, err);
}

TEST(InvalidInput, testFI19){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI19.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(11, err);
}

TEST(InvalidInput, testFI20){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI20.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(11, err);
}

TEST(InvalidInput, testFI21){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI21.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(12, err);
}

TEST(InvalidInput, testFI22){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI22.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(12, err);
}

TEST(InvalidInput, testFI23){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI23.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(13, err);
}

TEST(InvalidInput, testFI24){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI24.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(13, err);
}

TEST(InvalidInput, testFI25){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI25.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(7, err);
}

TEST(InvalidInput, testFI26){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI26.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(7, err);
}

TEST(InvalidInput, testFI27){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI27.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(9, err);
}

TEST(InvalidInput, testFI28){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI28.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(9, err);
}

TEST(InvalidInput, testFI29){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI29.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(14, err);
}

TEST(InvalidInput, testFI30){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI30.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(14, err);
}

TEST(InvalidInput, testFI31){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI31.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(15, err);
}

TEST(InvalidInput, testFI32){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI32.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(15, err);
}

TEST(InvalidInput, testFI33){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI33.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(16, err);
}

TEST(InvalidInput, testFI34){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI34.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(16, err);
}

TEST(InvalidInput, testFI35){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI35.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(17, err);
}

TEST(InvalidInput, testFI36){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI36.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(17, err);
}

TEST(InvalidInput, testFI37){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI37.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(18, err);
}

TEST(InvalidInput, testFI38){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI38.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(18, err);
}

TEST(InvalidInput, testFI39){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI39.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(8, err);
}

TEST(InvalidInput, testFI40){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI40.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(8, err);
}

TEST(InvalidInput, testFI41){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI41.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(20, err);
}

TEST(InvalidInput, testFI42){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI42.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(20, err);
}

TEST(InvalidInput, testFI43){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI43.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(8, err);
}

TEST(InvalidInput, testFI44){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI44.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(8, err);
}

TEST(InvalidInput, testFI45){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI45.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(19, err);
}

TEST(InvalidInput, testFI46){
    struct parameters params;
    params = load_params("Testing/src/FaultyInput/FI46.txt");
    int err;
    err = verify_valid(params);
    TEST_ASSERT_EQUAL_INT(19, err);
}
