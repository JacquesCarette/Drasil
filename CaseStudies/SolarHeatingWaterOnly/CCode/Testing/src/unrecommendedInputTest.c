#include "unity.h"
#include "unity_fixture.h"
#include "verify_params.h"
#include "load_params.h"
#include "parameters.h"

TEST_GROUP(UnrecommendedInput);

TEST_SETUP(UnrecommendedInput){
}

TEST_TEAR_DOWN(UnrecommendedInput){
}

TEST(UnrecommendedInput, testUI01){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI01.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 0.1 <= L <= 50", msg);
}

TEST(UnrecommendedInput, testUI02){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI02.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 0.1 <= L <= 50", msg);
}

TEST(UnrecommendedInput, testUI03){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI03.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 0.002 <= D/L <= 200", msg);
}

TEST(UnrecommendedInput, testUI04){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI04.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 0.002 <= D/L <= 200", msg);
}

TEST(UnrecommendedInput, testUI05){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI05.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that Vp be >= 0.0001% of Vt", msg);
}

TEST(UnrecommendedInput, testUI06){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI06.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that Vp <= Ap <= (2/0.001) * Vp", msg);
}

TEST(UnrecommendedInput, testUI07){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI07.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that Vp <= Ap <= (2/0.001) * Vp", msg);
}

TEST(UnrecommendedInput, testUI08){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI08.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 500 < rho_p < 20000", msg);
}

TEST(UnrecommendedInput, testUI09){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI09.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 500 < rho_p < 20000", msg);
}

TEST(UnrecommendedInput, testUI10){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI10.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 100 < C_ps < 4000", msg);
}

TEST(UnrecommendedInput, testUI11){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI11.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 100 < C_ps < 4000", msg);
}

TEST(UnrecommendedInput, testUI12){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI12.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 100 < C_pl < 5000", msg);
}

TEST(UnrecommendedInput, testUI13){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI13.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 100 < C_pl < 5000", msg);
}

TEST(UnrecommendedInput, testUI16){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI16.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that Ac <= pi * (D/2) ^ 2", msg);
}

TEST(UnrecommendedInput, testUI17){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI17.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 950 < rho_w <= 1000", msg);
}

TEST(UnrecommendedInput, testUI18){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI18.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 950 < rho_w <= 1000", msg);
}


TEST(UnrecommendedInput, testUI19){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI19.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 4170 < C_w < 4210", msg);
}

TEST(UnrecommendedInput, testUI20){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI20.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 4170 < C_w < 4210", msg);
}


TEST(UnrecommendedInput, testUI21){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI21.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 10 < hc < 10000", msg);
}

TEST(UnrecommendedInput, testUI22){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI22.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 10 < hc < 10000", msg);
}


TEST(UnrecommendedInput, testUI23){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI23.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 10 < hp < 10000", msg);
}

TEST(UnrecommendedInput, testUI24){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI24.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 10 < hp < 10000", msg);
}

TEST(UnrecommendedInput, testUI25){
    struct parameters params;
    params = load_params("Testing/src/unrecommendedInput/UI25.txt");
    const char * msg;
    msg = verify_recommended(params);
    TEST_ASSERT_EQUAL_STRING("Warning: It is recommended that 0 < tfinal < 86400", msg);
}
