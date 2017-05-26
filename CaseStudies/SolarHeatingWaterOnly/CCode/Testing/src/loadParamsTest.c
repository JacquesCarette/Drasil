#include "unity.h"
#include "unity_fixture.h"
#include "load_params.h"
#include "parameters.h"

TEST_GROUP(LoadParams);

TEST_SETUP(LoadParams){
}

TEST_TEAR_DOWN(LoadParams){
}

TEST(LoadParams, testParamsL){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(1.5, params.L, "L");
}

TEST(LoadParams, testParamsDiam){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(0.412, params.diam, "diam");
}

TEST(LoadParams, testParamsVp){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(0.05, params.Vp, "Vp");
}

TEST(LoadParams, testParamsAp){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(1.2, params.Ap, "Ap");
}

TEST(LoadParams, testParamsRhoP){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(1007, params.rho_p, "rho_p");
}

TEST(LoadParams, testParamsTmelt){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(44.2, params.Tmelt, "Tmelt");
}

TEST(LoadParams, testParamsCps){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(1760, params.C_ps, "C_ps");
}

TEST(LoadParams, testParamsCpl){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(2270, params.C_pl, "C_pl");
}

TEST(LoadParams, testParamsHf){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(211600, params.Hf, "Hf");
}

TEST(LoadParams, testParamsAc){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(0.12, params.Ac, "Ac");
}

TEST(LoadParams, testParamsTc){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(50.0, params.Tc, "Tc");
}

TEST(LoadParams, testParamsRhoW){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(1000.0, params.rho_w, "rho_w");
}

TEST(LoadParams, testParamsCw){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(4186.0, params.C_w, "C_w");
}

TEST(LoadParams, testParamshc){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(1000.0, params.hc, "hc");
}

TEST(LoadParams, testParamshp){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(1000.0, params.hp, "hp");
}

TEST(LoadParams, testParamsTinit){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(40.0, params.Tinit, "Tinit");
}

TEST(LoadParams, testParamststep){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(10.0, params.tstep, "tstep");
}

TEST(LoadParams, testParamstfinal){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(50000, params.tfinal, "tfinal");
}

TEST(LoadParams, testParamsAbsTol){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(0.0000000001, params.AbsTol, "AbsTol");
}

TEST(LoadParams, testParamsRelTol){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(0.0000000001, params.RelTol, "RelTol");
}

TEST(LoadParams, testParamsConsTol){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(0.001, params.ConsTol, "ConsTol");
}

TEST(LoadParams, testParamsVt){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(0.19997493877160469880110110191927, params.Vt, "Vt");
}

TEST(LoadParams, testParamsMw){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(149.97493877160469880110110191927, params.Mw, "Mw");
}

TEST(LoadParams, testParamsTauW){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(5231.6257808161439098450767719507, params.tau_w, "tau_w");
}

TEST(LoadParams, testParamsEta){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(10.0, params.eta, "eta");
}

TEST(LoadParams, testParamsMp){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(50.35, params.Mp, "Mp");
}

TEST(LoadParams, testParamsTauPS){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(73.846666666666666666666666666667, params.tau_ps, "tau_ps");
}

TEST(LoadParams, testParamsTauPL){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(95.245416666666666666666666666667, params.tau_pl, "tau_pl");
}

TEST(LoadParams, testParamsEpMeltInit){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(372187.2, params.Epmelt_init, "Epmelt_init");
}

TEST(LoadParams, testParamsEpMelt3){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(10654060.0, params.Ep_melt3, "Ep_melt3");
}

TEST(LoadParams, testParamsMwNoPCM){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(199.97493877160469880110110191927, params.Mw_noPCM, "Mw_noPCM");
}

TEST(LoadParams, testParamsTauWNoPCM){
    struct parameters params;
    params = load_params("test.in");
    TEST_ASSERT_EQUAL_FLOAT_MESSAGE(6975.7924474828105765117434386173, params.tau_w_noPCM, "tau_w_noPCM");
}
