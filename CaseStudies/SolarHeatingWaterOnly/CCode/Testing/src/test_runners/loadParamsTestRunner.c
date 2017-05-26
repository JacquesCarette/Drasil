#include "unity.h"
#include "unity_fixture.h"

TEST_GROUP_RUNNER(LoadParams){
    RUN_TEST_CASE(LoadParams, testParamsL);
    RUN_TEST_CASE(LoadParams, testParamsDiam);
    RUN_TEST_CASE(LoadParams, testParamsVp);
    RUN_TEST_CASE(LoadParams, testParamsAp);
    RUN_TEST_CASE(LoadParams, testParamsRhoP);
    RUN_TEST_CASE(LoadParams, testParamsTmelt);
    RUN_TEST_CASE(LoadParams, testParamsCps);
    RUN_TEST_CASE(LoadParams, testParamsCpl);
    RUN_TEST_CASE(LoadParams, testParamsHf);
    RUN_TEST_CASE(LoadParams, testParamsAc);
    RUN_TEST_CASE(LoadParams, testParamsTc);
    RUN_TEST_CASE(LoadParams, testParamsRhoW);
    RUN_TEST_CASE(LoadParams, testParamsCw);
    RUN_TEST_CASE(LoadParams, testParamshc);
    RUN_TEST_CASE(LoadParams, testParamshp);
    RUN_TEST_CASE(LoadParams, testParamsTinit);
    RUN_TEST_CASE(LoadParams, testParamststep);
    RUN_TEST_CASE(LoadParams, testParamstfinal);
    RUN_TEST_CASE(LoadParams, testParamsAbsTol);
    RUN_TEST_CASE(LoadParams, testParamsRelTol);
    RUN_TEST_CASE(LoadParams, testParamsConsTol);
    RUN_TEST_CASE(LoadParams, testParamsVt);
    RUN_TEST_CASE(LoadParams, testParamsMw);
    RUN_TEST_CASE(LoadParams, testParamsTauW);
    RUN_TEST_CASE(LoadParams, testParamsEta);
    RUN_TEST_CASE(LoadParams, testParamsMp);
    RUN_TEST_CASE(LoadParams, testParamsTauPS);
    RUN_TEST_CASE(LoadParams, testParamsTauPL);
    RUN_TEST_CASE(LoadParams, testParamsEpMeltInit);
    RUN_TEST_CASE(LoadParams, testParamsEpMelt3);
    RUN_TEST_CASE(LoadParams, testParamsMwNoPCM);
    RUN_TEST_CASE(LoadParams, testParamsTauWNoPCM);
}
