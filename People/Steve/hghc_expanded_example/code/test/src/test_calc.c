#include "calc.h"
#include "unity.h"


void setUp(void)
{
}

void tearDown(void)
{
}

void test_calc_h_g(void)
{
  TEST_ASSERT_EQUAL(2.0/3.0, calc_h_g(1.0, 1.0, 1.0));
  TEST_ASSERT_EQUAL(1.0, calc_h_g(2.0, 2.0, 2.0));
}

void test_calc_h_g_error(void)
{
  TEST_ASSERT_EQUAL(-1.0, calc_h_g(0.0, 0.0, 0.0));
  TEST_ASSERT_EQUAL(1, calc_get_error_code());
}

void test_calc_h_c(void)
{
  TEST_ASSERT_EQUAL(2.0/3.0, calc_h_c(1.0, 1.0, 1.0));
  TEST_ASSERT_EQUAL(1.0, calc_h_c(2.0, 2.0, 2.0));
}

void test_calc_h_c_error(void)
{
  TEST_ASSERT_EQUAL(-1.0, calc_h_c(0.0, 0.0, 0.0));
  TEST_ASSERT_EQUAL(1, calc_get_error_code());
}


int main()
{
  UNITY_BEGIN();
  RUN_TEST(test_calc_h_g);
  RUN_TEST(test_calc_h_g_error);
  RUN_TEST(test_calc_h_c);
  RUN_TEST(test_calc_h_c_error);
  return UNITY_END();
}