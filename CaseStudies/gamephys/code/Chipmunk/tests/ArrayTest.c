#define UNITY_INCLUDE_CONFIG_H
#define UNIT_TEST

#include "Chipmunk.h"
#include "unity.h"

// UNIT TESTS FOR ARRAY MODULE //

/**
 * Tests for default size condition specified in the MIS
 * if 0 is passed max size by default must be 4
 */
void test_default_size() {
  Array * test_array = arrayNew(0);
  TEST_ASSERT_TRUE(test_array->max==4);
}

/**
 * Tests max size is what specified value should be
 * when not default
 */
void test_size_n() {
  Array * test_array1 = arrayNew(1);
  Array * test_array2 = arrayNew(5);
  Array * test_array3 = arrayNew(13);
  Array * test_array4 = arrayNew(23);
  Array * test_array5 = arrayNew(47);
  Array * test_array6 = arrayNew(97);
  TEST_ASSERT_TRUE(test_array1->max==1);
  TEST_ASSERT_TRUE(test_array2->max==5);
  TEST_ASSERT_TRUE(test_array3->max==13);
  TEST_ASSERT_TRUE(test_array4->max==23);
  TEST_ASSERT_TRUE(test_array5->max==47);
  TEST_ASSERT_TRUE(test_array6->max==97);
}

void test_start_index() {
  Array * test_array1 = arrayNew(0);
  Array * test_array2 = arrayNew(1);
  Array * test_array3 = arrayNew(5);
  Array * test_array4 = arrayNew(97);
  TEST_ASSERT_TRUE(test_array1->num==0);
  TEST_ASSERT_TRUE(test_array2->num==0);
  TEST_ASSERT_TRUE(test_array3->num==0);
  TEST_ASSERT_TRUE(test_array4->num==0);
}

int main() {
  UNITY_BEGIN();
  RUN_TEST(test_default_size);
  RUN_TEST(test_size_n);
  RUN_TEST(test_start_index);
  return UNITY_END();
}
