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

/**
 * Test that start index is 0, regardless of initial size
 */
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

/**
 * Test object pointer is being pushed to the array
 */
void test_push() {
  Array * test_array = arrayNew(0);
  int test_val = 196613;
  arrayPush(test_array, &test_val);
  TEST_ASSERT_TRUE(&test_val==test_array->arr[0]);
}

/**
 * Test multiple object pointers being pushed to the array
 */
void test_push_n() {
  Array * test_array = arrayNew(4);
  int test_val = 196613;
  arrayPush(test_array, &test_val);
  arrayPush(test_array, &test_val);
  arrayPush(test_array, &test_val);
  arrayPush(test_array, &test_val);
  TEST_ASSERT_TRUE(&test_val==test_array->arr[0]);
  TEST_ASSERT_TRUE(&test_val==test_array->arr[1]);
  TEST_ASSERT_TRUE(&test_val==test_array->arr[2]);
  TEST_ASSERT_TRUE(&test_val==test_array->arr[3]);
}

/**
 * test push of different pointers to array
 */
void test_push_n_diff() {
  Array * test_array = arrayNew(4);
  int test_val1 = 196613;
  char test_val2 = 'a';
  arrayPush(test_array, &test_val1);
  arrayPush(test_array, &test_val2);
  arrayPush(test_array, &test_val2);
  arrayPush(test_array, &test_val1);
  TEST_ASSERT_TRUE(&test_val1==test_array->arr[0]);
  TEST_ASSERT_TRUE(&test_val2==test_array->arr[1]);
  TEST_ASSERT_TRUE(&test_val2==test_array->arr[2]);
  TEST_ASSERT_TRUE(&test_val1==test_array->arr[3]);
  TEST_ASSERT_TRUE(4==test_array->num); // size check
}

/**
 * test size of array is 4 after pushing 4 object ptr
 */
void test_push_size_check() {
  Array * test_array = arrayNew(4);
  int test_val1 = 196613;
  char test_val2 = 'a';
  arrayPush(test_array, &test_val1);
  arrayPush(test_array, &test_val2);
  arrayPush(test_array, &test_val2);
  arrayPush(test_array, &test_val1);
  TEST_ASSERT_TRUE(4==test_array->num); // size check
}

/**
 * Pop empty array should throw exception
 * this test will fail, can't catch an abort?
 */
void test_pop() {
  TEST_FAIL();
  Array * test_array = arrayNew(0);
  arrayPop(test_array);
}

/**
 * push pop should result in push ptr not being in first location for array
 */
void test_push_pop() {
  Array * test_array = arrayNew(0);
  int test_val = 196613;
  arrayPush(test_array, &test_val);
  arrayPop(test_array);
  TEST_ASSERT_TRUE(&test_val!=test_array->arr[0]);
}

/**
 * push pop multiple times result should be the same as push pop a single time
 */
void test_push_pop_n() {
  Array * test_array = arrayNew(0);
  int test_val = 196613;
  arrayPush(test_array, &test_val);
  arrayPush(test_array, &test_val);
  arrayPush(test_array, &test_val);
  arrayPush(test_array, &test_val);
  arrayPop(test_array);
  arrayPop(test_array);
  arrayPop(test_array);
  arrayPop(test_array);
  TEST_ASSERT_TRUE(&test_val!=test_array->arr[0]);
  TEST_ASSERT_TRUE(&test_val!=test_array->arr[1]);
  TEST_ASSERT_TRUE(&test_val!=test_array->arr[2]);
  TEST_ASSERT_TRUE(&test_val!=test_array->arr[3]);
}


/**
 * test contains with single ptr in array
 */
void test_containsObj() {
  Array * test_array = arrayNew(0);
  int test_val = 196613;
  arrayPush(test_array, &test_val);
  TEST_ASSERT_TRUE(arrayContains(test_array, &test_val));
}

/**
 * test contains with multiple different ptr
 */
void test_containsObj_n() {
  Array * test_array = arrayNew(0);
  int test_val = 196613;
  int test_val1 = 196614;
  int test_val2 = 196615;
  int test_val3 = 196616;
  arrayPush(test_array, &test_val);
  arrayPush(test_array, &test_val1);
  arrayPush(test_array, &test_val2);
  arrayPush(test_array, &test_val3);
  TEST_ASSERT_TRUE(arrayContains(test_array, &test_val2));
}

/**
 * test deleteObject with only object in array
 */
void test_deleteObject() {
  Array * test_array = arrayNew(0);
  int test_val = 196613;
  arrayPush(test_array, &test_val);
  arrayDeleteObj(test_array, &test_val);
  TEST_ASSERT_TRUE(&test_val!=test_array->arr[0]);
}

/**
 * test deleteObject with ptr not in array
 */
void test_not_deleteObject() {
  Array * test_array = arrayNew(0);
  int test_val = 196613;
  int test_val1 = 196614;
  arrayPush(test_array, &test_val);
  arrayDeleteObj(test_array, &test_val1);
  TEST_ASSERT_TRUE(arrayContains(test_array, &test_val));
}


/**
 * test deleteObject with array of multiple ptr
 */
void test_deleteObject_n() {
  Array * test_array = arrayNew(0);
  int test_val0 = 196613;
  int test_val1 = 196614;
  int test_val2 = 196615;
  int test_val3 = 196616;
  arrayPush(test_array, &test_val0);
  arrayPush(test_array, &test_val1);
  arrayPush(test_array, &test_val2);
  arrayPush(test_array, &test_val3);
  arrayDeleteObj(test_array, &test_val0);
  TEST_ASSERT_TRUE((!arrayContains(test_array, &test_val0)));
}


int main() {
  UNITY_BEGIN();
  RUN_TEST(test_default_size);
  RUN_TEST(test_size_n);
  RUN_TEST(test_start_index);
  RUN_TEST(test_push);
  RUN_TEST(test_push_n);
  RUN_TEST(test_push_n_diff);
  RUN_TEST(test_push_size_check);
  RUN_TEST(test_pop);
  RUN_TEST(test_push_pop);
  RUN_TEST(test_push_pop_n);
  RUN_TEST(test_containsObj);
  RUN_TEST(test_containsObj_n);
  RUN_TEST(test_deleteObject);
  RUN_TEST(test_not_deleteObject);
  RUN_TEST(test_deleteObject_n);
  return UNITY_END();
}
