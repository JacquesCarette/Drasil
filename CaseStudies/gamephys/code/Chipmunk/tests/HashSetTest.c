#define UNITY_INCLUDE_CONFIG_H
#define UNIT_TEST

#include "Chipmunk.h"
#include "unity.h"
#include "string.h"


static int primes[] = {
	5,
	13,
	23,
	47,
	97,
	193,
	389,
	769,
	1543,
	3079,
	6151,
	12289,
	24593,
	49157,
	98317,
	196613,
	393241,
	786433,
	1572869,
	3145739,
	6291469,
	12582917,
	25165843,
	50331653,
	100663319,
	201326611,
	402653189,
	805306457,
	1610612741,
	0,
};

// test default size 
void test_default_size() {
  HashSet * test_set = hashSetNew(0, (HashSetEqlFunc) strcmp);
  TEST_ASSERT_TRUE(test_set->size==primes[0]);
}


// test next size after default
void test_default_nextSize() {
  HashSet * test_set = hashSetNew(6, (HashSetEqlFunc) strcmp);
  TEST_ASSERT_TRUE(test_set->size==primes[1]);
}


// test last valid size
void test_default_lastSize() {
  HashSet * test_set = hashSetNew(805306458, (HashSetEqlFunc) strcmp);
  TEST_ASSERT_TRUE(test_set->size==primes[28]);
}

// test failure after overflow
void test_default_overflow() { // need way to test exceptions
  TEST_FAIL();
  HashSet * test_set = hashSetNew(1610612742, (HashSetEqlFunc) strcmp);
  TEST_ASSERT_TRUE(test_set->size==primes[29]);
}

// test count of empty hashset
void test_count_empty() {
  HashSet * test_set = hashSetNew(0, (HashSetEqlFunc) strcmp);
  TEST_ASSERT_TRUE(hashSetCount(test_set)==0);
}

// test count with a single item
void test_count_single() {
  HashSet * test_set = hashSetNew(0, (HashSetEqlFunc) strcmp);
  //hashSetInsert(test_set, )
  TEST_ASSERT_TRUE(hashSetCount(test_set)==1);
}

// test setting default value
void test_default_value() {
  HashSet * test_set = hashSetNew(0, (HashSetEqlFunc) strcmp);
  char greeting[5] = {'t', 'e', 's', 't', '\0'};
  hashSetSetDefaultValue(test_set, greeting);
  TEST_ASSERT_TRUE(strcmp(greeting, test_set->defaultVal)==0);
}

int main() {
    UNITY_BEGIN();
    RUN_TEST(test_default_value);
    RUN_TEST(test_default_size);
    RUN_TEST(test_default_nextSize);
    RUN_TEST(test_default_lastSize);
    RUN_TEST(test_count_empty);
    RUN_TEST(test_default_overflow);
    return UNITY_END();
}