#define UNITY_INCLUDE_CONFIG_H
#define UNIT_TEST

#include "Chipmunk.h"
#include "unity.h"

// Equality test for Vectors.
static void TEST_ASSERT_EQUAL_VECTOR(Vector expected, Vector actual) {
    TEST_ASSERT_EQUAL_FLOAT(expected.x, actual.x);
    TEST_ASSERT_EQUAL_FLOAT(expected.y, actual.y);
}

// Equality test for BB.
static void TEST_ASSERT_EQUAL_BB(BB expected, BB actual) {
    TEST_ASSERT_EQUAL_FLOAT(expected.left, actual.left);
    TEST_ASSERT_EQUAL_FLOAT(expected.bottom, actual.bottom);
    TEST_ASSERT_EQUAL_FLOAT(expected.right, actual.right);
    TEST_ASSERT_EQUAL_FLOAT(expected.top, actual.top);
}

// Tests:

void test_BBNew() {
    BB testBB1 = BBNew(1.0, 4.0, 3.0, 2.0);
    BB testBB2 = BBNew(21.45532, 31.56463, 82.22455, 31.45312);

    TEST_ASSERT_EQUAL_FLOAT(1.0, testBB1.left);
    TEST_ASSERT_EQUAL_FLOAT(4.0, testBB1.bottom);
    TEST_ASSERT_EQUAL_FLOAT(3.0, testBB1.right);
    TEST_ASSERT_EQUAL_FLOAT(2.0, testBB1.top);

    TEST_ASSERT_EQUAL_FLOAT(21.45532, testBB2.left);
    TEST_ASSERT_EQUAL_FLOAT(31.56463, testBB2.bottom);
    TEST_ASSERT_EQUAL_FLOAT(82.22455, testBB2.right);
    TEST_ASSERT_EQUAL_FLOAT(31.45312, testBB2.top);
}

void test_BBNewForExtents() {
    BB testBB1 = BBNewForExtents(zeroVect, 5.0, 10.0);
    BB testBB2 = BBNewForExtents(vect(-42.12455, 35.12464), 19.953, 21.536);
    BB testBB3 = BBNewForExtents(vect(9.24, 1.53), 0, 0);
    BB testBB4 = BBNewForExtents(vect(26.12, 35.33), -12.4, 2.4);

    BB expectedBB1 = {-5.0, -10.0, 5.0, 10.0};
    BB expectedBB2 = {-62.07755, 13.58863, -22.17155, 56.66064};
    BB expectedBB3 = {9.24, 1.53, 9.24, 1.53};

    TEST_ASSERT_EQUAL_BB(expectedBB1, testBB1);
    TEST_ASSERT_EQUAL_BB(expectedBB2, testBB2);
    TEST_ASSERT_EQUAL_BB(expectedBB3, testBB3);
    TEST_ASSERT_EQUAL_BB(BB_ERR, testBB4);
    TEST_ASSERT_EQUAL_BB(BB_ERR, testBB4);
}

void test_BBNewForCircle() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(vect(312.24159, -235.12412), 19.25351);
    BB testBB3 = BBNewForCircle(vect(5.0, 9.0), 0.0);
    BB testBB4 = BBNewForCircle(vect(13.0, 8.0), -15.0);

    BB expectedBB1 = {-25.0, -25.0, 25.0, 25.0};
    BB expectedBB2 = {292.98808, -254.37763, 331.49510, -215.87061};
    BB expectedBB3 = {5.0, 9.0, 5.0, 9.0};

    TEST_ASSERT_EQUAL_BB(expectedBB1, testBB1);
    TEST_ASSERT_EQUAL_BB(expectedBB2, testBB2);
    TEST_ASSERT_EQUAL_BB(expectedBB3, testBB3);
    TEST_ASSERT_EQUAL_BB(BB_ERR, testBB4);
}

void test_BBIntersects() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(vect(20.0, 20.0), 25.0);
    BB testBB3 = BBNewForCircle(vect(51.4, 61.2), 17.4);
    BB testBB4 = BBNewForCircle(vect(-125.31234, 99.24546), 35.92412);
    BB testBB5 = BBNewForCircle(vect(-200.53124, 115.23512), 100.00000);

    TEST_ASSERT_TRUE(BBIntersects(testBB1, testBB2));
    TEST_ASSERT_TRUE(BBIntersects(testBB2, testBB3));
    TEST_ASSERT_TRUE(BBIntersects(testBB5, testBB4));
    TEST_ASSERT_FALSE(BBIntersects(testBB1, testBB3));
    TEST_ASSERT_FALSE(BBIntersects(testBB2, testBB4));
}

void test_BBContainsBB() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(zeroVect, 20.0);
    BB testBB3 = BBNewForCircle(vect(51.4, 61.2), 117.4);
    BB testBB4 = BBNewForCircle(vect(-125.31234, 99.24546), 35.92412);
    BB testBB5 = BBNewForCircle(vect(-200.53124, 115.23512), 151.23523);

    TEST_ASSERT_TRUE(BBContainsBB(testBB1, testBB2));
    TEST_ASSERT_FALSE(BBContainsBB(testBB2, testBB1));
    TEST_ASSERT_TRUE(BBContainsBB(testBB3, testBB1));
    TEST_ASSERT_FALSE(BBContainsBB(testBB3, testBB4));
    TEST_ASSERT_TRUE(BBContainsBB(testBB5, testBB4));
}

void test_BBContainsVect() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(vect(51.4, 61.2), 117.4);
    BB testBB3 = BBNewForCircle(vect(-125.31234, 99.24546), 35.92412);

    TEST_ASSERT_TRUE(BBContainsVect(testBB1, zeroVect));
    TEST_ASSERT_TRUE(BBContainsVect(testBB1, vect(25.0, 25.0)));
    TEST_ASSERT_FALSE(BBContainsVect(testBB1, vect(25.1, 25.0)));
    TEST_ASSERT_TRUE(BBContainsVect(testBB2, vect(-66.0, -56.0)));
    TEST_ASSERT_TRUE(BBContainsVect(testBB3, vect(-100.23515, 132.35166)));
    TEST_ASSERT_FALSE(BBContainsVect(testBB3, vect(-195.35999, 151.64625)));
}

void test_BBMerge() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(zeroVect, 20.0);
    BB testBB3 = BBNewForCircle(vect(51.4, 61.2), 117.4);
    BB testBB4 = BBNewForCircle(vect(-125.31234, 99.24546), 35.92412);
    BB testBB5 = BBNewForCircle(vect(-200.53124, 115.23512), 151.23523);

    BB testBBMerged1 = {-25.0, -25.0, 25.0, 25.0};
    BB testBBMerged2 = {-66.0, -56.2, 168.8, 178.6};
    BB testBBMerged3 = {-161.23646, -56.2, 168.8, 178.6};
    BB testBBMerged4 = {-351.76647, -36.00011, -49.29601, 266.47035};
    BB testBBMerged5 = {-351.76647, -56.2, 168.8, 266.47035};

    TEST_ASSERT_EQUAL_BB(testBBMerged1, BBMerge(testBB1, testBB2));
    TEST_ASSERT_EQUAL_BB(testBBMerged2, BBMerge(testBB1, testBB3));
    TEST_ASSERT_EQUAL_BB(testBBMerged3, BBMerge(testBB3, testBB4));
    TEST_ASSERT_EQUAL_BB(testBBMerged4, BBMerge(testBB4, testBB5));
    TEST_ASSERT_EQUAL_BB(testBBMerged5, BBMerge(testBB3, testBB5));
}

void test_BBCenter() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(vect(5.0, 9.0), 20.0);
    BB testBB3 = BBNewForCircle(vect(51.4, 61.2), 117.4);
    BB testBB4 = BBNewForCircle(vect(-125.31234, 99.24546), 35.92412);
    BB testBB5 = BBNewForCircle(vect(-200.53124, 115.23512), 151.23523);

    TEST_ASSERT_EQUAL_VECTOR(zeroVect, BBCenter(testBB1));
    TEST_ASSERT_EQUAL_VECTOR(vect(5.0, 9.0), BBCenter(testBB2));
    TEST_ASSERT_EQUAL_VECTOR(vect(51.4, 61.2), BBCenter(testBB3));
    TEST_ASSERT_EQUAL_VECTOR(vect(-125.31234, 99.24546), BBCenter(testBB4));
    TEST_ASSERT_EQUAL_VECTOR(vect(-200.53124, 115.23512), BBCenter(testBB5));
}

void test_BBArea() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(vect(5.0, 9.0), 20.0);
    BB testBB3 = BBNewForCircle(vect(51.4, 61.2), 117.4);
    BB testBB4 = BBNewForCircle(vect(-125.31234, 99.24546), 35.92412);
    BB testBB5 = BBNewForCircle(vect(-200.53124, 115.23512), 151.23523);

    TEST_ASSERT_EQUAL_FLOAT(50.0*50.0, BBArea(testBB1));
    TEST_ASSERT_EQUAL_FLOAT(40.0*40.0, BBArea(testBB2));
    TEST_ASSERT_EQUAL_FLOAT(234.8*234.8, BBArea(testBB3));
    TEST_ASSERT_EQUAL_FLOAT(71.84824*71.84824, BBArea(testBB4));
    TEST_ASSERT_EQUAL_FLOAT(302.47046*302.47046, BBArea(testBB5));
}

void test_BBMergedArea() {
    BB testBB1 = BBNewForCircle(zeroVect, 25.0);
    BB testBB2 = BBNewForCircle(zeroVect, 20.0);
    BB testBB3 = BBNewForCircle(vect(51.4, 61.2), 117.4);
    BB testBB4 = BBNewForCircle(vect(-125.31234, 99.24546), 35.92412);
    BB testBB5 = BBNewForCircle(vect(-200.53124, 115.23512), 151.23523);

    TEST_ASSERT_EQUAL_FLOAT(50.0*50.0, BBMergedArea(testBB1, testBB2));
    TEST_ASSERT_EQUAL_FLOAT(55131.04, BBMergedArea(testBB1, testBB3));
    TEST_ASSERT_EQUAL_FLOAT(77492.560808, BBMergedArea(testBB3, testBB4));
    TEST_ASSERT_EQUAL_FLOAT(91488.3791726116, BBMergedArea(testBB4, testBB5));
    TEST_ASSERT_EQUAL_FLOAT(167971.3650731645, BBMergedArea(testBB3, testBB5));
}

int main() {
    UNITY_BEGIN();
    RUN_TEST(test_BBNew);
    RUN_TEST(test_BBNewForExtents);
    RUN_TEST(test_BBNewForCircle);
    RUN_TEST(test_BBIntersects);
    RUN_TEST(test_BBContainsBB);
    RUN_TEST(test_BBContainsVect);
    RUN_TEST(test_BBMerge);
    RUN_TEST(test_BBCenter);
    RUN_TEST(test_BBArea);
    RUN_TEST(test_BBMergedArea);
    return UNITY_END();
}
