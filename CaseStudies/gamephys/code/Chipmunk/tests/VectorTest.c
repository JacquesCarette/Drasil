#define UNITY_INCLUDE_CONFIG_H

#include "Chipmunk.h"
#include "unity.h"

// Tests:

void test_vect() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, zeroVect.x);
    TEST_ASSERT_EQUAL_FLOAT(0.0, zeroVect.y);
    TEST_ASSERT_EQUAL_FLOAT(5.0, vect(5, 9).x);
    TEST_ASSERT_EQUAL_FLOAT(10.0, vect(12, 10).y);
    TEST_ASSERT_EQUAL_FLOAT(5.1245, vect(5.1245, 9.2412).x);
    TEST_ASSERT_EQUAL_FLOAT(124.231, vect(-424.11, 124.231).y);
}

void test_vectEqual() {
    TEST_ASSERT_TRUE(vectEqual(zeroVect, zeroVect));
    TEST_ASSERT_TRUE(vectEqual(vect(5, 10), vect(5, 10)));
    TEST_ASSERT_TRUE(vectEqual(vect(6.24, 2.31), vect(6.24, 2.31)));
    TEST_ASSERT_FALSE(vectEqual(zeroVect, vect(1e-5, 1e-5)));
    TEST_ASSERT_FALSE(vectEqual(vect(5, 10), vect(10, 5)));
    TEST_ASSERT_FALSE(vectEqual(vect(3.1415, 6.2830), vect(3.141529, 6.2830)));
}

void test_vectAdd() {
    TEST_ASSERT_EQUAL_FLOAT(-241.0, vectAdd(vect(-241, 55), zeroVect).x);
    TEST_ASSERT_EQUAL_FLOAT(5192.0, vectAdd(zeroVect, vect(2414, 5192)).y);
    TEST_ASSERT_EQUAL_FLOAT(11.0, vectAdd(vect(5, 12), vect(6, 9)).x);
    TEST_ASSERT_EQUAL_FLOAT(49.0, vectAdd(vect(9, 89), vect(-12, -40)).y);
    TEST_ASSERT_EQUAL_FLOAT(152.1241, vectAdd(vect(131.1041, 121), vect(21.02, 55.92)).x);
    TEST_ASSERT_EQUAL_FLOAT(99.52, vectAdd(vect(91.42, 55.24), vect(21.55, 44.28)).y);
}

void test_vectSub() {
    TEST_ASSERT_EQUAL_FLOAT(-241.0, vectSub(vect(-241, 55), zeroVect).x);
    TEST_ASSERT_EQUAL_FLOAT(-5192.0, vectSub(zeroVect, vect(2414, 5192)).y);
    TEST_ASSERT_EQUAL_FLOAT(-1.0, vectSub(vect(5, 12), vect(6, 9)).x);
    TEST_ASSERT_EQUAL_FLOAT(129.0, vectSub(vect(9, 89), vect(-12, -40)).y);
    TEST_ASSERT_EQUAL_FLOAT(110.0841, vectSub(vect(131.1041, 121), vect(21.02, 55.92)).x);
    TEST_ASSERT_EQUAL_FLOAT(10.96, vectSub(vect(91.42, 55.24), vect(21.55, 44.28)).y);
}

void test_vectMult() {
    TEST_ASSERT_EQUAL_FLOAT(10.0, vectMult(vect(2, 5), 5).x);
    TEST_ASSERT_EQUAL_FLOAT(124.0, vectMult(vect(21, 31), 4).y);
    TEST_ASSERT_EQUAL_FLOAT(-5.0, vectMult(vect(5, 19), -1).x);
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectMult(vect(192, 929), 0).y);
    TEST_ASSERT_EQUAL_FLOAT(13364.8208, vectMult(vect(412.24, 525.94), 32.42).x);
    TEST_ASSERT_EQUAL_FLOAT(-23993.1516, vectMult(vect(924.12, -241.55), 99.33).y);
}

void test_vectNeg() {
    TEST_ASSERT_EQUAL_FLOAT(-2.0, vectNeg(vect(2, 5)).x);
    TEST_ASSERT_EQUAL_FLOAT(-31.0, vectNeg(vect(21, 31)).y);
    TEST_ASSERT_EQUAL_FLOAT(5.0, vectNeg(vect(-5, -19)).x);
    TEST_ASSERT_EQUAL_FLOAT(-929.0, vectNeg(vect(192, 929)).y);
    TEST_ASSERT_EQUAL_FLOAT(-412.24, vectNeg(vect(412.24, 525.94)).x);
    TEST_ASSERT_EQUAL_FLOAT(241.55, vectNeg(vect(924.12, -241.55)).y);
}

void test_vectDot() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectDot(zeroVect, zeroVect));
    TEST_ASSERT_EQUAL_FLOAT(58.0, vectDot(vect(3, 5), vect(11, 5)));
    TEST_ASSERT_EQUAL_FLOAT(-6358.0, vectDot(vect(-55, -9), vect(121, -33)));
    TEST_ASSERT_EQUAL_FLOAT(-5890.0, vectDot(vect(31, -124), vect(50, 60)));
    TEST_ASSERT_EQUAL_FLOAT(-1961.0868, vectDot(vect(42.52, 91.42), vect(23.11, -32.2)));
    TEST_ASSERT_EQUAL_FLOAT(-17645.095551, vectDot(vect(555.124, -192.005), vect(121.421, 442.951)));
}

void test_vectCross() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectCross(zeroVect, zeroVect));
    TEST_ASSERT_EQUAL_FLOAT(-40.0, vectCross(vect(3, 5), vect(11, 5)));
    TEST_ASSERT_EQUAL_FLOAT(2904.0, vectCross(vect(-55, -9), vect(121, -33)));
    TEST_ASSERT_EQUAL_FLOAT(8060.0, vectCross(vect(31, -124), vect(50, 60)));
    TEST_ASSERT_EQUAL_FLOAT(-3481.8602, vectCross(vect(42.52, 91.42), vect(23.11, -32.2)));
    TEST_ASSERT_EQUAL_FLOAT(269206.170029, vectCross(vect(555.124, -192.005), vect(121.421, 442.951)));
}

void test_vectPerp() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectPerp(zeroVect).x);
    TEST_ASSERT_EQUAL_FLOAT(9.0, vectPerp(vect(-55, -9)).x);
    TEST_ASSERT_EQUAL_FLOAT(-55.0, vectPerp(vect(-55, -9)).y);
    TEST_ASSERT_EQUAL_FLOAT(-91.42, vectPerp(vect(42.52, 91.42)).x);
    TEST_ASSERT_EQUAL_FLOAT(192.005, vectPerp(vect(555.124, -192.005)).x);
    TEST_ASSERT_EQUAL_FLOAT(555.124, vectPerp(vect(555.124, -192.005)).y);
}

void test_vectRPerp() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectRPerp(zeroVect).x);
    TEST_ASSERT_EQUAL_FLOAT(-9.0, vectRPerp(vect(-55, -9)).x);
    TEST_ASSERT_EQUAL_FLOAT(55.0, vectRPerp(vect(-55, -9)).y);
    TEST_ASSERT_EQUAL_FLOAT(91.42, vectRPerp(vect(42.52, 91.42)).x);
    TEST_ASSERT_EQUAL_FLOAT(-192.005, vectRPerp(vect(555.124, -192.005)).x);
    TEST_ASSERT_EQUAL_FLOAT(-555.124, vectRPerp(vect(555.124, -192.005)).y);
}

void test_vectProject() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectProject(zeroVect, vect(1, 1)).x);
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectProject(vect(1, 0), vect(0, 1)).y);
    TEST_ASSERT_EQUAL_FLOAT(3.061855670103093, vectProject(vect(5, -3), vect(-9, -4)).x);
    TEST_ASSERT_EQUAL_FLOAT(-10.352941176470589, vectProject(vect(-12, -14), vect(-2, 8)).y);
    TEST_ASSERT_EQUAL_FLOAT(25.20398385033259, vectProject(vect(22.53, -17.14), vect(51.23, -12.42)).x);
    TEST_ASSERT_EQUAL_FLOAT(159.50239525796383, vectProject(vect(523.512, 921.535), vect(-1012.331, -231.425)).y);
}

void test_vectForAngle() {
    TEST_ASSERT_EQUAL_FLOAT(1.0, vectForAngle(0.0).x);
    TEST_ASSERT_EQUAL_FLOAT(-1.0, vectForAngle(M_PI).x);
    TEST_ASSERT_EQUAL_INT(0.0, vectForAngle(M_PI).y);
    TEST_ASSERT_EQUAL_FLOAT(-0.903692205, vectForAngle(35.0).x);
    TEST_ASSERT_EQUAL_FLOAT(-0.599894281, vectForAngle(72.9).y);
    TEST_ASSERT_EQUAL_FLOAT(0.0671930344, vectForAngle(-152.3).x);
}

void test_vectToAngle() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectToAngle(zeroVect));
    TEST_ASSERT_EQUAL_FLOAT(M_PI, vectToAngle(vect(-1.0, 0.0)));
    TEST_ASSERT_EQUAL_FLOAT(M_PI_2, vectToAngle(vect(0.0, 1.0)));
    TEST_ASSERT_EQUAL_FLOAT(0.522650772, vectToAngle(vect(92, 53)));
    TEST_ASSERT_EQUAL_FLOAT(-0.694738276, vectToAngle(vect(30, -25)));
    TEST_ASSERT_EQUAL_FLOAT(2.706682247, vectToAngle(vect(-71.551, 33.241)));
}

void test_vectRotate() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectRotate(zeroVect, zeroVect).x);
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectRotate(zeroVect, zeroVect).y);
    TEST_ASSERT_EQUAL_FLOAT(-7.0, vectRotate(vect(5, -3), vect(-2, 1)).x);
    TEST_ASSERT_EQUAL_FLOAT(11.0, vectRotate(vect(5, -3), vect(-2, 1)).y);
    TEST_ASSERT_EQUAL_FLOAT(-5448.9903, vectRotate(vect(44.21, 24.11), vect(-73.50, 91.23)).x);
    TEST_ASSERT_EQUAL_FLOAT(2261.1833, vectRotate(vect(44.21, 24.11), vect(-73.50, 91.23)).y);

}

void test_vectUnrotate() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectUnrotate(zeroVect, zeroVect).x);
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectUnrotate(zeroVect, zeroVect).y);
    TEST_ASSERT_EQUAL_FLOAT(25.0, vectUnrotate(vect(-7, 11), vect(-2, 1)).x);
    TEST_ASSERT_EQUAL_FLOAT(-15.0, vectUnrotate(vect(-7, 11), vect(-2, 1)).y);
    TEST_ASSERT_EQUAL_FLOAT(-1049.8797, vectUnrotate(vect(44.21, 24.11), vect(-73.50, 91.23)).x);
    TEST_ASSERT_EQUAL_FLOAT(-5805.3633, vectUnrotate(vect(44.21, 24.11), vect(-73.50, 91.23)).y);
}

void test_vectLengthSq() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectLengthSq(zeroVect));
    TEST_ASSERT_EQUAL_FLOAT(25.0, vectLengthSq(vect(3, 4)));
    TEST_ASSERT_EQUAL_FLOAT(106.0, vectLengthSq(vect(-5, 9)));
    TEST_ASSERT_EQUAL_FLOAT(2080.0, vectLengthSq(vect(-12, -44)));
    TEST_ASSERT_EQUAL_FLOAT(1698.00245, vectLengthSq(vect(33.41, -24.12)));
    TEST_ASSERT_EQUAL_FLOAT(407135.952953, vectLengthSq(vect(-551.243, -321.352)));
}

void test_vectLength() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectLength(zeroVect));
    TEST_ASSERT_EQUAL_FLOAT(1.0, vectLength(vect(1, 0)));
    TEST_ASSERT_EQUAL_FLOAT(5.0, vectLength(vect(3, 4)));
    TEST_ASSERT_EQUAL_FLOAT(10.295630140987, vectLength(vect(-5, 9)));
    TEST_ASSERT_EQUAL_FLOAT(45.60701700396552, vectLength(vect(-12, -44)));
    TEST_ASSERT_EQUAL_FLOAT(41.206825890864245, vectLength(vect(33.41, -24.12)));
    TEST_ASSERT_EQUAL_FLOAT(638.0720593733909, vectLength(vect(-551.243, -321.352)));
}

void test_vectNormalize() {
    TEST_ASSERT_FLOAT_IS_NAN(vectNormalize(zeroVect).x);
    TEST_ASSERT_FLOAT_IS_NAN(vectNormalize(zeroVect).y);
    TEST_ASSERT_EQUAL_FLOAT(1.0, vectNormalize(vect(0, 1)).y);
    TEST_ASSERT_EQUAL_FLOAT(0.6, vectNormalize(vect(3, 4)).x);
    TEST_ASSERT_EQUAL_FLOAT(-0.910366477, vectNormalize(vect(5, -11)).y);
    TEST_ASSERT_EQUAL_FLOAT(-0.565911299, vectNormalize(vect(-24.12, 35.14)).x);
    TEST_ASSERT_EQUAL_FLOAT(-0.913993947 , vectNormalize(vect(104.425, -235.241)).y);
}

void test_vectClamp() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectClamp(zeroVect, 5.0).x);
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectClamp(zeroVect, 1.0).y);
    TEST_ASSERT_EQUAL_FLOAT(3.0, vectClamp(vect(3, 4), 6.0).x);
    TEST_ASSERT_EQUAL_FLOAT(-4.357877685622746, vectClamp(vect(9, -16), 5.0).y);
    TEST_ASSERT_EQUAL_FLOAT(21.42, vectClamp(vect(21.42, -19.04), 30.0).x);
    TEST_ASSERT_EQUAL_FLOAT(179.68001500764734, vectClamp(vect(-912.123, 351.244), 500.0).y);
}

void test_vectLerp() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectLerp(zeroVect, vect(5, 2), 0.0).x);
    TEST_ASSERT_EQUAL_FLOAT(2.0, vectLerp(vect(5, 2), zeroVect, 0.0).y);
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectLerp(vect(1, 4), vect(-3, 5), 0.25).x);
    TEST_ASSERT_EQUAL_FLOAT(0.199999, vectLerp(vect(-19, 21), vect(-15, -31), 0.4).y);
    TEST_ASSERT_EQUAL_FLOAT(14.3520, vectLerp(vect(31.52, 19.95), vect(-22.13, 11.11), 0.32).x);
    TEST_ASSERT_EQUAL_FLOAT(-240.24, vectLerp(vect(512.241, 92.412), vect(922.122, -351.124), 0.75).y);
}

void test_vectDistSq() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectDistSq(zeroVect, zeroVect));
    TEST_ASSERT_EQUAL_FLOAT(25.0, vectDistSq(vect(-3, 5), vect(0, 9)));
    TEST_ASSERT_EQUAL_FLOAT(1780.0, vectDistSq(vect(21, 12), vect(-21, 8)));
    TEST_ASSERT_EQUAL_FLOAT(121181.0, vectDistSq(vect(-314, 241), vect(-244, -100)));
    TEST_ASSERT_EQUAL_FLOAT(231.8137, vectDistSq(vect(14.52, 22.44), vect(13.11, 7.28)));
    TEST_ASSERT_EQUAL_FLOAT(622106.702388, vectDistSq(vect(-161.532, -253.351), vect(-241.314, 531.341)));
}

void test_vectDist() {
    TEST_ASSERT_EQUAL_FLOAT(0.0, vectDist(zeroVect, zeroVect));
    TEST_ASSERT_EQUAL_FLOAT(5.0, vectDist(vect(-3, 5), vect(0, 9)));
    TEST_ASSERT_EQUAL_FLOAT(42.19004621946, vectDist(vect(21, 12), vect(-21, 8)));
    TEST_ASSERT_EQUAL_FLOAT(348.1106146040, vectDist(vect(-314, 241), vect(-244, -100)));
    TEST_ASSERT_EQUAL_FLOAT(15.2254293864, vectDist(vect(14.52, 22.44), vect(13.11, 7.28)));
    TEST_ASSERT_EQUAL_FLOAT(788.7374102881136, vectDist(vect(-161.532, -253.351), vect(-241.314, 531.341)));
}

void test_vectNear() {
    TEST_ASSERT_TRUE(vectNear(zeroVect, zeroVect, 1.0));
    TEST_ASSERT_FALSE(vectNear(vect(-3, 5), vect(0, 9), 4.0));
    TEST_ASSERT_TRUE(vectNear(vect(21, 12), vect(-21, 8), 45.3));
    TEST_ASSERT_FALSE(vectNear(vect(-314, 241), vect(-244, -100), 333.15));
    TEST_ASSERT_TRUE(vectNear(vect(14.52, 22.44), vect(13.11, 7.28), 20.124));
    TEST_ASSERT_FALSE(vectNear(vect(-161.532, -253.351), vect(-241.314, 531.341), 724.124));
}

int main() {
    UNITY_BEGIN();
    RUN_TEST(test_vect);
    RUN_TEST(test_vectEqual);
    RUN_TEST(test_vectAdd);
    RUN_TEST(test_vectSub);
    RUN_TEST(test_vectMult);
    RUN_TEST(test_vectNeg);
    RUN_TEST(test_vectDot);
    RUN_TEST(test_vectCross);
    RUN_TEST(test_vectForAngle);
    RUN_TEST(test_vectToAngle);
    RUN_TEST(test_vectRotate);
    RUN_TEST(test_vectUnrotate);
    RUN_TEST(test_vectProject);
    RUN_TEST(test_vectLengthSq);
    RUN_TEST(test_vectLength);
    RUN_TEST(test_vectNormalize);
    RUN_TEST(test_vectClamp);
    RUN_TEST(test_vectLerp);
    RUN_TEST(test_vectDistSq);
    RUN_TEST(test_vectDist);
    RUN_TEST(test_vectNear);
    return UNITY_END();
}
