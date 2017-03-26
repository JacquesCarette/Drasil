function tests = compareFortranTest
tests = functiontests(localfunctions);
end

function setupOnce(testCase)

addpath('compareFortran');
end

function e = getGlobalEpsilon
global epsilon
epsilon = 0.003; % NOTE: testFM5 obeys a different relative tolerance, indicated in the function
e = epsilon;
end

function testFM1(testCase)
main('M01.txt');
delta = PCM_Error('F01.out', 'M01.out', 'Twat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F01.out', 'M01.out', 'TPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F01.out', 'M01.out', 'Ewat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F01.out', 'M01.out', 'EPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F01.out', 'M01.out', 'Etot')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
end

function testFM2(testCase)
main('M02.txt');
delta = PCM_Error('F02.out', 'M02.out', 'Twat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F02.out', 'M02.out', 'TPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F02.out', 'M02.out', 'Ewat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F02.out', 'M02.out', 'EPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F02.out', 'M02.out', 'Etot')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
end

function testFM3(testCase)
main('M03.txt');
delta = PCM_Error('F03.out', 'M03.out', 'Twat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F03.out', 'M03.out', 'TPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F03.out', 'M03.out', 'Ewat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F03.out', 'M03.out', 'EPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F03.out', 'M03.out', 'Etot')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
end

function testFM4(testCase)
main('M04.txt');
delta = PCM_Error('F04.out', 'M04.out', 'Twat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F04.out', 'M04.out', 'TPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F04.out', 'M04.out', 'Ewat')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F04.out', 'M04.out', 'EPCM')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
delta = PCM_Error('F04.out', 'M04.out', 'Etot')
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
end

function testFM5(testCase)
main('M05.txt');
delta = PCM_Error('F01.out', 'M05.out', 'TwatNoP')
verifyLessThanOrEqual(testCase, delta, 1e-6)
end

function teardownOnce(testCase)
delete('M01.out', 'M02.out', 'M03.out', 'M04.out', 'M05.out')
end
