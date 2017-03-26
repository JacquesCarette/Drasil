function tests = eventTest
tests = functiontests(localfunctions);
end

function setupOnce(testCase)
%addpath('Testing');
addpath('Matlab');
end

function epsilon = getEpsilon
epsilon = 0.00000001;
end

function [params] = getParams
[params] = load_params('M01.txt');
end

function event1aTest(testCase)
t = 100;
T = [41, 40.9];
[actVal, actIs, actDir] = event1(t, T, getParams);
actOut = [actVal, actIs, actDir];
expOut = [3.3, 1, 0];
verifyEqual(testCase, actOut, expOut, 'RelTol', getEpsilon)
end

function event1bTest(testCase)
t = 3000;
T = [44.2, 44.2];
[actVal, actIs, actDir] = event1(t, T, getParams);
actOut = [actVal, actIs, actDir];
expOut = [0, 1, 0];
verifyEqual(testCase, actOut, expOut, 'RelTol', getEpsilon)
end

function event2aTest(testCase)
t = 3000;
T = [44.2, 44.2, 0];
[actVal, actIs, actDir] = event2(t, T, getParams);
actOut = [actVal, actIs, actDir];
expOut = [-1, 1, 0];
verifyEqual(testCase, actOut, expOut, 'RelTol', getEpsilon)
end

function event2bTest(testCase)
t = 4000;
T = [45, 44.2, 600000];
[actVal, actIs, actDir] = event2(t, T, getParams);
actOut = [actVal, actIs, actDir];
expOut = [-0.943683441, 1, 0];
verifyEqual(testCase, actOut, expOut, 'RelTol', getEpsilon)
end

function event2cTest(testCase)
t = 20570;
T = [44.7, 44.2, 10654060];
[actVal, actIs, actDir] = event2(t, T, getParams);
actOut = [actVal, actIs, actDir];
expOut = [0, 1, 0];
verifyEqual(testCase, actOut, expOut, 'RelTol', getEpsilon)
end