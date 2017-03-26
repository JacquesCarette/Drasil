function tests = temperatureTest
tests = functiontests(localfunctions);
end

function setupOnce(testCase)
%addpath('Testing');
addpath('Matlab');
end

function epsilon = getEpsilon
epsilon = 0.000001;
end

function [params] = getParams
[params] = load_params('M01.txt');
end

function temperature1Test(testCase)
t = 100;
T = [40.7, 40.5];
dTact = temperature1(t, T, getParams);
dTexp(1) = 0.00139536;
dTexp(2) = 0.002708315;
verifyEqual(testCase, dTact, dTexp.', 'RelTol', getEpsilon)
end

function temperature2aTest(testCase)
t = 3000;
T = [44.2, 44.2];
dTact = temperature2(t, T, getParams);
dTexp(1) = 0.001108642;
dTexp(2) = 0;
dTexp(3) = 0;
verifyEqual(testCase, dTact, dTexp.', 'RelTol', getEpsilon)
end

function temperature2bTest(testCase)
t = 4000;
T = [45, 44.2];
dTact = temperature2(t, T, getParams);
dTexp(1) = -0.000573435;
dTexp(2) = 0;
dTexp(3) = 960;
verifyEqual(testCase, dTact, dTexp.', 'RelTol', getEpsilon)
end

function temperature3Test(testCase)
t = 25000;
T = [47, 46.5];
dTact = temperature3(t, T, getParams);
dTexp(1) = -0.00038229;
dTexp(2) = 0.005249596;
verifyEqual(testCase, dTact, dTexp.', 'RelTol', getEpsilon)
end