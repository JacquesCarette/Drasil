function tests = latentHeatTest
tests = functiontests(localfunctions)
end

%function setupOnce(testCase)
%addpath('Testing');
%end

function e = getGlobalEpsilon
global epsilon
epsilon = 0.005;
e = epsilon;
end

function closedFormTest(testCase)
closedData = TwatClosed('M01Latent.txt');
closedDataTime = closedData(:, 1);
closedDataTemp = closedData(:, 2);

main('Matlab/M01.txt');
coupledData = dlmread('Matlab/M01.out', '', [280 0 910 1]);
coupledDataTime = coupledData(:, 1);
coupledDataTemp = coupledData(:, 2);

delta = errorCalc(closedDataTime, coupledDataTime, closedDataTemp, coupledDataTemp)
verifyLessThanOrEqual(testCase, delta, getGlobalEpsilon)
end

function teardownOnce(testCase)
delete('Matlab/M01.out')
end

