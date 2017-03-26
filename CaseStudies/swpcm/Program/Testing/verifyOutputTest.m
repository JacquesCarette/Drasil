function tests = verifyOutputTest
tests = functiontests(localfunctions);
end

function setupOnce(testCase)
addpath('Matlab');
end

function [params] = getParams
[params] = load_params('M01.txt');
end

function testOutput1(testCase)
[params] = getParams;
t = [0;10;20;30];
T = [40 40; 42 41.9; 44 43.8; 46 45.7];
Ew = [0;1000;2000;19800];
Ep = [0;1000;2000;5400];
f = @()verify_output(params, t, T, Ew, Ep);
verifyWarningFree(testCase, f);
end

function testOutput2(testCase)
[params] = getParams;
t = [0;10;20;30];
T = [40 40; 42 41.9; 44 43.8; 46 45.7];
Ew = [0;1000;2000;19800];
Ep = [0;1000;2000;3000];
f = @()verify_output(params, t, T, Ew, Ep);
verifyWarning(testCase, f, 'output:Ep');
end

function testOutput3(testCase)
[params] = getParams;
t = [0;10;20;30];
T = [40 40; 42 41.9; 44 43.8; 46 45.7];
Ew = [0;1000;2000;3000];
Ep = [0;1000;2000;5400];
f = @()verify_output(params, t, T, Ew, Ep);
verifyWarning(testCase, f, 'output:Ew');
end

function testOutput4(testCase)
[params] = getParams;
t = [0;10;20;30];
T = [40 40; 42 41.9; 44 43.8; 46 45.7];
Ew = [0;1000;2000;3000];
Ep = [0;1000;2000;3000];
f = @()verify_output(params, t, T, Ew, Ep);
verifyWarning(testCase, f, 'output:Ew');
verifyWarning(testCase, f, 'output:Ep');
end
