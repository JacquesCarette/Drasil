function tests = energyTest
tests = functiontests(localfunctions);
end

function setupOnce(testCase)
addpath('Matlab');
end

function epsilon = getEpsilon
epsilon = 0.00000001;
end

function [params] = getParams
[params] = load_params('M01.txt');
end

function energy1Test(testCase)
T = [40:44 ; 40:44];
expEw = [0; 627795.0938; 1255590.188; 1883385.281; 2511180.375];
expEp = [0; 88616; 177232; 265848; 354464];
expE = [expEw, expEp];
[actEw, actEp] = energy1(T.', getParams);
actE = [actEw, actEp];
verifyEqual(testCase, actE, expE, 'RelTol', getEpsilon)
end

function energy2Test(testCase)
T = [44.2:0.1:44.6; 44.2, 44.2, 44.2, 44.2, 44.2; 372000:51000:576000];
expEw = [2636739.394; 2699518.903; 2762298.413; 2825077.922; 2887857.431];
expEp = [744187.2; 795187.2; 846187.2; 897187.2; 948187.2];
expE = [expEw, expEp];
[actEw, actEp] = energy2(T.', getParams);
actE = [actEw, actEp];
verifyEqual(testCase, actE, expE, 'RelTol', getEpsilon)
end

function energy3Test(testCase)
T = [45:49 ; 45:49];
expEw = [3138975.469; 3766770.563; 4394565.657; 5022360.75; 5650155.844];
expEp = [11117682.8; 11231977.3; 11346271.8; 11460566.3; 11574860.8];
expE = [expEw, expEp];
[actEw, actEp] = energy3(T.', getParams);
actE = [actEw, actEp];
verifyEqual(testCase, actE, expE, 'RelTol', getEpsilon)
end