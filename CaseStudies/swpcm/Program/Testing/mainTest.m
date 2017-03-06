function tests = mainTest
  tests = functiontests(localfunctions);
  end

function testNormalInput(testCase)
  actSolution = dlmread('deltaTestNormal.txt');
  expSolution = deltaTest([1:2, 5:8])
  verifyEqual(testCase, actSolution, expSolution)
  end

function testTcTinit(testCase)
  f = @() main("M09.in");
  assertExceptionThrown(f, 'temperature:TcTinit');
  end
 
function testBoiling(testCase)
  f = @() main("M03.in");
  assertExceptionThrown(f, 'temperature:BoilingPt');
  end
 
function testFreezing(testCase)
  f = @() main("M04.in");
  assertExceptionThrown(f, 'temperature:FreezingPt');
  end
  
