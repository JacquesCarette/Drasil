function delta = closedFormdelta()

addpath('Testing');

closedData = TwatClosed('M01Latent.txt');
closedDataTime = closedData(:, 1);
closedDataTemp = closedData(:, 2);

main('Testing/Matlab/M01.txt');
coupledData = dlmread('Testing/Matlab/M01.out', '', [278 0 908 1]);
coupledDataTime = coupledData(:, 1);
coupledDataTemp = coupledData(:, 2);

delete('Testing/Matlab/M01.out');

delta = errorCalc(closedDataTime, coupledDataTime, closedDataTemp, coupledDataTemp)
