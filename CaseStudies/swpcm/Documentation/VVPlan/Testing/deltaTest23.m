function data = deltaTest(inputNums, filename)
%deltaTest produces a table of the delta values given by PCM_Error when run on the out files with the given numbers in inputNum


inputsLength = length(inputNums);

data = zeros(5, inputsLength);

j = 1;  %column index (input files);
C = {'Twat', 'TPCM', 'Ewat', 'EPCM', 'Etot'};

for x = inputNums;
  Mstring = sprintf('Matlab/M%02d.out', x);
  M23string = sprintf('Matlab/M%02d_23.out', x);
  for i = [1:5];                                      %row index (comparators)
    data(i, j) = PCM_ErrorMM(Mstring, M23string, C(i));
  end;
  j = j+1;
 end;
 
 dlmwrite(filename, data,'delimiter', '\t', 'precision', '%.6f');