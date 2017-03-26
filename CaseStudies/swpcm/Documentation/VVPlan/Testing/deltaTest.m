function data = deltaTest(inputNums)
%deltaTest produces a table of the delta values given by PCM_Error when run on the out files with the given numbers in inputNum


inputsLength = length(inputNums);

data = zeros(5, inputsLength);

j = 1;  %column index (input files);
C = {'Twat', 'TPCM', 'Ewat', 'EPCM', 'Etot'};

for x = inputNums;
  Fstring = sprintf('Fortran/output_F%02d.out', x);
  Mstring = sprintf('Matlab/output_M%02d.txt', x);
  for i = [1:5];                                      %row index (comparators)
    data(i, j) = PCM_Error(Fstring, Mstring, C(i));
  end;
  j = j+1;
 end;
 
 dlmwrite('deltaTesting.txt', data,'delimiter', '\t', 'precision', '%.6f');