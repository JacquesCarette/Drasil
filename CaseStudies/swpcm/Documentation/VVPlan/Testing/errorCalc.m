function del_rel = errorCalc(trueTime, calcTime, trueVect, calcVect)

%errorCalc calculates the percentage error between two curves

trueVect_new = interp1(trueTime, trueVect, calcTime, 'linear', 'extrap');

delta = trueVect_new - calcVect;

del_rel = norm(delta)/norm(trueVect_new);