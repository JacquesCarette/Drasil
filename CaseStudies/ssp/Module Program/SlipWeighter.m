function [newslips] = SlipWeighter (Meval, slipsurfs)

% Slope Stability Analysis Program
% SlipWeighter.m
%
% 20 August 2015
% 
%  - For a description of the module interface refer to the MIS.
% (../Documentation Files/MIS_SSP.pdf)
%
%  - For a description of the module secrets and services refer to the MG.
% (../Documentation Files/MG_SSP.pdf)
%
% ---------------------
%
% Assigns weights to a pool of possible critical slip surfaces based on 
% factors of safety. A slip surface with a smaller factor of safety will be
% assigned a smaller weight. Slip surfaces are then organized by ascending
% factors of safety.
% -------------------------------------------------------------------------

wts = zeros(Meval,1);
newslips = cell(Meval,3);
for i = 1:Meval
    wts(i) = slipsurfs{i,2};
end
[wts,ix] = sort(wts);
wts = wts-wts(Meval);
wtSum = sum(wts);
wts = wts./wtSum;
for i = 1:Meval
    newslips{i,1} = slipsurfs{ix(i),1};
    newslips{i,2} = slipsurfs{ix(i),2};

    if i > 1, newslips{i,3} = newslips{i-1,3} + wts(i);
    else newslips{i,3} = wts(i);
    end
    
end

end
