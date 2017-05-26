function [evalslip] = Slicer(evnslc, slip, inslice)

% Slope Stability Analysis Program
% Slicer.m
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
% Prepares a slip for evaluation by designating the slice points of a
% surface. The number of slices can be chosen, or left empty and the
% default number of 36 slices used.
% -------------------------------------------------------------------------


if isempty(inslice) % default # of slices
    xslice = 36;
else % specified # of slices
    xslice = inslice; 
end

nvtx = size(slip,2);
nsubslice = xslice / (nvtx-1);

if evnslc
    % initialize surface for evaluation
    evalslip = zeros(2,xslice+1);
    evalslip(:,1:nvtx) = slip(:,:);

    % initialize slice counter
    islice = nvtx-1;

    % divide segments in half until desired number of slices reached
    while islice < xslice
        % find maximum segment width
        [tmp,imax] = max(diff(evalslip(1,1:islice+1))); %#ok<ASGLU>

        % shift vertices after max width segment
        evalslip(:,imax+2:islice+2) = evalslip(:,imax+1:islice+1);

        % take average for new vertex
        evalslip(:,imax+1)=0.5*(evalslip(:,imax)+evalslip(:,imax+2));

        % increment slice counter
        islice = islice+1;
    end
    
else
    % initialize surface
    evalslip = zeros(2,xslice+1);
    evalslip(:,1) = slip(:,1);

    % divide each subsegment into an equal number of slices
    for j = 1:nvtx-1
        % beginning and end coords of subsegment
        xsl1 = slip(1,j);     ysl1 = slip(2,j);
        xsl2 = slip(1,j+1);   ysl2 = slip(2,j+1);

        % slope and y-intercept of subsegment
        msl = (ysl2-ysl1)/(xsl2-xsl1);
        b = (xsl2-xsl1)/nsubslice;

        % divide into slices
        for k = 1:nsubslice
            ii = (j-1)*nsubslice+k; iii = ii+1;
            evalslip(1,iii) = evalslip(1,ii)+b;
            evalslip(2,iii) = evalslip(2,ii)+msl*b;
        end    
    end
end

end