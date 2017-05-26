function [pass, newslip, ReportCode, CodeSlip, thetalist] = ...
   KinAdm(slip, strat1, params_soln)

% Slope Stability Analysis Program
% KinAdm.m
%
% 20 August 2015
% 
%  - For a description of the module interface refer to the MIS.
% (../Documentation Files/MIS_SSP.pdf)
%
%  - For a description of the module secrets and services refer to the MG.
% (../Documentation Files/MG_SSP.pdf)
% 
%
% Admissibility criterion : 
% 
%        (i)     x-ordinates must be monotonically increasing
%        (ii)    end vertices must be on slope surface
%        (iii)   non-end vertices must be within the domain (i.e. below
%                the set of line segments in strat1 and between the
%                min and max abscissae of strat1)
%        (iv)    line segments of slip surface must not intersect the
%                upper surface, strat1
%        (v)     slopes of the line segments must be monotonically
%                increasing (i.e. surface must be concave up)
%        (vi)    enclosed angle between adjacent slice bases should not be
%                too sharp (<110deg) to prevent unrealistic convergence of
%                factor of safety
% 
% -------------------------------------------------------------------------

eps_cncvu = 1E-5;

cncvu = params_soln.cncvu;
obtu = params_soln.obtu;

xmin = min(strat1(1,:));
xmax = max(strat1(1,:));
nvtx = size(slip,2);
npsrf = size(strat1,2);

thetalist = zeros (1, nvtx-2);

pass=1;
CodeA='';
CodeB='';

for j = 1:nvtx-1 % loop through vertices of slope
            
    xcur = slip(1,j);      ycur = slip(2,j);
    xnxt = slip(1,j+1);    ynxt = slip(2,j+1);
            
    if j > 1,	mprv = mcur;    end
        mcur = (ynxt-ycur)/(xnxt-xcur);
        bcur = ycur - mcur*xcur;
            
    % (i) x-coords monotonically increasing
    if xnxt < xcur
        pass = 0;
        ReportCode=' Failure Code1 - Non monotonic x';
        break;
    end
            
    % (ii) end vertices must be on slope surface
    if j == 1 % entrance vertice
        
        if xcur < xmin || xcur > xmax
            pass = 0;
            ReportCode=' Failure Code2 - Vertex outside x range';
            break;
        end

        for k = 1:npsrf-1
            xly1 = strat1(1,k);   yly1 = strat1(2,k);
            xly2 = strat1(1,k+1); yly2 = strat1(2,k+1);
                    
            if xcur >= xly1 && xcur <= xly2
                yint = yly1 + (xcur-xly1)*(yly2-yly1)/(xly2-xly1); % y of slope surface at x(1)
                CodeA=strcat(' Start vertice adjustment, ',num2str(ycur),'->',num2str(yint));
                ycur = yint; % change slip y to slope surface y
                slip(2,1) = yint;
                mcur = (ynxt-ycur)/(xnxt-xcur);
                bcur = ycur - mcur*xcur;
                break;
            end
        end
        
    elseif j == nvtx-1 % exit vertice
        
        if xnxt < xmin || xnxt > xmax
            pass = 0;
            ReportCode=' Failure Code2 - Vertex outside x range';
            break;
        end
        
        for k = npsrf:-1:2
            xly1 = strat1(1,k-1); yly1 = strat1(2,k-1);
            xly2 = strat1(1,k);   yly2 = strat1(2,k);
                    
            if xnxt >= xly1 && xnxt <= xly2
                yint = yly1 + (xnxt-xly1)*(yly2-yly1)/(xly2-xly1);
                CodeB=strcat(' End vertice adjustment, ',num2str(ynxt),'->',num2str(yint));
                ynxt = yint;
                slip(2,nvtx) = yint;
                mcur = (ynxt-ycur)/(xnxt-xcur);
                bcur = ycur - mcur*xcur;
                break;
            end
        end
        
        for k = 1:npsrf-1
            xly1 = strat1(1,k);   yly1 = strat1(2,k);
            xly2 = strat1(1,k+1); yly2 = strat1(2,k+1);

            if xcur >= xly1 && xcur <= xly2
                ys = yly1 + (xcur-xly1)*(yly2-yly1)/(xly2-xly1);
                break;
            end
        end
        
        if ycur > ys
            pass = 0;
            ReportCode=' Failure Code3 - Vertex above surface';
            break;
        end
            
    % (iii) non-end vertices must be in the domain
    else
        if xcur < xmin || xcur > xmax
            pass = 0;
            ReportCode=' Failure Code2 - Vertex outside x range';
            break;
        end
        
        for k = 1:npsrf-1
            xly1 = strat1(1,k);   yly1 = strat1(2,k);
            xly2 = strat1(1,k+1); yly2 = strat1(2,k+1);

            if xcur >= xly1 && xcur <= xly2
                ys = yly1 + (xcur-xly1)*(yly2-yly1)/(xly2-xly1);
                break;
            end
        end
        
        if ycur > ys
            pass = 0;
            ReportCode=' Failure Code3 - Vertex above surface';
            break;
        end
    end
            
    % (iv) Slip surface line segments must not intersect
    %           uppermost stratigraphic layer
            
    for k = 1:npsrf-1
        xly1 = strat1(1,k);   yly1 = strat1(2,k);
        xly2 = strat1(1,k+1); yly2 = strat1(2,k+1);
        mly = (yly2-yly1)/(xly2-xly1);
        bly = yly1 - mly*xly1;
        
        xint = (bly-bcur)/(mcur-mly);
        
        if  xint-xcur > 1e-3 && ...
            xint-xnxt < -1e-3 && ...
            xint-xly1 > 1e-3 && ...
            xint-xly2 < -1e-3
            pass = 0;
            ReportCode=' Failure Code4 - Surface Intersection';
        break;
        end
    end
    
    if ~pass,	break;	end
            
    % (v) surface must be concave up (optional)
    if cncvu && j > 1 && mcur < mprv - eps_cncvu
        pass = 0;
        ReportCode=strcat(' Failure Code5 - Concave Down, mcur=',num2str(mcur),' mprv=',num2str(mprv));
        break;
    end
    
    % (vi) angles must be >110deg between slice bases (optional)
    if obtu && j > 1
        xprv = slip(1,j-1);    yprv = slip(2,j-1);
        
        asq = (xcur-xprv)^2 + (ycur-yprv)^2;
        a = sqrt(asq);
        bsq = (xnxt-xcur)^2 + (ynxt-ycur)^2;
        b = sqrt(bsq);
        csq = (xnxt-xprv)^2 + (ynxt-yprv)^2;
        theta = acos((asq+bsq-csq)/(2*a*b));
        thetalist(j-1) = theta;

        if theta < 1.9199     % limit is 110deg in radians
            pass = 0;
            ReportCode=strcat(' Failure Code6 - Sharp angle, Theta=',num2str(theta));
            break;
        end
        
    end
end

newslip=slip;

if pass
    ReportCode=strcat('');
end
CodeSlip=strcat(CodeA,' ',CodeB);

end