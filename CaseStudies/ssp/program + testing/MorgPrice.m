function [ F, lam, Nint, Tint, thrust, piezthrust ] = ...
    MorgPrice(	slip, strat, phily, cohly, gamly, gamsly, piez, gamw, ...
                Q, omega, Kc, ltor, ftype )
% MorgPrice.m
% 10 October 2011
% 
% Contact:
% Brandon Karchewski
% c/o Department of Civil Engineering - JHE-301
% McMaster University
% 1280 Main St W
% Hamilton, Ontario, Canada
% L8S 4L7
% P: 905-525-9140 x24287
% F: 905-529-9688
% E: karcheba@mcmaster.ca
% 
% 
% ---------------------
% Revision Notes:
% ---------------------
%
% 8 January 2012
% =Modified shape of interslice force outputs to match that of inputs and
%       other outputs
%
% 6 January 2012
% +Added return values for interslice forces
%
% 21 October 2011
% +Added computation of line of thrust for interslice normal forces
%       (effective) and water pressure forces
%
% 19 October 2011
% +Reformulated including force of water pressure on top of slice, loads at
%       top of slice, and earthquake load factor
% +Reformulated including water pressure terms on the interslice interfaces
%       explicitly and relating the mobilized interslice shear forces to
%       the effective interslice normal forces (as opposed to the total
%       interslice normal forces).
% -Removed non-vectorized loops that were previously commented out.
% -Removed code for simple average base properties that was previously
%       commented out.
% 
% 
% ---------------------
% Description:
% ---------------------
% 
% This function determines the factor of safety, F, for a slope given
% stratigraphy, material properties, location of water table, and the
% coordinates of the assumed (general shape) slip surface in accordance
% with the Morgenstern-Price method. The evaluation is based on the
% algorithm presented in:
% 
%	Zhu,D.Y., Lee,C.F., Qian,Q.H., and Chen,G.R. (2005). A concise
%   	algorithm for computing the factor of safety using the
%       Morgenstern-Price method. Canadian Geotechnical Journal, 42(1),
%       272-278.
% 
% The technique generally involves computing the factor of safety, F, based
% on a ratio of available shear resistance to mobilized shear force and a
% constant that relates interslice (effective) normal force to interslice
% shear force, lam. These values are computed iteratively until both
% converge.
% 
% 
% ---------------------
% Input Parameters:
% ---------------------
% 
% slip      matrix with dimension [2,nvtx] containing the coordinates of the
%           base of each interslice interface where nvtx is the number of
%           interfaces (vertices)
% strat		cell array with dimension [nlayer,1] with each cell containing
%         	a matrix of the coordinates of a stratigraphic layer with
%           dimension [2,npts]; the layers must proceed from top to bottom
%           and every layer must span the domain (partial layering may be
%           captured by "pinching out" to zero thickness in regions where
%           the layer is not present)
% phily     vector containing effective friction angle of each layer;
%           [1,nlayer] or [nlayer,1]
% cohly     vector containing effective cohesion of each layer; [1,nlayer]
%           or [nlayer,1]
% gamly     vector containing dry unit weight of each layer; [1,nlayer] or
%           [nlayer,1]
% gamsly    vector containing saturated unit weight of each layer;
%           [1,nlayer] or [nlayer,1]
% piez      matrix of the coordinates of the piezometric surface (water
%           table) with dimension [2,nptspz]
% gamw      unit weight of water; [1,1]
% Q         vector of applied loads on slope surface (positive down) of
%           dimension [1,n]
% omega     vector of application angles for applied loads, Q, of dimension
%           [1,n] and measured clockwise from vertical
% Kc        earthquake load factor (positive acceleration assumed to be in the
%           positive x direction); [1,1]
% ltor      switch for left-to-right slope movement,
%           0 => right-to-left
%         	1 => left-to-right; [1,1]
% ftype     switch for the interslice force function,
%           0 => f(x) = f(x) = sin[ pi*(x - xl)/(xr - xl) ]
%           1 => f(x) = 1; [1,1]
% 
% 
% ---------------------
% Output Parameters:
% ---------------------
% 
% F             the factor of safety; [1,1]
% lam         	the ratio relating interslice shear force to interslice
%               normal force; [1,1]
% Nint          interslice normal forces (compression +ve); [1,nint]
% Tint          interslice shear forces (+ve face, +ve upward); [1,nint]
% thrust        line of thrust of interslice normal forces; [1,n+1]
% piezthrust    line of thrust of interslice water forces; [1,n+1]
% 
% 
% ---------------------
% Notes:
% ---------------------
% 
% 1. There are no special restrictions on slice width (it need not be
%       constant and there is no maximum size), but one should be aware
%       that extremely narrow slices will cause propagation of numerical
%       error to the extent that the algorithm will either not converge or
%       give spurious results.
% 2. For slices where the base crosses a bedding plane, it is assumed that
%       only one bedding plane is crossed at a time in order to compute the
%       weighted average properties for that slice. Therefore, one should
%       ensure that this is the case before calling this function otherwise
%       the slice properties may not be modelled accurately.
% 3. It is assumed that the surface provided to this function is
%       kinematically admissible. Consequently, one should verify that this
%       is indeed the case prior to calling the function (either
%       graphically or based on a set of constraints) otherwise the results
%       may be spurious.
% 4. The vector of effective angles of friction, phily, is assumed to be in
%       radians.
% -------------------------------------------------------------------------


% COMPUTE SLICE PROPERTIES
%{
n           number of slices
nint        number of non-zero interfaces (n-1)
npz         number of vertices in piezometric line
nly         number of stratigraphic layers
nply        number of vertices in each stratigraphic layer
b           width of slice
h           height of slice
alpha       base angle of slice (+ve is CCW from +ve x-axis)
sina        sin(alpha)
cosa        cos(alpha)
tana        tan(alpha)
seca        sec(alpha)
beta        surface angle of slice (+ve is CCW from +ve x-axis)
sinb        sin(beta)
cosb        cos(beta)
secb        sec(beta)
sinw        sin(omega)
cosw        cos(omega)
ub          water pressure at base of interface
ut          water pressure at top of interface
Ub          water force at base of slice
Ut          water force at top of slice
hw          height of interslice water force (from base of interface)
H           interslice water forces
w           total weight per unit width along each interface
W           total weight of slice
ypz         y-coord of piez surf for each interface
yly         y-coord of each strat layer at each interface
phi         tan(phi) at base of each interface/slice (effective)
coh         cohesion at base of each interface/slice (effective)
cly1,cly2   for tracking layer transitions
f           interslice function
%}


% get count of slices, layers, and vertices (for both piez and strat)
n = size(slip,2) - 1;
nint = n-1;
npz = size(piez,2);
nly = size(strat,1);
nply = zeros(nly,1);
for i = 1:nly
    nply(i) = size(strat{i},2);
end

% allow for empty surface load vector
if isempty(Q) || isempty(omega)
    Q = zeros(1,n);
    omega = zeros(1,n);
end

% compute base angle and width of slices
b = slip(1,2:n+1) - slip(1,1:n);
tana = (slip(2,2:n+1) - slip(2,1:n)) ./ b;
alpha = atan(tana);
sina = sin(alpha);
cosa = cos(alpha);
seca = sec(alpha);

% compute angle parameters for applied loads
sinw = sin(omega);
cosw = cos(omega);

% compute physical slice properties
ub = zeros(1,n+1);
ut = zeros(1,n+1);
w = zeros(1,n+1);
hw = zeros(1,nint);
H = zeros(1,nint);
ypz = zeros(1,n+1);
yly = zeros(nly,n+1);
phi = zeros(1,n+1);
coh = zeros(1,n+1);
cly1 = 0;   % for determining if base crosses bedding plane
cly2 = 0;
for i = 1:n+1   % loop through interfaces

    x = slip(1,i); y = slip(2,i);   % get coordinates at base of interface
    
    % compute y-coord of each strat layer
    for ily = nly:-1:1
        for iply = 1:nply(ily)-1

            x1 = strat{ily}(1,iply);    y1 = strat{ily}(2,iply);
            x2 = strat{ily}(1,iply+1);  y2 = strat{ily}(2,iply+1);

            if x >= x1 && x < x2    % check if coords of base lie in the domain of this line seg
                yly(ily,i) = y1 + (x - x1)*(y2 - y1)/(x2 - x1);    % compute y coord on strat layer
                break;
            end
        end
    end

    if npz > 0  % check if there is a piezometric surface

        for ipz = 1:npz-1   % loop through piez surf line segs

            x1 = piez(1,ipz);   y1 = piez(2,ipz);       % get trailing coords of piez surf
            x2 = piez(1,ipz+1); y2 = piez(2,ipz+1);     % get leading coords of piez surf

            if x >= x1 && x < x2    % check if coords of base lie in the domain of this line seg
                ypz(i) = y1 + (x - x1)*(y2 - y1)/(x2 - x1);    % compute y coord on piez surf
                break;
            end
        end     % ipz, piez surf
        
                
        if yly(1,i) < ypz(i)    % if piez surface is above ground surface

            ut(i) = (ypz(i)-yly(1,i))*gamw;
            ub(i) = (ypz(i)-y)*gamw;
            
            if i > 1 && i < n+1
                
                Hrec = gamw*(ypz(i)-yly(1,i))^2;
                hrec = 0.5*(yly(1,i)-y);
                
                Htri = 0.5*gamw*(yly(1,i)-y)^2;
                htri = 0.33333333333333*(yly(1,i)-y);
                
                H(i-1) = Hrec+Htri;
                hw(i-1) = (Hrec*hrec + Htri*htri)/H(i-1);
                
            end
            
        elseif y < ypz(i)       % if piez surface is below ground, but within slice
            
            ub(i) = (ypz(i)-y)*gamw;
            
            if i > 1 && i < n+1
                H(i-1) = 0.5*gamw*(ypz(i)-y)^2;
                hw(i-1) = 0.33333333333333*(ypz(i)-y);
            end
            
        end
        
    end     % npz>0

    % compute strength at base and soil weight at interface
    yb = y;     % initialize bottom y coord to bottom of interface
    found = 0;  % initialize layer search switch
    for ily = nly:-1:1
        if y > yly(ily,i), continue;    end     % advance to layer that y coord of base is in

        if ~found
            found = 1;
            cly2 = ily;             % for weighted average base properties
        end

        if npz > 0 && ypz(i) > yb  % check if water table lies above bottom y coord
            
            if ypz(i) <= yly(ily,i)     % w.t. lies in current layer, use some sat and some dry weight
                w(i) = w(i) + (ypz(i)-yb)*gamsly(ily);
                w(i) = w(i) + (yly(ily,i)-ypz(i))*gamly(ily);
            else    % current layer is completely submerged, use all sat weight
                w(i) = w(i) + (yly(ily,i)-yb)*gamsly(ily);
            end     % ypz(i)<=yly(ily,i)
            
        else    % w.t. lies below current layer (or is not present), use all dry weight
            
            w(i) = w(i) + (yly(ily,i)-yb)*gamly(ily);
            
        end     % npz>0 && ypz(i)>yb

        yb = yly(ily,i);    % advance to next layer
        
    end     % ily=nly:-1:1

    % compute slice properties
    if cly1
        
        % compute weighted average base properties
        if cly2 > cly1  % if base is descending a layer
            
            % get coords of base line segment
            xsl1 = slip(1,i-1); ysl1 = slip(2,i-1);
            xsl2 = x;           ysl2 = y;
            
            % get coords of strat layer line seg that is being crossed
            for iply = 1:nply(cly2)-1
                xly1 = strat{cly2}(1,iply);    yly1 = strat{cly2}(2,iply);
                xly2 = strat{cly2}(1,iply+1);  yly2 = strat{cly2}(2,iply+1);
                
                if xsl1 >= xly1 && xsl1 < xly2, break;  end
            end
            
            % compute slope and y-intercept of strat layer line seg
            mly = (yly2-yly1)/(xly2-xly1);
            bly = yly1 - mly*xly1;
            
            % compute slope and y-intercept of base of slice
            msl = (ysl2-ysl1)/(xsl2-xsl1);
            bsl = ysl1 - msl*xsl1;
            
            % compute intersection point between slice and layer
            xint = (bsl-bly)/(mly-msl);
            yint = msl*xint + bsl;
            
            % compute weight for trailing layer properties
            % weight for leading layer properties is (1-wt)
            wt = sqrt((xint-xsl1)^2+(yint-ysl1)^2)/sqrt((xsl2-xsl1)^2+(ysl2-ysl1)^2);
            
            % linearly interpolate between layer properties
            phi(i-1) = tan(wt*phily(cly1)+(1-wt)*phily(cly2));
            coh(i-1) = wt*cohly(cly1)+(1-wt)*cohly(cly2);
            
        elseif cly1 > cly2  % if base is ascending a layer
            
            % get coords of base line segment
            xsl1 = slip(1,i-1); ysl1 = slip(2,i-1);
            xsl2 = x;           ysl2 = y;
            
            % get coords of strat layer line seg that is being crossed
            for iply = 1:nply(cly1)-1
                xly1 = strat{cly1}(1,iply);    yly1 = strat{cly1}(2,iply);
                xly2 = strat{cly1}(1,iply+1);  yly2 = strat{cly1}(2,iply+1);
                
                if xsl1 >= xly1 && xsl1 < xly2, break;  end
            end
            
            % compute slope and y-intercept of strat layer line seg
            mly = (yly2-yly1)/(xly2-xly1);
            bly = yly1 - mly*xly1;
            
            % compute slope and y-intercept of base of slice
            msl = (ysl2-ysl1)/(xsl2-xsl1);
            bsl = ysl1 - msl*xsl1;
            
            % compute intersection point between slice and layer
            xint = (bsl-bly)/(mly-msl);
            yint = msl*xint + bsl;
            
            % compute weight for trailing layer properties
            % weight for leading layer properties is (1-wt)
            wt = sqrt((xint-xsl1)^2+(yint-ysl1)^2)/sqrt((xsl2-xsl1)^2+(ysl2-ysl1)^2);
            
            % linearly interpolate between layer properties
            phi(i-1) = tan(wt*phily(cly1)+(1-wt)*phily(cly2));
            coh(i-1) = wt*cohly(cly1)+(1-wt)*cohly(cly2);
            
        else    % if strat layer has not changed, use current layer props
            
            phi(i-1) = tan(phily(cly2));
            coh(i-1) = cohly(cly2);
            
        end     % if cly2>cly1, elseif cly1>cly2, interpolation of slice properties
        
    end     % if cly1, compute slice properties
    
    cly1 = cly2;    % advance layer tracking variable

end     % i=1:n+1, slice interface and base properties
Ub = 0.5*(ub(2:n+1)+ub(1:n)).*b.*seca;
W = 0.5*(w(2:n+1)+w(1:n)).*b;
phi = phi(1:n); % drop last value of base properties since there are n
coh = coh(1:n); %   slices and n+1 interfaces

% compute height and surface angle of each slice and force of water
% pressure at the top of each slice
h = 0.5*((yly(1,1:n)-slip(2,1:n)) + (yly(1,2:n+1)-slip(2,2:n+1)));
beta = atan((yly(1,2:n+1) - yly(1,1:n)) ./ b);
sinb = sin(beta);
cosb = cos(beta);
secb = sec(beta);
Ut = 0.5*(ut(2:n+1)+ut(1:n)).*b.*secb;

% compute line of thrust of interslice water forces
if npz>0,   piezthrust = [slip(2,1) slip(2,2:n)+hw slip(2,n+1)];
else        piezthrust = [];
end

% compute interslice function
if ftype == 1
    f = ones(1,nint);
else
    f = sin(pi * (slip(1,2:n) - slip(1,1)) / (slip(1,n+1)-slip(1,1)));
end


% COMPUTE RESISTING AND APPLIED SHEAR FORCES FOR EACH SLICE
%{
T     forces tending to cause instability
R     resisting forces (excluding interslice forces)
%}
T = W.*(sina + Kc*cosa) ...
        + Ut.*(cosb.*sina - sinb.*cosa) ...
        + Q.*(cosw.*sina - sinw.*cosa) ... 
        + ([H(1) (H(2:nint)-H(1:nint-1)) -H(nint)]).*cosa;
R = ((W + Ut.*cosb + Q.*cosw).*cosa ...
        + (Kc*W - ([H(1) (H(2:nint)-H(1:nint-1)) -H(nint)]) ...
            + Ut.*sinb + Q.*sinw).*sina - Ub).*phi + coh.*b.*seca;


% CONCISE MORGENSTERN-PRICE ALGORITHM
%{
PHI         multiplier function for interslice normal force on leading side
PSI         multiplier function for interslice normal force on trailing
            side
E           vector of interslice normal forces
eps_s       stopping criterion (based on absolute relative difference in F
            and lam)
dF          absolute relative difference in F
dLam        absolute relative difference in lam
max_iter    maximum allowable iterations for convergence
iter        iteration counter
z           vector of heights of interslice forces from base of interface
%}
PHI = zeros(1,nint);
PSI = zeros(1,nint);
E = zeros(1,nint);
eps_s = 1e-6;
F = 1;
lam = 0;
dF = 1;
dLam = 1;
max_iter = 20;
iter = 0;
while dF > eps_s && dLam > eps_s && iter < max_iter
    
    F_old = F;
    lam_old = lam;
    
    for m = 1:2
        
        % compute PHI and PSI
        PHI = (lam*f.*cosa(1:nint) - sina(1:nint)).*phi(1:nint) ...
                - (lam*f.*sina(1:nint) + cosa(1:nint))*F;
        PSI = ((lam*f.*cosa(2:n) - sina(2:n)).*phi(2:n) ...
                - (lam*f.*sina(2:n) + cosa(2:n))*F) ./ PHI;
        
        % compute F
        sumR = R(n);
        sumT = T(n);
        prodPSI = 1;
        for i = nint:-1:1
            prodPSI = prodPSI*PSI(i);
            sumR = sumR + R(i)*prodPSI;
            sumT = sumT + T(i)*prodPSI;
        end
        
        F = sumR/sumT;
    end
    
    % compute interslice forces
    E(1) = (F*T(1) - R(1))/PHI(1);
    for i = 2:nint
        E(i) = (PSI(i-1)*E(i-1)*PHI(i-1) + F*T(i) - R(i))/PHI(i);
    end
    
    % compute lam
    sumN = b(1)*(E(1)+H(1))*tana(1) ...
            + sum(b(2:nint).*(  E(2:nint) + E(1:nint-1) ...
                                + H(2:nint) + H(1:nint-1)).*tana(2:nint)) ...
            + sum(h.*(Kc*W - 2*Ut.*sinb - 2*Q.*sinw)) ...
            + b(n)*(E(nint)+H(nint))*tana(n);
    sumS = b(1)*f(1)*E(1) ...
            + sum(b(2:nint).*(	f(2:nint).*E(2:nint) ...
                                + f(1:nint-1).*E(1:nint-1))) ...
            + b(n)*f(nint)*E(nint);
    lam = sumN/sumS;
    
    % compute error
    dF = abs(F - F_old);
    dLam = abs(lam - lam_old);
    
    % increment iteration counter
    iter = iter + 1;
    
end

% compute line of thrust of interslice normal forces
z = zeros(1,nint);
z(1) = 0.5*(-b(1)*(E(1)+H(1))*tana(1) + lam*b(1)*f(1)*E(1) ...
                - 2*H(1)*hw(1) - h(1)*(Kc*W(1) - 2*Ut(1)*sinb(1) ...
                                        - 2*Q(1)*sinw(1))) / E(1);
for i = 2:nint
    z(i) = 0.5*(-b(i)*(E(i)+H(i)+E(i-1)+H(i-1))*tana(i) + E(i-1)*z(i-1) ...
                    + lam*b(i)*(f(i)*E(i)+f(i-1)*E(i-1)) ...
                    - 2*(H(i)*hw(i)-H(i-1)*hw(i-1)) ...
                    - h(i)*(Kc*W(i)-2*Ut(i)*sinb(i)-2*Q(i)*sinw(i)))/ E(i);
end
thrust = [slip(2,1) slip(2,2:n)+z slip(2,n+1)];

% flip sign for left-to-right slopes
if ltor
    F = -F;
    lam = -lam;
end

% throw out low and non-converging values due to inadmissible surfaces
if F < 0.5 || iter == max_iter
    F = 1000;
    lam = 0;
    Nint = [];
    Tint = [];
    thrust = [];
    piezthrust = [];
    return;
end

% interslice forces
Nint = E;
Tint = -lam*f.*E;

end

