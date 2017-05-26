function [ F, Floc, dx, dy, Nint, Tint ] = ...
    RFEM(	slip, strat, phily, cohly, gamly, gamsly, ...
                piez, gamw, ...
                Q, omega, Kc, ltor, ...
                Ely, nuly)
% RFEM.m
% 22 December 2011
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
% =Changed shape of outputs to row vectors (to match inputs)
% =Changed order of input parameters to correspond to that required by
%   GenAlgNonCirc
% 
% 
% ---------------------
% Description:
% ---------------------
% 
% This function determines the factor of safety, F, for a slope given
% stratigraphy, material properties, location of water table, and the
% coordinates of the assumed (general shape) slip surface in accordance
% with the rigid finite element method (RFEM). Details of this technique
% are presented in:
%                 
% 	Stolle, D. and Guo, P. (2008). Limit equilibrium slope stability
%         analysis using rigid finite elements. Canadian Geotechnical
%         Journal, 45(5), 653-662.
% 
% The technique generally involves computing the factor of safety, F, based
% on a ratio of available shear resistance to mobilized shear force at the
% base of each slice. Mobilized forces are computed through stiffness terms
% related to the relative displacements obtained from finite element
% analysis. The stiffness terms themselves are non-linear functions of
% relative displacements. As such, the displacement field is obtained
% through an iterative solve driving the residual load vector to zero.
%                 
% 
% 
% ---------------------
% Input Parameters:
% ---------------------
% 
% slip      matrix with dimension [2,nvtx] containing the coordinates of
%           the base of each interslice interface where nvtx is the number
%           of interfaces (vertices)
% strat     cell array with dimension [nlayer,1] with each cell containing
%         	a matrix of the coordinates of a stratigraphic layer with
%           dimension [2,npts]; the layers must proceed from top to bottom
%           and every layer must span the domain (partial layering may be
%           captured by "pinching out" to zero thickness in regions where
%           the layer is not present)
% phily     vector containing effective friction angle of each layer
%           [2,nlayer]
% cohly     vector containing effective cohesion of each layer [2,nlayer]
% gamly     vector containing dry unit weight of each layer [2,nlayer]
% gamsly	vector containing saturated unit weight of each layer
%           [2,nlayer]
% piez      matrix of the coordinates of the piezometric surface (water
%           table) with dimension [2,nptspz]
% gamw      unit weight of water [1,1]
% Q         vector of applied loads on slope surface (positive down) of
%           dimension [1,n]
% omega     vector of application angles for applied loads, Q, of dimension
%           [1,n] and measured clockwise from vertical
% Kc        earthquake load factor (positive acceleration assumed to be in
%           the positive x direction) [1,1]
% ltor      switch for left-to-right slope movement,
%           0 => right-to-left
%         	1 => left-to-right [1,1]
% Ely       vector containing elastic modulus of each layer [2,nlayer]
% nuly      vector containing Poisson's ratio for each layer [2,nlayer]
% 
% 
% ---------------------
% Output Parameters:
% ---------------------
% 
% F           the factor of safety; [1,1]
% Floc        local factors of safety for each slice; [1,n]
% dx          horizontal displacement of each slice; [1,n]
% dy          vertical displacement of each slice; [1,n]
% Nint        interslice normal forces (compression +ve); [1,nint]
% Tint        interslice shear forces (+ve face, +ve upward); [1,nint]
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



% COMPUTE STRENGTH AND ELASTIC PROPERTIES (BASE AND INTERSLICE)
%{
n           number of slices
nint        number of non-zero interfaces (n-1)
npz         number of vertices in piezometric line
nly         number of stratigraphic layers
nply        number of vertices in each stratigraphic layer
xn          x-coordinates of nodes
bb          width of slice (for base properties)
bi          distance between adjacent nodes (for interslice properties)
h           height of slice (at centreline)
hi          height of interface (between slices)
lb          length of base
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
Ub          water force at base of slice (normal to slice base)
Ut          water force at top of slice (normal to surface)
hw          height of interslice water force (from base of interface)
H           interslice water force
w           total weight per unit width along each interface
W           total weight of slice
ypz         y-coord of piez surf for each interface
yly         y-coord of each strat layer at each interface
phib        tan(phi) at base of each slice (effective)
cohb        cohesion at base of each slice (effective)
Eb          elastic modulus at base of each slice
nub         Poisson's ratio at base of each slice
phii        tan(phi) for each interslice interface (effective)
cohi        cohesion for each interslice interface (effective)
Ei          elastic modulus at each interslice interface
nui         Poisson's ratio at each interslice interface
cly1,cly2   for tracking layer transitions
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
xn = 0.5 * (slip(1,2:n+1)+slip(1,1:n));
bb = slip(1,2:n+1) - slip(1,1:n);
bi = xn(2:n) - xn(1:nint);
tana = (slip(2,2:n+1) - slip(2,1:n)) ./ bb;
alpha = atan(tana);
sina = sin(alpha);
cosa = cos(alpha);
seca = sec(alpha);
lb = sqrt((slip(1,2:n+1)-slip(1,1:n)).^2 + (slip(2,2:n+1)-slip(2,1:n)).^2);

% compute angle parameters for applied loads
sinw = sin(omega);
cosw = cos(omega);

% compute physical slice properties
ub = zeros(1,n+1);
ut = zeros(1,n+1);
w = zeros(1,n+1);
hw = zeros(1,nint);
hi = zeros(1,nint);
H = zeros(1,nint);
ypz = zeros(1,n+1);
yly = zeros(nly,n+1);
phib = zeros(1,n);
cohb = zeros(1,n);
Eb = zeros(1,n);
nub = zeros(1,n);
phii = zeros(1,nint);
cohi = zeros(1,nint);
Ei = zeros(1,nint);
nui = zeros(1,nint);
cly1 = 0;   % for determining if base crosses bedding plane
cly2 = 0;
for i = 1:n+1   % loop through interfaces

    ipv = i-1;
    
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
    end     % ily, strat coords

    if npz  % check if there is a piezometric surface

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
                
                H(ipv) = Hrec+Htri;
                hw(ipv) = (Hrec*hrec + Htri*htri)/H(ipv);
                
            end
            
        elseif y < ypz(i)       % if piez surface is below ground, but within slice
            
            ub(i) = (ypz(i)-y)*gamw;
            
            if i > 1 && i < n+1
                H(ipv) = 0.5*gamw*(ypz(i)-y)^2;
                hw(ipv) = 0.33333333333333*(ypz(i)-y);
            end
            
        end
        
    end     % npz>0

    % compute strength at base and soil weight and stiffness properties at interface
    yb = y;     % initialize bottom y coord to bottom of interface
    found = 0;  % initialize layer search switch
    for ily = nly:-1:1
        if y > yly(ily,i), continue;    end     % advance to layer containing y coord of base

        if ~found
            found = 1;
            cly2 = ily;             % for weighted average base properties
        end
        
        dh = yly(ily,i) - yb;       % height of slice interface in current layer
        
        if i > 1 && i < n+1
            hi(ipv) = hi(ipv) + dh;
            phii(ipv) = phii(ipv) + dh*phily(ily);
            cohi(ipv) = cohi(ipv) + dh*cohly(ily);
            Ei(ipv) = Ei(ipv) + dh*Ely(ily);
            nui(ipv) = nui(ipv) + dh*nuly(ily);
        end

        if npz && ypz(i) > yb  % check if water table lies above bottom y coord
            
            if ypz(i) <= yly(ily,i)     % w.t. lies in current layer, use some sat and some dry weight
                w(i) = w(i) + (ypz(i)-yb)*gamsly(ily);
                w(i) = w(i) + (yly(ily,i)-ypz(i))*gamly(ily);
            else    % current layer is completely submerged, use all sat weight
                w(i) = w(i) + dh*gamsly(ily);
            end     % ypz(i)<=yly(ily,i)
            
        else    % w.t. lies below current layer (or is not present), use all dry weight
            
            w(i) = w(i) + dh*gamly(ily);
            
        end     % npz>0 && ypz(i)>yb

        yb = yly(ily,i);    % advance to next layer
        
    end     % ily=nly:-1:1

    % compute slice properties
    if cly1
        
        % compute weighted average base properties
        if cly2 > cly1  % if base is descending a layer
            
            % get coords of base line segment
            xsl1 = slip(1,ipv); ysl1 = slip(2,ipv);
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
            phib(ipv) = tan(wt*phily(cly1)+(1-wt)*phily(cly2));
            cohb(ipv) = wt*cohly(cly1)+(1-wt)*cohly(cly2);
            Eb(ipv) = wt*Ely(cly1)+(1-wt)*Ely(cly2);
            nub(ipv) = wt*nuly(cly1)+(1-wt)*nuly(cly2);
            
        elseif cly1 > cly2  % if base is ascending a layer
            
            % get coords of base line segment
            xsl1 = slip(1,ipv); ysl1 = slip(2,ipv);
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
            phib(ipv) = tan(wt*phily(cly1)+(1-wt)*phily(cly2));
            cohb(ipv) = wt*cohly(cly1)+(1-wt)*cohly(cly2);
            Eb(ipv) = wt*Ely(cly1)+(1-wt)*Ely(cly2);
            nub(ipv) = wt*nuly(cly2)+(1-wt)*nuly(cly2);
            
        else    % if strat layer has not changed, use current layer props
            
            phib(ipv) = tan(phily(cly2));
            cohb(ipv) = cohly(cly2);
            Eb(ipv) = Ely(cly2);
            nub(ipv) = nuly(cly2);
            
        end     % if cly2>cly1, elseif cly1>cly2, interpolation of slice properties
        
    end     % if cly1, compute slice properties
    
    cly1 = cly2;    % advance layer tracking variable

end     % i=1:n+1, slice interface and base properties
Ub = 0.5*(ub(2:n+1)+ub(1:n)).*bb.*seca;
W = 0.5*(w(2:n+1)+w(1:n)).*bb;
phii = tan(phii./hi);
cohi = cohi./hi;
Ei = Ei./hi;
nui = nui./hi;

% surface angle of each slice and force of water pressure at the top of
% each slice
beta = atan((yly(1,2:n+1) - yly(1,1:n)) ./ bb);
sinb = sin(beta);
cosb = cos(beta);
secb = sec(beta);
Ut = 0.5*(ut(2:n+1)+ut(1:n)).*bb.*secb;


% COMPUTE NONLINEAR STIFFNESS PROPERTIES
%{
a       constant involved in denominator of shear law, taken as 0.00001
kappa   constant involved in numerator of tension law, taken as 1 kPa
ktri    residudal interslice shear stiffness
Knoi    interslice normal stiffness in compression
knri    residual interslice normal stiffness in tension
Ai      constant involved in denominator of tension law (interslice)
ktrb    residudal base shear stiffness
Knob    base normal stiffness in compression
knrb    residual base normal stiffness in tension
Ab      constant involved in denominator of tension law (base)
%}
a = 0.00001;
kappa = 1;
ktri = zeros(1,nint);
Knoi = zeros(1,nint);
knri = zeros(1,nint);
Ai = zeros(1,nint);
ktrb = zeros(1,n);
Knob = zeros(1,n);
knrb = zeros(1,n);
Ab = zeros(1,n);

% interslice properties
for i = 1:nint
    del = bi(i) / 1000;
    del = 1/del;
    ktri(i) = Ei(i) / (2*(1+nui(i))) * (del*1e-4);
    Knoi(i) = Ei(i) * (1-nui(i)) / ((1+nui(i))*(1-2*nui(i) + 0.00001)) * del;
    knri(i) = 0.01*Knoi(i);
    Ai(i) = kappa / (Knoi(i) - knri(i));
end

% base properties
for i = 1:n
    del = bb(i) / 1000;
    del = 1/del;
    ktrb(i) = Eb(i) / (2*(1+nub(i))) * (del*1e-3);
    Knob(i) = Eb(i) * (1-nub(i)) / ((1+nub(i))*(1-2*nub(i) + 0.00001)) * del;
    knrb(i) = 0.01*Knob(i);
    Ab(i) = kappa / (Knob(i) - knrb(i));
end


% CONSTRUCT INITIAL STIFFNESS MATRIX AND LOAD VECTOR
%{
nnet    number of degrees of freedom
gstif   global initial stiffness matrix
estif   element stiffness matrix
gload   global load vector
eload   element load vector
%}
nnet = 2*n;
gstif = sparse(nnet,nnet);
gload = zeros(nnet,1);

% interslice stiffness and load
for i = 1:nint
    
    ii = 2*i-1;
    index = ii:ii+3;
    
    Kn = Knoi(i);
    Kt = ktri(i) + cohi(i) / a;
    
    estif = hi(i) * [   Kn      0       -Kn     0
                        0       Kt      0       -Kt
                        -Kn     0       Kn      0
                        0       -Kt     0       Kt  ];
    
    gstif(index,index) = gstif(index,index) + estif(:,:); %#ok<SPRIX>
    
    eload = [-H(i); 0; H(i); 0];
    
    gload(index) = gload(index) + eload(:);
    
end

% base stiffness and load
for i = 1:n
    
    ii = 2*i-1;
    index = ii:ii+1;
    
    Kn = Knob(i);
    Kt = ktrb(i) + cohb(i) / a;
    
    Kloc = [Kt  0;  0   Kn];
    
    T = [cosa(i)    sina(i);    -sina(i)    cosa(i)];
    
    estif = lb(i) * T' * Kloc * T;
    
    gstif(index,index) = gstif(index,index) + estif(:,:); %#ok<SPRIX>
    
    eload = [   -Kc*W(i) -	Ub(i)*sina(i) + Ut(i)*sinb(i) + Q(i)*sinw(i)
                -W(i) +     Ub(i)*cosa(i) - Ut(i)*cosb(i) - Q(i)*cosw(i) ];
    
    gload(index) = gload(index) + eload(:);
    
end


% SOLVE FOR DISPLACEMENTS
%{
This nonlinear solver uses a modified Newton-Raphson solve to determine the
displacement increments on each load step and bisection method for the load
stepping. For factors of safety greater than one, there will only be a
single load step. Otherwise, the factor of safety is determined as the
proportion of the load that results in a factor of safety equal to one.

eps_s   convergence criterion for modified N-R
niter   iteration limit for a given load step
nstep   maximum number of load steps
relax   relaxation factor for updated displacements
dfact   load factor increment
fact0   base load factor

gdisp0      initial global displacements
dload0      initial global internal forces
dload       global internal forces

eps_a       approximation error
iter        iteration counter
converge    switch for convergence

iload       global (residual) load vector for current iteration
idisp       displacement increment for current iteration
newdisp     unrelaxed global displacements

u       local displacements (for interslice forces)
du      relative local displacements

ug      local displacements, global coords (for base forces)
ul      local displacements, local coords
T       transformation matrix
floc    internal forces, local coords

Kn      normal stiffness
Kt      tangential stiffness
sig_n   normal stress (tension positive)
sig_t   tangential stress (+ve face, +ve direction)
Fmc     Coulomb failure function, c-sig_n*tan(phi) >= 0
%}

% initialization
eps_s = 1e-5;
niter = 5000;
nstep = ceil(log(1/eps_s)/log(2)+1);
relax = 0.25;
dfact = 1;
fact0 = 0;

gdisp0 = zeros(nnet,1);
dload0 = zeros(nnet,1);

% load stepping (bisection method)
for istep = 1:nstep
    
    factor = fact0 + dfact;
    gdisp = gdisp0;
    dload = dload0;
    
    eps_a = 1;
    iter = 0;
    converge = 0;
    
    % solver (modified Newton-Raphson)
    while eps_a > eps_s && iter < niter && factor <= 1
        
        iter = iter+1;
        
        % residual load
        iload = factor*gload - dload;
        
        % displacement increment (initial stiffness with relaxation)
        idisp = gstif\iload;
        newdisp = gdisp + idisp;
        gdisp = (1-relax)*gdisp + relax*newdisp;
        
        % internal forces
        dload = zeros(nnet,1);
        
        for i = 1:nint  % interslice
            
            ii = 2*i-1;     iii = ii+3;
            
            % local displacements
            u = gdisp(ii:iii);
            du = u(3:4)-u(1:2);
            
            % normal stiffness
            if du(1) <= 0,  Kn = Knoi(i);
            else            Kn = knri(i) + kappa / (du(1)+Ai(i));
            end
            
            % normal stress
            sig_n = Kn*du(1);
            
            % tangential stiffness
            Fmc = max(cohi(i) - sig_n*phii(i), 0);
            Kt = ktri(i) + Fmc / (abs(du(2)) + a);
            
            % shear stress
            sig_t = Kt*du(2);
            
            % global internal force
            dload(ii:iii) = dload(ii:iii) ...
                            + hi(i)*[-sig_n; -sig_t; sig_n; sig_t];
            
        end
        
        for i = 1:n     % base
            
            ii = 2*i-1;     iii = ii+1;
            
            % local displacements
            ug = gdisp(ii:iii);
            T = [cosa(i) sina(i); -sina(i) cosa(i)];
            ul = T*ug;
            
            % normal stiffness
            if ul(2) <= 0,  Kn = Knob(i);
            else            Kn = knrb(i) + kappa / (ul(2) + Ab(i));
            end
            
            % normal stress
            sig_n = Kn*ul(2);
            
            % tangential stiffness
            Fmc = max(cohb(i) - sig_n*phib(i), 0);
            Kt = ktrb(i) + Fmc / (abs(ul(1)) + a);
            
            % local force
            floc = [Kt*ul(1)*lb(i); sig_n*lb(i)];
            
            % global internal force
            dload(ii:iii) = dload(ii:iii) + T'*floc;
            
        end
        
        % approximation error
        eps_a = norm(idisp) / norm(gdisp);
        
    end
    
    if iter < niter,    converge = 1;   end
    
    if converge
        fact0 = factor;
        gdisp0 = gdisp;
        dload0 = dload;
        dfact = dfact/2;
        niter = max(round(niter/2), 200);
    else
        dfact = dfact/2;
    end
    
    if fact0 == 1,  break;  end
    
end


% GLOBAL AND LOCAL FACTORS OF SAFETY
%{
The global factor of safety is based on a ratio between the sum of
available resisting shear forces (from Coulomb failure function) and the
mobilized shear forces (from local sliding at the base) along the slip
surface. The local factor of safety for a given slice is simply the ratio
of available shear resistance to the mobilized shear stress at the base.

sumR    sum of resisting forces, sum((c - sig_n*tan(phi))*lb)
sumM    sum of mobilized forces, sum(sig_t*lb)
%}
Floc = zeros(1,n);
sumR = 0;
sumM = 0;
for i = 1:n
    
    ii = 2*i-1;     iii = ii+1;
    
    % local displacements
    ug = gdisp(ii:iii);
    T = [cosa(i) sina(i); -sina(i) cosa(i)];
    ul = T*ug;

    % normal stiffness
    if ul(2) <= 0,  Kn = Knob(i);
    else            Kn = knrb(i) + kappa / (ul(2) + Ab(i));
    end

    % normal stress
    sig_n = Kn*ul(2);

    % tangential stiffness
    Fmc = max(cohb(i) - sig_n*phib(i), 0);
    Kt = ktrb(i) + Fmc / (abs(ul(1)) + a);

    % shear stress
    sig_t = Kt*ul(1);
    
    % local and global factors of safety
    Floc(i) = Fmc / sig_t;
    sumR = sumR + Fmc*lb(i);
    sumM = sumM + sig_t*lb(i);
    
end
F = sumR/sumM;

F = F*factor;
Floc = Floc*factor;


% flip sign for left-to-right slopes
if ~ltor
    F = -F;
    Floc = -Floc;
end

% throw out low and non-converging values due to inadmissible surfaces
if F < 0.5
    F = 1000;
    Floc = 1000*ones(1,n);
    dx = [];
    dy = [];
    Nint = [];
    Tint = [];
    return;
end


% GLOBAL DISPLACEMENT RETURN VALUES
%{
These are extracted from the converged global displacement vector.
%}
dx = gdisp(1:2:nnet-1,:)';
dy = gdisp(2:2:nnet,:)';


% INTERSLICE FORCES
%{
These are computed as in the solver and are useful for comparison with
assumptions made in other slope stability techniques such as the
Morgenstern-Price method.
%}

Nint = zeros(1,nint);
Tint = zeros(1,nint);

for i = 1:nint
    
    ii = 2*i-1;     iii = ii+3;

    % local displacements
    u = gdisp(ii:iii);
    du = u(3:4)-u(1:2);

    % normal stiffness
    if du(1) <= 0,  Kn = Knoi(i);
    else            Kn = knri(i) + kappa / (du(1)+Ai(i));
    end

    % normal stress
    sig_n = Kn*du(1);

    % tangential stiffness
    Fmc = max(cohi(i) - sig_n*phii(i), 0);
    Kt = ktri(i) + Fmc / (abs(du(2)) + a);

    % shear stress
    sig_t = Kt*du(2);
    
    % interslice forces
    Nint(i) = -hi(i)*sig_n;
    Tint(i) = hi(i)*sig_t;
    
end

end

