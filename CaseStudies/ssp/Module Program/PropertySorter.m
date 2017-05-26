function [params_internalForce, params_angles,...
    params_soilInterior, params_soilBase, hw, h]=...
    PropertySorter(slip, params_layers, params_piez)

% 
% Slope Stability Analysis Program
% PropertySorter.m
%
% 20 August 2015
% 
%  - For a description of the theories and equations the module is built on
%  refer to the SRS. Specifically DD1 to DD8.
%  (../Documentation Files/SRS_SSP.pdf)
%
%  - For a description of the module interface refer to the MIS.
% (../Documentation Files/MIS_SSP.pdf)
%
%  - For a description of the module secrets and services refer to the MG.
% (../Documentation Files/MG_SSP.pdf)
%
% ---------------------
%
% Sorts and calculates: soil properties for slice bases and interslice 
% surface, The water and weight forces acting on the slices, and the angles 
% the basal and top surfaces of the slices make with the surface, necessary 
% to perform Morgenstern Price or RFEM analysis. 
%
% -------------------------------------------------------------------------

% COMPUTE SLIP FORCE/ANGLE/SOIL PROPERTIES
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

nlayer = length(params_layers); % strat extraction
strat = cell(nlayer,1);
for i = 1:nlayer
    strat{i} = params_layers(i).strat;
end

phi = params_layers.phi; % soil property extraction
coh = params_layers.coh;
gamly = params_layers.gam;
gamsly = params_layers.gams;
E = params_layers.E;
nu = params_layers.nu;

piez = params_piez.piez; % piez extraction
gamw = params_piez.gamw;

% Read lengths of data 
n = size(slip,2) - 1;
nint = n-1;
npz = size(piez,2);
nly = size(strat,1);
nply = zeros(nly,1);
for i = 1:nly
    nply(i) = size(strat{i},2);
end

hi= zeros(1,nint); %Initialize interlsice property vectors
phi_IS = zeros(1,nint); 
coh_IS = zeros(1,nint);
E_IS = zeros(1,nint);
nu_IS = zeros(1,nint);

phi_B = zeros(1,n); %Initialize base property vectors
coh_B = zeros(1,n);
E_B = zeros(1,n); 
nu_B = zeros(1,n);

ub = zeros(1,n+1); % Initialize slice and surface pore pressures
ut = zeros(1,n+1);

w = zeros(1,n+1); % Initialize slice weight

ypz = zeros(1,n+1); % Initialize layer, and piez vertex coordinates
yly = zeros(nly,n+1);

hw = zeros(1,nint); % Initialize interslice water force, and vector height
H = zeros(1,nint);

cly1 = 0; % for determining if base crosses bedding plane
cly2 = 0;

for i = 1:n+1   % loop through interfaces
    
    ipv=i-1;
    
    x = slip(1,i); y = slip(2,i);   % get coordinates at base of interface
    
    % compute y-coord of each strat layer
    for ily = nly:-1:1
        for iply = 1:nply(ily)-1

            x1 = strat{ily}(1,iply);    y1 = strat{ily}(2,iply);
            x2 = strat{ily}(1,iply+1);  y2 = strat{ily}(2,iply+1);

            if x >= x1 && x < x2    % check if coords of base lie in the domain of this line seg
                yly(ily,i) = y1 + (x - x1)*(y2 - y1)/(x2 - x1);    % compute y coord on strat layer
                break; % for iply=1:nply(ily)-1
            end % if x >= x1 && x < x2
        end % for iply=1:nply(ily)-1
    end % for ily = nly:-1:1
    
    if npz > 0  % check if there is a piezometric surface

        for ipz = 1:npz-1   % loop through piez surf line segs

            x1 = piez(1,ipz);   y1 = piez(2,ipz);       % get trailing coords of piez surf
            x2 = piez(1,ipz+1); y2 = piez(2,ipz+1);     % get leading coords of piez surf

            if x >= x1 && x < x2    % check if coords of base lie in the domain of this line seg
                ypz(i) = y1 + (x - x1)*(y2 - y1)/(x2 - x1);    % compute y coord on piez surf
                break; % for ipz = 1:npz-1
            end % if x >= x1 && x < x
        end % for ipz = 1:npz-1
        
                
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
            
        end % if/elseif , Piez below slice
    end % npz>0
    
    
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
            phi_IS(ipv) = phi_IS(ipv) + dh*(phi(ily)); %Initialize interlsice property vectors
            coh_IS(ipv) = coh_IS(ipv) + dh*(coh(ily));
            E_IS(ipv) = E_IS(ipv) + dh*(E(ily));
            nu_IS(ipv) = nu_IS(ipv) + dh*(nu(ily));
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
            
            % compute weight for trailing layer properties
            % weight for leading layer properties is (1-wt)
            wt=Weighting(slip,strat,x,y,ipv,nply,cly2);
            
            % linearly interpolate between layer properties
            phi_B(ipv) = wt*(phi(cly1)) + (1-wt)*(phi(cly2)); %Initialize interlsice property vectors
            coh_B(ipv) = wt*(coh(cly1)) + (1-wt)*(coh(cly2));
            E_B(ipv) = wt*(E(cly1)) + (1-wt)*(E(cly2)); 
            nu_B(ipv) = wt*(nu(cly1)) + (1-wt)*(nu(cly2));
            
        elseif cly1 > cly2  % if base is ascending a layer
            
            % compute weight for trailing layer properties
            % weight for leading layer properties is (1-wt)
            wt=Weighting(slip,strat,x,y,ipv,nply,cly1);
            
            % linearly interpolate between layer properties
            phi_B(ipv) = wt*(phi(cly1)) + (1-wt)*(phi(cly2)); %Initialize interlsice property vectors
            coh_B(ipv) = wt*(coh(cly1)) + (1-wt)*(coh(cly2));
            E_B(ipv) = wt*(E(cly1)) + (1-wt)*(E(cly2)); 
            nu_B(ipv) = wt*(nu(cly1)) + (1-wt)*(nu(cly2));
            
        else    % if strat layer has not changed, use current layer props
            
            phi_B(ipv) = phi(cly2); %Initialize interlsice property vectors
            coh_B(ipv) = coh(cly2);
            E_B(ipv) = E(cly2); 
            nu_B(ipv) = nu(cly2);
            
        end     % if cly2>cly1, elseif cly1>cly2, interpolation of slice properties
        
    end     % if cly1, compute slice properties
    
    cly1 = cly2;    % advance layer tracking variable
    
end

% Compute x width and height of slices %
b = slip(1,2:n+1) - slip(1,1:n);
h = 0.5*((yly(1,1:n)-slip(2,1:n)) + (yly(1,2:n+1)-slip(2,2:n+1)));

% Slice angles %
alpha = atan((slip(2,2:n+1) - slip(2,1:n)) ./ b);
beta = atan((yly(1,2:n+1) - yly(1,1:n)) ./ b);

Ub = 0.5*(ub(2:n+1)+ub(1:n)).*b.*sec(alpha);
W = 0.5*(w(2:n+1)+w(1:n)).*b;
Ut = 0.5*(ut(2:n+1)+ut(1:n)).*b.*sec(beta);

phi_IS = (phi_IS) ./ (hi);
coh_IS = (coh_IS) ./ (hi);
E_IS = (E_IS) ./ (hi);
nu_IS = (nu_IS) ./ (hi);


params_soilBase = struct('phi_B',phi_B,'coh_B',coh_B,...
    'E_B',E_B,'nu_B',nu_B);
params_soilInterior = struct('hi',hi,'phi_IS',phi_IS,'coh_IS',coh_IS,...
    'E_IS',E_IS,'nu_IS',nu_IS);
params_angles = struct('Alpha',alpha,'Beta',beta);
params_internalForce = struct('Ub',Ub,'Ut',Ut,'W',W,'H',H);

end


function [wt]=Weighting(slip,strat,x,y,ipv,nply,TrailLayer)

% get coords of base line segment
xsl1 = slip(1,ipv); ysl1 = slip(2,ipv);
xsl2 = x;           ysl2 = y;
            
% get coords of strat layer line seg that is being crossed
for iply = 1:nply(TrailLayer)-1
    xly1 = strat{TrailLayer}(1,iply);    yly1 = strat{TrailLayer}(2,iply);
    xly2 = strat{TrailLayer}(1,iply+1);  yly2 = strat{TrailLayer}(2,iply+1);
    
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
end