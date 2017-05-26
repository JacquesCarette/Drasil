function [ F, lam, Nint, Tint, thrust, piezthrust, lamArray ] = ...
    MorgPriceSolver(slip, params_layers, params_piez,...
    params_soln, params_load)
% 
% Slope Stability Analysis Program
% MorgPriceSolver.m
%
% 20 August 2015
% 
%  - For a description of the theories and equations the module is built on
%  refer to the SRS. Specifially models with purple or brass coloured table
%  headings.
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
% The evaluation is based on the algorithm presented in:
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
% ---------------------
% Notes:
% ---------------------
% 
% 1. For slices where the base crosses a bedding plane, it is assumed that
%       only one bedding plane is crossed at a time in order to compute the
%       weighted average properties for that slice. Therefore, one should
%       ensure that this is the case before calling this function otherwise
%       the slice properties may not be modelled accurately.
% 2. It is assumed that the surface provided to this function is
%       kinematically admissible. Consequently, one should verify that this
%       is indeed the case prior to calling the function using the KinAdm.m 
%       module otherwise the results may be spurious.
%
% -------------------------------------------------------------------------

Kc = params_load.Kc; % load forces extraction
Q = params_load.Q;
omega = params_load.omega;

nlayer = length(params_layers); % strat extraction
strat = cell(nlayer,1);
for i = 1:nlayer
    strat{i} = params_layers(i).strat;
end

piez = params_piez.piez; % piez extraction

ftype = params_soln.ftype; % soln method extraction
ltor = params_soln.ltor;

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
% compute angle parameters for applied loads
sinw = sin(omega);
cosw = cos(omega);

b = slip(1,2:n+1) - slip(1,1:n);

% COMPUTE SLIP FORCE/ANGLE/SOIL PROPERTIES
%{
Compute using the PropertySorter.m module. Recieve the params_soilBase,
params_angles, and params_internalForce structures from the property sorter
algorithm, as outlined in the MIS. Identifies forces between slices, angles 
of the base and top surfaces of slices, and the soil properties at the base 
surface. Morgenstern price analysis only needs soil base properties, not 
soil interslice properties.
%}
[params_internalForce, params_angles,...
    ~,params_soilBase, hw, h]=...
    PropertySorter(slip,params_layers,params_piez);

alpha = params_angles.Alpha; % angle extraction
beta = params_angles.Beta;

phi = tan(params_soilBase.phi_B); % soil base extraction
coh = params_soilBase.coh_B;

Ub = params_internalForce.Ub; % internal force extraction
Ut = params_internalForce.Ut;
W = params_internalForce.W;
H = params_internalForce.H;


% base(a) and surface(b) angle definitions
tana = tan(alpha);
sina = sin(alpha);
cosa = cos(alpha);
seca = sec(alpha);
sinb = sin(beta);
cosb = cos(beta);

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
    
    % Individual Lambdas
    
    lamArray=zeros(1,n);
    lamArray(1)=(b(1)*(E(1)+H(1))*tana(1))/(b(1)*f(1)*E(1));
    for i=2:nint
        lamArray(i)=b(i)*(E(i)+ E(i-1) + H(i) + H(i-1))*tana(i) ...
                    +h(i).*(Kc*W(i) - 2*Ut(i)*sinb(i) - 2*Q(i)*sinw(i));
    end
    lamArray(n)=(b(n)*(E(nint)+H(nint))*tana(n))/(b(n)*f(nint)*E(nint));
    
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