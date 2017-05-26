function [ F, Floc, dx, dy, Nint, Tint ] = ...
    RFEMSolver(	slip, params_layers, params_piez,...
    params_soln, params_load)
% 
% Slope Stability Analysis Program
% RFEMSolver.m
%
% 20 August 2015
% 
%  - For a description of the theories and equations the module is built on
%  refer to the SRS. Specifially sections with blue or brass coloured table
%  headings.
%  (../Documentation Files/SRS_SSP.pdf)
%
%  - For a description of the module interface refer to the MIS.
% (../Documentation Files/MIS_SSP.pdf)
%
%  - For a description of the module secrets and services refer to the MG.
% (../Documentation Files/MG_SSP.pdf)
%
%  ---------------------
%
% Details of this technique are presented in:
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

ltor = params_soln.ltor; % soln method extraction


% get count of slices, layers, and vertices (for both piez and strat)
n = size(slip,2) - 1;
nint = n-1;
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

% compute base width of slices
xn = 0.5 * (slip(1,2:n+1)+slip(1,1:n)); % midpoint positions
bb = slip(1,2:n+1) - slip(1,1:n); % base x width
bi = xn(2:n) - xn(1:nint); % distance between midpoints
lb = sqrt((slip(1,2:n+1)-slip(1,1:n)).^2 + (slip(2,2:n+1)-slip(2,1:n)).^2); % total base length

% COMPUTE SLIP FORCE/ANGLE/SOIL PROPERTIES
%{
Compute using the PropertySorter.m module. Recieve the params_soilBase,
params_soilInterior, params_angles, and params_internalForce structures 
from the property sorter algorithm, as outlined in the MIS. Identifies
forces between slices, angles of the base and top surfaces of slices, and 
the soil properties at base and interlice surface.
%} 
[params_internalForce,params_angles,...
    params_soilInterior,params_soilBase]=...
    PropertySorter(slip, params_layers, params_piez);

alpha = params_angles.Alpha; % angle extraction
beta = params_angles.Beta;

phib = tan(params_soilBase.phi_B); % soil base extraction
cohb = params_soilBase.coh_B;
Eb = params_soilBase.E_B;
nub = params_soilBase.nu_B;

hi = params_soilInterior.hi; % soil base extraction
phii = tan(params_soilInterior.phi_IS); 
cohi = params_soilInterior.coh_IS;
Ei = params_soilInterior.E_IS;
nui = params_soilInterior.nu_IS;

Ub = params_internalForce.Ub; % internal force extraction
Ut = params_internalForce.Ut;
W = params_internalForce.W;
H = params_internalForce.H;


% base(a) and surface(b) angle definitions
sina = sin(alpha);
cosa = cos(alpha);
sinb = sin(beta);
cosb = cos(beta);


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

