% -------------------------------------------------------------------------
% RFEMTester.m
% 5 January 2011
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
% Description:
%
% This script file tests the function RFEM.m to verify that it
% evaluates to the correct Fs for a number of test cases.
% -------------------------------------------------------------------------


%%
% -------------------------------------------------------------------------
% EXAMPLE 1 [Greco (1996), Malkawi et al (2001), Cheng et al (2007),
%                   Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('../data files/Ex1.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight
E = zeros(nlayer,1);        % elastic modulus
nu = zeros(nlayer,1);       % Poisson's ratio

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    E(ilayer) = data(i,6);
    nu(ilayer) = data(i,7);
    
    i = i+1;
    
    strat{ilayer} = data(i:i+npts-1,1:2)';   % read strat geom
    
    i = i+npts;
    
end
phi = phi.*pi./180;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
else
    
    piez = [];
    gamw = 0;
    
end

% read in slip surfaces
data = dlmread('../data files/Ex1_RFEM_4slice.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[F1_4, Floc1_4, dx1_4, dy1_4, Nint1_4, Tint1_4] = ...
    RFEM(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,E,nu);
rt1_4 = cputime - rt;


% read in slip surfaces
data = dlmread('../data files/Ex1_RFEM_30slice.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[F1_30, Floc1_30, dx1_30, dy1_30, Nint1_30, Tint1_30] = ...
    RFEM(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,E,nu);
rt1_30 = cputime - rt;


%%
% -------------------------------------------------------------------------
% EXAMPLE 2 [Zolfaghari et al (2005), Cheng et al (2007), Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('../data files/Ex2.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight
E = zeros(nlayer,1);        % elastic modulus
nu = zeros(nlayer,1);       % Poisson's ratio

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    E(ilayer) = data(i,6);
    nu(ilayer) = data(i,7);
    
    i = i+1;
    
    strat{ilayer} = data(i:i+npts-1,1:2)';   % read strat geom
    
    i = i+npts;
    
end
phi = phi.*pi./180;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
else
    
    piez = [];
    gamw = 0;
    
end


% read in slip surfaces
data = dlmread('../data files/Ex2_RFEM.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[F2, Floc2, dx2, dy2, Nint2, Tint2] = ...
    RFEM(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,E,nu);
rt2 = cputime - rt;


%%
% -------------------------------------------------------------------------
% EXAMPLE 6 [Pham and Fredlund (2003), Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('../data files/Ex6.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight
E = zeros(nlayer,1);        % elastic modulus
nu = zeros(nlayer,1);       % Poisson's ratio

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    E(ilayer) = data(i,6);
    nu(ilayer) = data(i,7);
    
    i = i+1;
    
    strat{ilayer} = data(i:i+npts-1,1:2)';   % read strat geom
    
    i = i+npts;
    
end
phi = phi.*pi./180;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
else
    
    piez = [];
    gamw = 0;
    
end


% read in slip surfaces
data = dlmread('../data files/Ex6_RFEM.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[F6, Floc6, dx6, dy6, Nint6, Tint6] = ...
    RFEM(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,E,nu);
rt6 = cputime - rt;


%%
% -------------------------------------------------------------------------
% PRINT RESULTS
% -------------------------------------------------------------------------

fprintf('\n');
fprintf('\n');
fprintf('Case                 Fs,MP        Fs     err,Fs      time\n');
fprintf('Ex1 (4 slice)      %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.319, F1_4, abs(F1_4-1.319)/1.319*100, rt1_4);
fprintf('Ex1 (30 slice)     %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.319, F1_30, abs(F1_30-1.319)/1.319*100, rt1_30);
fprintf('Ex2                %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.113, F2, abs(F2-1.113)/1.113*100, rt2);
fprintf('Ex6                %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.064, F6, abs(F6-1.064)/1.064*100, rt6);