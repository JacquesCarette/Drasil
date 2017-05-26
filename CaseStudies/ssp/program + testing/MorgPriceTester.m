% -------------------------------------------------------------------------
% MorgPriceTester.m
% 8 October 2011
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
% This script file tests the function MorgPrice.m to verify that it
% evaluates to the correct Fs for a number of test cases.
% -------------------------------------------------------------------------

%%
% -------------------------------------------------------------------------
% Fredlund and Krahn (1977)
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/FredlundKrahn1977.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    
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
    
end

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/FredlundKrahn1977_circ.surf')';
slipc = data(1:2,:);
Kcc = data(3,1);
n = size(data,2);
Qc = data(3,2:n);
omegac = data(4,2:n);
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/FredlundKrahn1977_noncirc.surf')';
slipnc = data(1:2,:);
Kcnc = data(3,1);
n = size(data,2);
Qnc = data(3,2:n);
omeganc = data(4,2:n);


% compute F and lam for circular surface, dry slope
rt = cputime;
[Fcd, lamcd, Nintcd, Tintcd, thrustcd] = ...
    MorgPrice(slipc,strat,phi,coh,gam,gams,[],gamw,Qc,omegac,Kcc,ltor,0);
rtcd = cputime - rt;
rt = cputime;
[Fcw, lamcw, Nintcw, Tintcw, thrustcw, piezthrustcw] = ...
    MorgPrice(slipc,strat,phi,coh,gam,gams,piez,gamw,Qc,omegac,Kcc,ltor,0);
rtcw = cputime - rt;
rt = cputime;
[Fncd, lamncd, Nintncd, Tintncd, thrustncd] = ...
    MorgPrice(slipnc,strat,phi,coh,gam,gams,[],gamw,Qnc,omeganc,Kcnc,ltor,0);
rtncd = cputime - rt;
rt = cputime;
[Fncw, lamncw, Nintncw, Tintncw, thrustncw, piezthrustncw] = ...
    MorgPrice(slipnc,strat,phi,coh,gam,gams,piez,gamw,Qnc,omeganc,Kcnc,ltor,0);
rtncw = cputime - rt;

% %%
% % plot stratigraphy and slip surfaces
% figure;
% nlayer = size(strat,1);
% if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
% else plot(strat{1}(1,1),strat{1}(2,1));
% end
% hold on;
% for i = 1:nlayer
%     plot(strat{i}(1,:),strat{i}(2,:),'-k');
% end
% plot(slipnc(1,:),slipnc(2,:),'-or');
% plot(slipnc(1,:),thrustncw(:),'--*k');
% plot(slipnc(1,:),piezthrustncw(:),'--*b');
% text(35,18,sprintf('F = %g',Fncw));
% xlabel('x (m)','fontweight','demi');
% ylabel('y (m)','fontweight','demi');
% axis equal;
% hold off;


%%
% -------------------------------------------------------------------------
% EXAMPLE 1 [Greco (1996), Malkawi et al (2001), Cheng et al (2007),
%                   Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex1.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    
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
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex1_Greco1996.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fgrec1, lamgrec1] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtgrec1 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex1_MalkawiEtAl2001.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fmal1, lammal1] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtmal1 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex1_ChengEtAl2007.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fchg1, lamchg1] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtchg1 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex1_LiEtAl2010.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);


% compute F and lam for slip surface
rt = cputime;
[Fli1, lamli1, Nintli1, Tintli1] = ...
    MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,1);
rtli1 = cputime - rt;



%%
% -------------------------------------------------------------------------
% EXAMPLE 2 [Zolfaghari et al (2005), Sarma and Tan (2006),
%               Cheng et al (2007), Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex2.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    
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
    
end

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex2_ZolfaghariEtAl2005.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fzol2, lamzol2] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtzol2 = cputime - rt;

% compute F and lam for slip surface
rt = cputime;
[Fst2, lamst2] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtst2 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex2_ChengEtAl2007.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fchg2, lamchg2] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtchg2 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex2_LiEtAl2010.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fli2, lamli2, Nintli2, Tintli2] = ...
    MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtli2 = cputime - rt;

% %%
% % plot stratigraphy and slip surfaces
% figure;
% nlayer = size(strat,1);
% if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
% else plot(strat{1}(1,1),strat{1}(2,1));
% end
% hold on;
% for i = 1:nlayer
%     plot(strat{i}(1,:),strat{i}(2,:),'-k');
% end
% plot(slip(1,:),slip(2,:),'-or');
% plot(slip(1,:),thrust(:),'--*k');
% text(25,50,sprintf('F = %g',Fli2));
% xlabel('x (m)','fontweight','demi');
% ylabel('y (m)','fontweight','demi');
% axis equal;
% hold off;

%%
% -------------------------------------------------------------------------
% EXAMPLE 3 [Zolfaghari et al (2005), Sarma and Tan (2006),
%               Cheng et al (2007), Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex3.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    
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
    
end

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex3_ZolfaghariEtAl2005.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fzol3, lamzol3] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtzol3 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex3_ChengEtAl2007.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fchg3, lamchg3] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtchg3 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex3_LiEtAl2010.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fli3, lamli3] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtli3 = cputime - rt;

%%
% -------------------------------------------------------------------------
% EXAMPLE 4 [Cheng et al (2007), Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex4.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    
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
    
end

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex4_ChengEtAl2007.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fchg4, lamchg4] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtchg4 = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex4_LiEtAl2010.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);

% compute F and lam for slip surface
rt = cputime;
[Fli4, lamli4] = MorgPrice(slip,strat,phi,coh,gam,gams,[],gamw,Q,omega,Kc,ltor,0);
rtli4 = cputime - rt;

%%
% -------------------------------------------------------------------------
% EXAMPLE 5 [Pham and Fredlund (2003), Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex5.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    
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
    
end

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex5_PhamFredlund2003.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);


% compute F and lam for slip surface
rt = cputime;
[Fpf5dyn, lampf5dyn] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtpf5dyn = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex5_PhamFredlund2003_circ.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);


% compute F and lam for slip surface
rt = cputime;
[Fpf5cir, lampf5cir] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtpf5cir = cputime - rt;


% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex5_LiEtAl2010.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);


% compute F and lam for slip surface
rt = cputime;
[Fli5, lamli5] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtli5 = cputime - rt;

%%
% -------------------------------------------------------------------------
% EXAMPLE 6 [Pham and Fredlund (2003), Li et al (2010)]
% -------------------------------------------------------------------------

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex6.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    
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
    
end

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex6_PhamFredlund2003.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);


% compute F and lam for slip surface
rt = cputime;
[Fpf6dyn, lampf6dyn] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtpf6dyn = cputime - rt;

% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex6_PhamFredlund2003_circ.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);


% compute F and lam for slip surface
rt = cputime;
[Fpf6cir, lampf6cir] = MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtpf6cir = cputime - rt;


% read in slip surfaces
data = dlmread('C:/Users/HenryFrankis/Desktop/SSP_ddd/data files/Ex6_LiEtAl2010.surf')';
slip = data(1:2,:);
Kc = data(3,1);
n = size(data,2);
Q = data(3,2:n);
omega = data(4,2:n);


% compute F and lam for slip surface
rt = cputime;
[Fli6, lamli6, Nintli6, Tintli6] = ...
    MorgPrice(slip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,0);
rtli6 = cputime - rt;


%%
% -------------------------------------------------------------------------
% PRINT RESULTS
% -------------------------------------------------------------------------

fprintf('\n');
fprintf('\n');
fprintf('Fredlund and Krahn (1977) / Zhu et al (2005)\n');
fprintf('Case     Fs,exp        Fs       lam    err,Fs       time\n');
fprintf('C/D     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 2.074, Fcd, lamcd, abs(Fcd-2.074)/2.074*100, rtcd);
fprintf('NC/D    %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.371, Fncd, lamncd, abs(Fncd-1.371)/1.371*100, rtncd);
fprintf('C/W     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.831, Fcw, lamcw, abs(Fcw-1.831)/1.831*100, rtcw);
fprintf('NC/W    %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.245, Fncw, lamncw, abs(Fncw-1.245)/1.245*100, rtncw);

fprintf('\n');
fprintf('Greco (1996)\n');
fprintf('Case     Fs,exp        Fs       lam    err,Fs       time\n');
fprintf('Ex1     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.327, Fgrec1, lamgrec1, abs(Fgrec1-1.327)/1.327*100, rtgrec1);

fprintf('\n');
fprintf('Malkawi et al (2001)\n');
fprintf('Case     Fs,exp        Fs       lam    err,Fs       time\n');
fprintf('Ex1     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.238, Fmal1, lammal1, abs(Fmal1-1.238)/1.238*100, rtmal1);

fprintf('\n');
fprintf('Pham and Fredlund (2003)\n');
fprintf('Case       Fs,exp        Fs       lam    err,Fs       time\n');
fprintf('Ex5 (DYN) %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.413, Fpf5dyn, lampf5dyn, abs(Fpf5dyn-1.413)/1.413*100, rtpf5dyn);
fprintf('Ex5 (cir) %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.485, Fpf5cir, lampf5cir, abs(Fpf5cir-1.485)/1.485*100, rtpf5cir);
fprintf('Ex6 (DYN) %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.000, Fpf6dyn, lampf6dyn, abs(Fpf6dyn-1.000)/1.000*100, rtpf6dyn);
fprintf('Ex6 (cir) %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.140, Fpf6cir, lampf6cir, abs(Fpf6cir-1.140)/1.140*100, rtpf6cir);

fprintf('\n');
fprintf('Zolfaghari et al (2005)\n');
fprintf('Case     Fs,exp        Fs       lam    err,Fs       time\n');
fprintf('Ex2     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.240, Fzol2, lamzol2, abs(Fzol2-1.240)/1.240*100, rtzol2);
fprintf('Ex3     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.480, Fzol3, lamzol3, abs(Fzol3-1.480)/1.480*100, rtzol3);

fprintf('\n');
fprintf('Cheng et al (2007)\n');
fprintf('Case     Fs,exp        Fs       lam    err,Fs       time\n');
fprintf('Ex1     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.325, Fchg1, lamchg1, abs(Fchg1-1.325)/1.325*100, rtchg1);
fprintf('Ex2     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.101, Fchg2, lamchg2, abs(Fchg2-1.101)/1.101*100, rtchg2);
fprintf('Ex3     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.349, Fchg3, lamchg3, abs(Fchg3-1.349)/1.349*100, rtchg3);
fprintf('Ex4     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.184, Fchg4, lamchg4, abs(Fchg4-1.184)/1.184*100, rtchg4);

fprintf('\n');
fprintf('Li et al (2010)\n');
fprintf('Case     Fs,exp        Fs       lam    err,Fs       time\n');
fprintf('Ex1     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.327, Fli1, lamli1, abs(Fli1-1.327)/1.327*100, rtli1);
fprintf('Ex2     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.113, Fli2, lamli2, abs(Fli2-1.113)/1.113*100, rtli2);
fprintf('Ex3     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.335, Fli3, lamli3, abs(Fli3-1.335)/1.335*100, rtli3);
fprintf('Ex4     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.197, Fli4, lamli4, abs(Fli4-1.197)/1.197*100, rtli4);
fprintf('Ex5     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.408, Fli5, lamli5, abs(Fli5-1.408)/1.408*100, rtli5);
fprintf('Ex6     %7.4f   %7.4f   %7.4f   %7.4f%%   %7.4f\n', 1.017, Fli6, lamli6, abs(Fli6-1.017)/1.017*100, rtli6);