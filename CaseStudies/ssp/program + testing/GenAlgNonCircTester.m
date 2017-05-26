% -------------------------------------------------------------------------
% GenAlgNonCircTester.m
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
% Description:
%
% This script file tests the function GenAlgNonCirc.m for a number of test
% cases.
% -------------------------------------------------------------------------

clear;  clc;

%%
% -------------------------------------------------------------------------
% EXAMPLE 1 [Greco (1996), Malkawi et al (2001), Cheng et al (2007),
%                   Li et al (2010)]
% -------------------------------------------------------------------------

% reported values of Fs
F1_grec = 1.327;
F1_malk = 1.238;
F1_cheng = 1.325;
F1_li = 1.327;

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
phi = phi*(pi/180);

Q = [];
omega = [];
Kc = 0;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end

%% pass on to GenAlgNonCirc
Xetr = [0,10];
Xext = [15,20];
Ylim = [0,12];
cncvu = 1;
obtu = 1;
evnslc = 1;
ftype = 0;
[F1,cslip1,slips1,gens1,Fgens1,nof1,rt1] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);

%%
% plot stratigraphy and slip surfaces
figure;
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    plot(strat{i}(1,:),strat{i}(2,:),'-k');
end
nslip = size(slips1,1);
for i = 1:nslip
    plot(slips1{i,1}(1,:),slips1{i,1}(2,:),'-or');
end
plot(cslip1(1,:),cslip1(2,:),'--k');
text(2,10,sprintf('F = %7.4f',F1));
xlabel('x (m)','fontweight','bold');
ylabel('y (m)','fontweight','bold');
axis equal;
hold off;
title('Ex 1 - Greco (1996), Malkawi et al (2001), Cheng et al (2007), Li et al (2010)', 'fontweight','bold');

figure;
plot(gens1,Fgens1);
xlabel('Number of generations','fontweight','bold');
ylabel('F_{min}','fontweight','bold');
title('Ex 1 - Greco (1996), Malkawi et al (2001), Cheng et al (2007), Li et al (2010)', 'fontweight','bold');

fprintf('\n');
fprintf('Example 1\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Current                   %7.4f               %7.4f\n', F1, rt1);
fprintf('Greco (1996)              %7.4f     %7.4f\n', F1_grec, abs(F1-F1_grec)/F1_grec*100);
fprintf('Malkawi et al (2001)      %7.4f     %7.4f\n', F1_malk, abs(F1-F1_malk)/F1_malk*100);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F1_cheng, abs(F1-F1_cheng)/F1_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f\n', F1_li, abs(F1-F1_li)/F1_li*100);


%%
% -------------------------------------------------------------------------
% EXAMPLE 2 [Zolfaghari et al (2005), Cheng et al (2007), Li et al (2010)]
% -------------------------------------------------------------------------

% reported values of Fs
F2_zolf = 1.240;
F2_cheng = 1.101;
F2_li = 1.113;

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
phi = phi*pi/180;

Q = [];
omega = [];
Kc = 0;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end

%% pass on to GenAlgNonCirc
Xetr = [10,15];
Xext = [27,34];
Ylim = [40,52];
cncvu = 1;
obtu = 1;
evnslc = 1;
ftype = 0;
[F2,cslip2,slips2,gens2,Fgens2,nof2,rt2] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);

%%
% plot stratigraphy and slip surfaces
figure;
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    plot(strat{i}(1,:),strat{i}(2,:),'-k');
end
nslip = size(slips2,1);
for i = 1:nslip
    plot(slips2{i,1}(1,:),slips2{i,1}(2,:),'-or');
end
plot(cslip2(1,:),cslip2(2,:),'--k');
text(27,50,sprintf('F = %7.4f',F2));
xlabel('x (m)','fontweight','bold');
ylabel('y (m)','fontweight','bold');
title('Ex 2 - Zolfaghari et al (2005), Cheng et al (2007), Li et al (2010)', 'fontweight','bold');
axis equal;
hold off;

figure;
plot(gens2,Fgens2);
xlabel('Number of generations','fontweight','bold');
ylabel('F_{min}','fontweight','bold');
title('Ex 2 - Zolfaghari et al (2005), Cheng et al (2007), Li et al (2010)', 'fontweight','bold');

fprintf('\n');
fprintf('Example 2\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Current                   %7.4f               %7.4f\n', F2, rt2);
fprintf('Zolfaghari et al (2005)   %7.4f     %7.4f\n', F2_zolf, abs(F2-F2_zolf)/F2_zolf*100);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F2_cheng, abs(F2-F2_cheng)/F2_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f\n', F2_li, abs(F2-F2_li)/F2_li*100);

%%
% -------------------------------------------------------------------------
% EXAMPLE 3 [Zolfaghari et al (2005), Cheng et al (2007), Li et al (2010)]
% -------------------------------------------------------------------------

% reported values of Fs
F3_zolf = 1.480;
F3_cheng = 1.349;
F3_li = 1.335;

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('../data files/Ex3.dat');

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
phi = phi*pi/180;

Q = [];
omega = [];
Kc = 0;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end

%% pass on to GenAlgNonCirc
Xetr = [10,15];
Xext = [23,30];
Ylim = [40,52];
cncvu = 1;
obtu = 1;
evnslc = 1;
ftype = 0;
[F3,cslip3,slips3,gens3,Fgens3,nof3,rt3] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);

%%
% plot stratigraphy and slip surfaces
figure;
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    plot(strat{i}(1,:),strat{i}(2,:),'-k');
end
nslip = size(slips3,1);
for i = 1:nslip
    plot(slips3{i,1}(1,:),slips3{i,1}(2,:),'-or');
end
plot(cslip3(1,:),cslip3(2,:),'--k');
text(27,50,sprintf('F = %7.4f',F3));
xlabel('x (m)','fontweight','bold');
ylabel('y (m)','fontweight','bold');
title('Ex 3 - Zolfaghari et al (2005), Cheng et al (2007), Li et al (2010)', 'fontweight','bold');
axis equal;
hold off;

figure;
plot(gens3,Fgens3);
xlabel('Number of generations','fontweight','bold');
ylabel('F_{min}','fontweight','bold');
title('Ex 3 - Zolfaghari et al (2005), Cheng et al (2007), Li et al (2010)', 'fontweight','bold');

fprintf('\n');
fprintf('Example 3\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Current                   %7.4f               %7.4f\n', F3, rt3);
fprintf('Zolfaghari et al (2005)   %7.4f     %7.4f\n', F3_zolf, abs(F3-F3_zolf)/F3_zolf*100);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F3_cheng, abs(F3-F3_cheng)/F3_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f\n', F3_li, abs(F3-F3_li)/F3_li*100);


%%
% -------------------------------------------------------------------------
% EXAMPLE 4 [Cheng et al (2007), Li et al (2010)]
% -------------------------------------------------------------------------

% reported values of Fs
F4_cheng = 1.184;
F4_li = 1.197;

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('../data files/Ex4.dat');

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

Q = [];
omega = [];
Kc = 0;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end

%% pass on to GenAlgNonCirc
Xetr = [10,15];
Xext = [30,35];
Ylim = [40,52];
cncvu = 0;
obtu = 1;
evnslc = 1;
ftype = 0;
[F4,cslip4,slips4,gens4,Fgens4,nof4,rt4] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);

%%
% plot stratigraphy and slip surfaces
figure;
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    plot(strat{i}(1,:),strat{i}(2,:),'-k');
end
nslip = size(slips4,1);
for i = 1:nslip
    plot(slips4{i,1}(1,:),slips4{i,1}(2,:),'-or');
end
plot(cslip4(1,:),cslip4(2,:),'--k');
text(27,50,sprintf('F = %7.4f',F4));
xlabel('x (m)','fontweight','bold');
ylabel('y (m)','fontweight','bold');
title('Ex 4 - Cheng et al (2007), Li et al (2010)', 'fontweight','bold');
axis equal;
hold off;

figure;
plot(gens4,Fgens4);
xlabel('Number of generations','fontweight','bold');
ylabel('F_{min}','fontweight','bold');
title('Ex 4 - Cheng et al (2007), Li et al (2010)', 'fontweight','bold');

fprintf('\n');
fprintf('Example 4\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Current                   %7.4f               %7.4f\n', F4, rt4);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F4_cheng, abs(F4-F4_cheng)/F4_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f\n', F4_li, abs(F4-F4_li)/F4_li*100);


%%
% -------------------------------------------------------------------------
% EXAMPLE 5 [Pham and Fredlund (2003), Li et al (2010)]
% -------------------------------------------------------------------------

% reported values of Fs
F5_pham = 1.413;
F5_li = 1.408;

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('../data files/Ex5.dat');

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
phi = phi*pi/180;

Q = [];
omega = [];
Kc = 0;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end

%% pass on to GenAlgNonCirc
Xetr = [10,20];
Xext = [35,50];
Ylim = [7,30];
cncvu = 1;
obtu = 1;
evnslc = 1;
ftype = 0;
[F5,cslip5,slips5,gens5,Fgens5,nof5,rt5] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);

%%
% plot stratigraphy and slip surfaces
figure;
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    plot(strat{i}(1,:),strat{i}(2,:),'-k');
end
nslip = size(slips5,1);
for i = 1:nslip
    plot(slips5{i,1}(1,:),slips5{i,1}(2,:),'-or');
end
plot(cslip5(1,:),cslip5(2,:),'--k');
text(45,25,sprintf('F = %7.4f',F5));
xlabel('x (m)','fontweight','bold');
ylabel('y (m)','fontweight','bold');
title('Ex 5 - Pham and Fredlund (2003), Li et al (2010)', 'fontweight','bold');
axis equal;
hold off;

figure;
plot(gens5,Fgens5);
xlabel('Number of generations','fontweight','bold');
ylabel('F_{min}','fontweight','bold');
title('Ex 5 - Pham and Fredlund (2003), Li et al (2010)', 'fontweight','bold');

fprintf('\n');
fprintf('Example 5\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Current                   %7.4f               %7.4f\n', F5, rt5);
fprintf('Pham and Fredlund (2003)  %7.4f     %7.4f\n', F5_pham, abs(F5-F5_pham)/F5_pham*100);
fprintf('Li et al (2010)           %7.4f     %7.4f\n', F5_li, abs(F5-F5_li)/F5_li*100);


%%
% -------------------------------------------------------------------------
% EXAMPLE 6 [Pham and Fredlund (2003), Li et al (2010)]
% -------------------------------------------------------------------------

% reported values of Fs
F6_pham = 1.000;
F6_li = 1.017;

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
phi = phi*pi/180;

Q = [];
omega = [];
Kc = 0;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end

%% pass on to GenAlgNonCirc
Xetr = [10,20];
Xext = [40,55];
Ylim = [11,30];
cncvu = 1;
obtu = 1;
evnslc = 1;
ftype = 0;
[F6,cslip6,slips6,gens6,Fgens6,nof6,rt6] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);

%%
% plot stratigraphy and slip surfaces
figure;
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    plot(strat{i}(1,:),strat{i}(2,:),'-k');
end
nslip = size(slips6,1);
for i = 1:nslip
    plot(slips6{i,1}(1,:),slips6{i,1}(2,:),'-or');
end
plot(cslip6(1,:),cslip6(2,:),'--k');
text(45,25,sprintf('F = %7.4f',F6));
xlabel('x (m)','fontweight','bold');
ylabel('y (m)','fontweight','bold');
title('Ex 6 - Pham and Fredlund (2003), Li et al (2010)', 'fontweight','bold');
axis equal;
hold off;

figure;
plot(gens6,Fgens6);
xlabel('Number of generations','fontweight','bold');
ylabel('F_{min}','fontweight','bold');
title('Ex 6 - Pham and Fredlund (2003), Li et al (2010)', 'fontweight','bold');

fprintf('\n');
fprintf('Example 6\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Current                   %7.4f               %7.4f\n', F6, rt6);
fprintf('Pham and Fredlund (2003)  %7.4f     %7.4f\n', F6_pham, abs(F6-F6_pham)/F6_pham*100);
fprintf('Li et al (2010)           %7.4f     %7.4f\n', F6_li, abs(F6-F6_li)/F6_li*100);


%%
% -------------------------------------------------------------------------
% Example 7 - Fredlund and Krahn (1977)
% -------------------------------------------------------------------------

% reported values of Fs
F7_fred = 1.245;

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread('../data files/FredlundKrahn1977.dat');

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
    gams(ilayer) = data(i,4);
    
    i = i+1;
    
    strat{ilayer} = data(i:i+npts-1,1:2)';   % read strat geom
    
    i = i+npts;
    
end
phi = phi.*pi./180;

Q = [];
omega = [];
Kc = 0;

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end

%% pass on to GenAlgNonCirc
Xetr = [10,20];
Xext = [40,48];
Ylim = [4,20];
cncvu = 1;
obtu = 1;
evnslc = 1;
ftype = 0;
[F7,cslip7,slips7,gens7,Fgens7,nof7,rt7] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);

%%
% plot stratigraphy and slip surfaces
figure;
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    plot(strat{i}(1,:),strat{i}(2,:),'-k');
end
nslip = size(slips7,1);
for i = 1:nslip
    plot(slips7{i,1}(1,:),slips7{i,1}(2,:),'-or');
end
plot(cslip7(1,:),cslip7(2,:),'--k');
text(40,18,sprintf('F = %7.4f',F7));
xlabel('x (m)','fontweight','bold');
ylabel('y (m)','fontweight','bold');
title('Ex 7 - Fredlund and Krahn (1977)', 'fontweight','bold');
axis equal;
hold off;

figure;
plot(gens7,Fgens7);
xlabel('Number of generations','fontweight','bold');
ylabel('F_{min}','fontweight','bold');
title('Ex 7 - Fredlund and Krahn (1977)', 'fontweight','bold');

fprintf('\n');
fprintf('Example 7\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Current                   %7.4f               %7.4f\n', F7, rt7);
fprintf('Fredlund and Krahn (1977) %7.4f     %7.4f\n', F7_fred, abs(F7-F7_fred)/F7_fred*100);

