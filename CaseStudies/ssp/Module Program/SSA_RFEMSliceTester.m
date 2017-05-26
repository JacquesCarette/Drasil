function SSA_RFEMSliceTester

clear all; close all; 

%% 
% <latex>
% \section{RFEM Slice Tester} \label{sec:RFEMTests}
% Testing results obtained from the RFEMSolver.m program, a module in
% the SSA program. Factor of safety results from the program are
% compared to results from examples in slope stability papers, to judge the
% accuracy of the implemented algorithm. As seen mentioned in the
% Morgenstern Price solver testing (section \ref{sec:MPTests}), due to the
% imperfect nature of the comparisons, results are judged on a relative
% basis. Example numbers refer to the same slope/slip problem as
% analyzed in the Morgenstern Price tests. The comparisons from scientific 
% papers were performed using non RFEM solver algorithms, such as 
% Morgenstern Price or Spencer's method. Therefore the relative accuracy of
% the implemented Morgenstern Price algorithm may appear higher, this does
% not also suggest the Morgenstern Price solver is truly more accurate
% however. Ideally and as is seen the RFEM solver should converge to a
% solution similar to the comparison slip, as two different methods
% calculating similar answers suggest accuracy of both methods.
% </latex>
%

Q = []; omega = [];
nStar = 5:50;

%%
% <latex>
% \subsection{Example 1}
% Results compared to those from Greco (1996), Malwaki et al (2001), 
% Cheng et al(2007), and Li et al (2010). A graph of the relative error 
% between the factor of safety calculated by the program with the factor of 
% safety given in the papers is used to analyze the results. The results
% using the Morgenstern Price algorithm is also plotted. As the comparison
% was also performed using the Morgenstern Price algorithm this relative
% error is almost 0.
% </latex>


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex1.dat');
params_soln = struct('ltor',ltor,'ftype',0);

data = dlmread('../data files/Ex1_RFEM_30slice.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);


[E1] = SSA_SliceTesterAlgorithm (@RFEMSolver, slip, params_layers,... % compute F and lam for slip surface
    params_piez, params_soln, params_load, 1.319, nStar);

[E1MP] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers,... % compute F and lam for slip surface
    params_piez, params_soln, params_load, 1.319, nStar);

%{
strat = params_layers.strat;
phi = params_layers.phi;
coh = params_layers.coh;
gam = params_layers.gam;
gams = params_layers.gams;
E = params_layers.E;
nu = params_layers.nu;
    
Kc = 0;

piez = params_piez.piez;
gamw = params_piez.gamw;
    
[E1_Original] = SSA_Original_RFEMSliceTester (@RFEM, slip, strat,...
    phi, coh, gam, gams, piez, gamw, Kc,...
    ltor,E,nu, 1.319,nStar); % compute F and lam for circular surface, dry slope
%}
    
figure;
plot(nStar,E1,'r')
hold on;
plot(nStar,E1MP,'b')
title('Error : Example 1 - Greco, Malwaki, Li')
xlabel('# of slices')
ylabel('Error (%)')
legend( 'RFEM', 'Morg Price', 'Location', 'northeast')

%%
% <latex>
% The figure shows convergence to a consistent relative error of less than 
% 5\% after approximately 25 slices for the RFEM solver. This is a positive 
% result suggesting accuracy of both methods. However as seen in the 
% following figure the RFEM algorithm requires significantly more 
% computation time than the Morgenstern Price algorithm (section 
% \ref{sec:MPTests}). The large difference in calculation time makes the
% Morgenstern Price solver much more efficient, especially when performing 
% repeated analysis in the Genetic Algorithm search. 
% </latex>

nTest1=[50,100,500]; 
[~, ~, T1_Long]=...
    SSA_SliceTesterAlgorithm (@RFEMSolver, slip, params_layers,...
    params_piez, params_soln, params_load, 1.319, nTest1);

[~, ~, T1MP_Long]=...
    SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers,...
    params_piez, params_soln, params_load, 1.319, nTest1);

figure;
plot(nTest1,T1_Long,'r')
hold on;
plot(nTest1,T1MP_Long,'b')
title('Computation time : Example 1 - Greco, Malwaki, Cheng, Li')
xlabel('# of slices')
ylabel('Computation Time (s)')
axis([50 500 -5 30])
legend( 'RFEM', 'Morg Price', 'Location', 'northwest')

%%
% <latex>
% \subsection{Examples 2 and 6}
% Results compared to those from Zolfaghari et al (2005), Cheng et al 
% (2007), Li et al (2010) and Pham and Fredlund (2003). The graphs of these 
% examples continue to show convergence to relative error of approximately 
% 5\% - 10\% after approximately 25 steps. Again these are all positive
% results suggesting accuracy of both solvers.
% </latex>


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex2.dat');
params_soln = struct('ltor',ltor,'ftype',0);

data = ... % read in slip surfaces
    dlmread('../data files/Ex2_RFEM.surf')'; 
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);


[E2] = SSA_SliceTesterAlgorithm (@RFEMSolver, slip,params_layers,... % compute F and lam for slip surface
    params_piez, params_soln, params_load, 1.113, nStar);

[E2MP] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez,... % compute F and lam for slip surface
    params_soln, params_load, 1.113, nStar);

figure;
plot(nStar,E2,'r')
hold on;
plot(nStar,E2MP,'b')
title('Error : Example 2 - Zolfaghari, Cheng, Li')
xlabel('# of slices')
ylabel('Error (%)')
legend( 'RFEM', 'Morg Price', 'Location', 'northeast')

%
% -----------------
%

[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex2.dat');
params_soln = struct('ltor',ltor,'ftype',0);


data = ... % read in slip surfaces
    dlmread('../data files/Ex6_RFEM.surf')';
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);


[E6] = SSA_SliceTesterAlgorithm (@RFEMSolver, slip, params_layers, params_piez,... % compute F and lam for slip surface
    params_soln, params_load, 1.064, nStar);

[E6MP] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez,... % compute F and lam for slip surface
    params_soln, params_load, 1.064, nStar);


figure;
plot(nStar,E6,'r')
hold on;
plot(nStar,E6MP,'b')
title('Error : Example 6 - Pham/Fredlund, Li')
xlabel('# of slices')
ylabel('Error (%)')
legend( 'RFEM', 'Morg Price', 'Location', 'northeast')

end


function [ params_layers, params_piez, ltor ] = RecieveInput (fname)

data = ... % read in slope geometry, stratigraphy, and piezometric surface
    dlmread(fname); 

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight
E = zeros(nlayer,1);
nu = zeros(nlayer,1);

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

params_layers = struct('strat',strat,'phi',phi,'coh',coh,...
    'gam',gam,'gams',gams,'E',E,'nu',nu);

nwpts = data(i,1);  % number of points in piez data (0 if dry)
piez = [];
gamw = 0;
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
end
params_piez = struct('piez',piez,'gamw',gamw);
end

