function SSA_MPSliceTester

clear all; close all;

%% 
% <latex>
% \section{Morgenstern Price Slice Tester} \label{sec:MPTests}
% Testing results obtained from the MorgPriceSolver.m program, a module in
% the SSA program. Factor of safety results from the program are
% compared to results from examples in slope stability papers, to judge the
% accuracy of the implemented algorithm. Due to the comparison analysis of
% the slip surfaces being imperfect definitive pass or fail assessments are 
% not used, so results are measured on a relative and objective scale. The
% general rule of less relative error between results and comparisons 
% suggests a more accurate solution can be used. For the same slip the 
% result may be more accurate towards some comparisons more than others. As
% a guideline relative error less than 10\% could be considered acceptable,
% less than 5\% good, and less than 1\% excellent.
% </latex>
%

%%
% <latex>
% \subsection{Fredlund and Krahn (1977)}
% Results compared to the Fredlund and Krahn (1977) paper. A graph of the 
% relative error between the factor of safety calculated by the program 
% with the factor of safety given in the paper, as a function of the number
% of analysis slices is used to analyze the results. For this paper a 
% circular and non circular slip surface for the same slope are studied, 
% under both dry and wet conditions.
% </latex>
%

nStar = 5:100;

%
% -----------------
%

[params_layers, params_piezYes, ltor] = RecieveInput('../data files/FredlundKrahn1977.dat');

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_soln = struct ('ftype',0,'ltor',ltor);
params_piezNo = struct('piez',[],'gamw',0);

data = dlmread('../data files/FredlundKrahn1977_circ.surf')'; % read in slip surfaces
slipc = data(1:2,:);

data = dlmread('../data files/FredlundKrahn1977_noncirc.surf')'; % read in slip surfaces
slipnc = data(1:2,:);

[ECD] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipc, params_layers, params_piezNo, params_soln, params_load, 2.074, nStar); % compute F and lam for circular surface, dry slope
[ECW] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipc, params_layers, params_piezYes, params_soln, params_load, 1.831, nStar);
[ENCD] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipnc, params_layers, params_piezNo, params_soln, params_load, 1.371, nStar);
[ENCW] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipnc, params_layers, params_piezYes, params_soln, params_load, 1.245, nStar);

%{
strat = params_layers.strat;
phi = params_layers.phi;
coh = params_layers.coh;
gam = params_layers.gam;
gams = params_layers.gams;
E = params_layers.E;
nu = params_layers.nu;

Kcc = 0;
Kcnc = 0;

piez = params_piez.piez;
gamw = params_piez.gamw;

[ECD_Original] = SSA_Original_SliceTester(@MP_Special,slipc,strat,phi,coh,gam,gams,[],gamw,Kcc,ltor,2.074,nStar); % compute F and lam for circular surface, dry slope
[ECW_Original] = SSA_Original_SliceTester(@MP_Special,slipc,strat,phi,coh,gam,gams,piez,gamw,Kcc,ltor,1.831,nStar);
[ENCD_Original] = SSA_Original_SliceTester(@MP_Special,slipnc,strat,phi,coh,gam,gams,[],gamw,Kcnc,ltor,1.371,nStar);
[ENCW_Original] = SSA_Original_SliceTester(@MP_Special,slipnc,strat,phi,coh,gam,gams,piez,gamw,Kcnc,ltor,1.245,nStar);
%}

figure;
plot(nStar,ECD,'r')
hold on
plot(nStar,ECW,'b')
hold on
plot(nStar,ENCD,'g')
hold on
plot(nStar,ENCW,'m')
title('Fredlund and Krahn (1977)')
xlabel('# of slices')
ylabel('Error (%)')
legend('Circular Dry','Circular Wet','NonCircular Dry','NonCirular Wet','Location','northeast')

%%
% <latex>
% The figure shows very large error at low numbers of slices, up until
% approximately 20 slice analysis. Accuracy then begins to level off and
% stays consistent. The graph also shows that the dry analysis has less 
% relative error than the wet analysis for both circular and non circular 
% analysis. It can also be seen that the circular slip converges to less 
% relative error than the non circular analysis. This suggests that the 
% simpler dry and circular cases produce more accurate results, however
% further analysis of this type would be needed for a concrete conclusion.
% </latex>
%

%%
% <latex>
% ~\newline \noindent
% Inspecting the computation of the factor of safety at extremely large
% numbers of slices shows no noticeable change in the returned value.
% Convergence to the same factor of safety occurs at 100 and 100000
% slices, as seen on the following graph.
% </latex>
%

nTest1=[100,1000,10000,30000,50000,75000,100000];
[~,FSCD,TCD] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipc, params_layers, params_piezNo, params_soln, params_load, 2.074, nTest1); % compute F and lam for circular surface, dry slope
[~,FSCW,TCW] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipc, params_layers, params_piezYes, params_soln, params_load, 1.831, nTest1);
[~,FSNCD,TNCD] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipnc, params_layers, params_piezNo, params_soln, params_load, 1.371, nTest1);
[~,FSNCW,TNCW] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slipnc, params_layers, params_piezYes, params_soln, params_load, 1.245, nTest1);

figure;
plot(nTest1,FSCD,'r')
hold on
plot(nTest1,FSCW,'b')
hold on
plot(nTest1,FSNCD,'g')
hold on
plot(nTest1,FSNCW,'m')
title('Fredlund and Krahn (1977)')
xlabel('# of slices')
ylabel('Factor of Safety')
legend('Circular Dry','Circular Wet','NonCircular Dry','NonCirular Wet','Location','northeast')


%%
% <latex>
% The next graph shows the change in computation time for calculation of
% the slope with different slice numbers. An approximately linear increase 
% in computation time with number of analysis slices can be seen. As using 
% a large number of slices sees no noticeable increase in the calculation 
% of the Factor of Safety, the increased computation time makes using more 
% slices than approximately 50 unnecessary. When compared to the 
% computation time results the RFEM solver (\ref{sec:RFEMTests}), it can be 
% seen that the computation time for the Morgenstern Price algorithm is 
% significantly less.
% </latex>
%

figure;
plot(nTest1,TCD,'r')
hold on
plot(nTest1,TCW,'b')
hold on
plot(nTest1,TNCD,'g')
hold on
plot(nTest1,TNCW,'m')
title('Fredlund and Krahn (1977)')
xlabel('# of slices')
ylabel('Computation Time')
legend('Circular Dry','Circular Wet','NonCircular Dry','NonCirular Wet','Location','northeast')


%%
% <latex>
% \subsection{Examples 1 - 5} 
% Results from the examples used in the papers: Greco (1996), Malkawi et al 
% (2001) Zolfaghari et al (2005), Sarma and Tan (2006), Pham and Fredlund 
% (2003), Cheng et al (2007), and Li et al (2010). Results followed the 
% same general pattern as the previous test: Converging to a consistent 
% factor of safety at approximately 25 slices. Relative error between 
% results achieved and the results from the examples in the papers are all 
% less than 10\%, and for some lower than 1\%. For examples with multiple 
% comparisons the result usually converges very strongly to at least one 
% of the comparisons. It should again be noted that the accuracy of results 
% is relative only to the accuracy of the results from the comparison 
% papers. However consistently high accuracy compared to results from
% different papers in many different examples, means a large true error
% would suggest a flaw in the slope stability analysis community as a 
% whole. Not displayed here, but it was also seen that a large number of 
% slices resulted in no noticeable change in the factor of safety 
% calculation.
% </latex>


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex1.dat');
params_soln = struct ('ftype',0,'ltor',ltor);

Q = [];
omega = [];

data = dlmread('../data files/Ex1_Greco1996.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EG1]= SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load, 1.327, nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex1_MalkawiEtAl2001.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EM1]= SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load,1.238,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex1_ChengEtAl2007.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EC1]= SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load,1.325,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex1_LiEtAl2010.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EL1]= SSA_SliceTesterAlgorithm (@MorgPriceSolver,slip, params_layers, params_piez, params_soln, params_load,1.327,nStar); % compute F and lam for slip surface

figure;
plot(nStar,EG1,'r')
hold on
plot(nStar,EM1,'b')
hold on
plot(nStar,EC1,'g')
hold on
plot(nStar,EL1,'m')
title('Example 1')
xlabel('# of slices')
ylabel('Error (%)')
legend('Greco (1996)','Malkawi et al (2001)','Cheng et al (2007)','Li et al (2010)')

%
% -----------------
%

[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex2.dat');
params_soln = struct ('ftype',0,'ltor',ltor);

data = dlmread('../data files/Ex2_ZolfaghariEtAl2005.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EZ2] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load, 1.240, nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex2_ChengEtAl2007.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EC2] = SSA_SliceTesterAlgorithm (@MorgPriceSolver,slip,params_layers, params_piez, params_soln, params_load,1.101,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex2_LiEtAl2010.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EL2] = SSA_SliceTesterAlgorithm (@MorgPriceSolver,slip,params_layers, params_piez, params_soln, params_load,1.113,nStar); % compute F and lam for slip surface

figure;
plot(nStar,EZ2,'r')
hold on
plot(nStar,EC2,'b')
hold on
plot(nStar,EL2,'g')
title('Example 2')
xlabel('# of slices')
ylabel('Error (%)')
legend('Zolfaghari et al (2005)','Cheng et al (2007)','Li et al (2010)','Location','northeast')

%
% -----------------
%

[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex3.dat');
params_soln = struct ('ftype',0,'ltor',ltor);

data = dlmread('../data files/Ex3_ZolfaghariEtAl2005.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EZ3] = SSA_SliceTesterAlgorithm (@MorgPriceSolver,slip,params_layers, params_piez, params_soln, params_load,1.48,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex3_ChengEtAl2007.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EC3] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load,1.349,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex3_LiEtAl2010.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EL3] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load, 1.335, nStar);% compute F and lam for slip surface

figure;
plot(nStar,EZ3,'r')
hold on
plot(nStar,EC3,'b')
hold on
plot(nStar,EL3,'g')
title('Example 3')
xlabel('# of slices')
ylabel('Error (%)')
legend('Zolfaghari et al (2005)','Cheng et al (2007)','Li et al (2010)','Location','northeast')

%
% -----------------
%

[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex4.dat');
params_soln = struct ('ftype',0,'ltor',ltor);

data = dlmread('../data files/Ex4_ChengEtAl2007.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EC4] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load,1.184,nStar);% compute F and lam for slip surface

data = dlmread('../data files/Ex4_LiEtAl2010.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EL4] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load,1.197,nStar); % compute F and lam for slip surface

figure;
plot(nStar,EC4,'r')
hold on
plot(nStar,EL4,'b')
title('Example 4')
xlabel('# of slices')
ylabel('Error (%)')
legend('Cheng et al (2007)','Li et al (2010)','Location','northeast')

%
% -----------------
%

[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex5.dat');
params_soln = struct ('ftype',0,'ltor',ltor);


data = dlmread('../data files/Ex5_PhamFredlund2003.surf')';% read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EPD5] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load,1.413,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex5_PhamFredlund2003_circ.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EPC5] = SSA_SliceTesterAlgorithm (@MorgPriceSolver, slip, params_layers, params_piez, params_soln, params_load,1.485,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex5_LiEtAl2010.surf')'; % read in slip surfaces
slip = data(1:2,:);
Kc = data(3,1);
params_load = struct('Kc',Kc, 'Q',Q, 'omega',omega);

[EL5] = SSA_SliceTesterAlgorithm (@MorgPriceSolver,slip,params_layers, params_piez, params_soln, params_load,1.408,nStar); % compute F and lam for slip surface

figure;
plot(nStar,EPD5,'r')
hold on
plot(nStar,EPC5,'b')
hold on
plot(nStar,EL5,'g')
title('Example 5')
xlabel('# of slices')
ylabel('Error (%)')
legend('Pham and Fredlund (2003) - DYN','Pham and Fredlund (2003) - cir','Li et al (2010)','Location','northeast')


%%
% <latex>
% \subsection{Example 6} 
% Results from the example used in the papers: Pham and Fredlund (2003), Li 
% et al (2010). Analysis of this example demonstrated non convergence of
% the Factor of Safety calculation for specific slice counts of the 
% circular Pham and Fredlund analysis. Non convergence occurs when the
% algorithm calculates a very low factor of safety, or if the algorithm
% solution doesn't become consistent within the limited amount of
% iterations.
% </latex>

[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex6.dat');
params_soln = struct ('ftype',0,'ltor',ltor);

data = dlmread('../data files/Ex6_PhamFredlund2003.surf')'; % read in slip surfaces
slipPD = data(1:2,:);
KcPD = data(3,1);
params_load = struct('Kc',KcPD, 'Q',[], 'omega',[]);

[EPD6] = SSA_SliceTesterAlgorithm (@MorgPriceSolver,slipPD,params_layers, params_piez, params_soln, params_load,1.000,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex6_PhamFredlund2003_circ.surf')'; % read in slip surfaces
slipPC = data(1:2,:);
KcPC = data(3,1);
params_load = struct('Kc',KcPC, 'Q',[], 'omega',[]);

[EPC6] = SSA_SliceTesterAlgorithm (@MorgPriceSolver,slipPC,params_layers, params_piez, params_soln, params_load,1.140,nStar); % compute F and lam for slip surface

data = dlmread('../data files/Ex6_LiEtAl2010.surf')'; % read in slip surfaces
slipL = data(1:2,:);
KcL = data(3,1);
params_load = struct('Kc',KcL, 'Q',[], 'omega',[]);

[EL6] = SSA_SliceTesterAlgorithm (@MorgPriceSolver,slipL,params_layers, params_piez, params_soln, params_load,1.017,nStar); % compute F and lam for slip surface

figure;
plot(nStar,EPD6,'r')
hold on
plot(nStar,EPC6,'b')
hold on
plot(nStar,EL6,'g')
title('Example 6')
axis([0 100 0 200])
xlabel('# of slices')
ylabel('Error (%)')
legend('Pham and Fredlund (2003) - DYN','Pham and Fredlund (2003) - cir','Li et al (2010)','Location','northeast')

%%
% <latex>
% As a special case the limiting number of iterations allowed for the 
% analysis was raised from 20 iterations to 30. The previously non 
% converging results now converge to approximately 15% error, as can be 
% seen in the following figure. This is less accurate than the results seen 
% previously, which were generally under 10%. This demonstrates the trade 
% off between raising the iteration limit allowing better convergence, but 
% decreasing the overall accuracy of the solver. 
% </latex>

%%
% <latex>
% \includegraphics[width=5in]{./VV_SubDocuments/SSA_MPSlice_SpecialCase.png}
% </latex>
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
