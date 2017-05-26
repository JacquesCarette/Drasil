clear all; close all;

%%
% <latex>
% \section{Example 6 - Further Study} \label{sec:Ex6Tests}
% This script file tests the slope problem from example 6, which has been 
% seen to create spurious results for the GenAlg module. The slope problem
% will be given a consistency test to investigate the performance issues.
% </latex>

F6_pham = 1.000; % reported values of Fs
F6_li = 1.017;

data = ... % read in slope geometry, stratigraphy, and piezometric surface
    dlmread('../data files/Ex6.dat');

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight
E = zeros(nlayer,1);        % youngs modulus weight
nu = zeros(nlayer,1);       % poissons ratio weight

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
phi = phi*pi/180;

params_layers = struct('strat',strat,'phi',phi,'coh',coh,...
    'gam',gam,'gams',gams,'E',E,'nu',nu);

params_load = struct('Kc',0, 'Q',[], 'omega',[]);

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

params_search = struct ('Xetr',[10,20],'Xext',[40,55],'Ylim',[11,30]);
params_soln = struct ('cncvu',1,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);


niterations = 10; % Analysis
cslipVEC6 = cell(niterations,1);
FgensVEC6 = cell(niterations,1);
FSVEC6 = zeros(niterations,1);
rtVEC6 = zeros(niterations,1);
switch7VEC6 = zeros(niterations,1);
switch13VEC6 = zeros(niterations,1);

ix_high = [];
ix_low = [];

for i=1:niterations

  [cslip6,F6,slips6,gens6,Fgens6,nof6,rt6, switch7_6, switch13_6] = ...
      GenAlg( @MorgPriceSolver, params_layers, ...
            params_piez, params_search, params_soln, params_load );
                  
  cslipVEC6{i} = cslip6;
  FgensVEC6{i} = Fgens6;
  FSVEC6(i) = F6;
  rtVEC6(i) = rt6;
  switch7VEC6(i) = switch7_6;
  switch13VEC6(i) = switch13_6; 
  
  if F6 >= 1
      ix_high = [ ix_high, i ];
  else
      ix_low = [ ix_low, i ];
  end
  
end

%%
% <latex>
% \subsection{Testing}
% The genetic algorithm search is performed 10 times. The results of each 
% critical slip are plotted in the following figure. The figure shows 
% little difference between the slips that converge to factor of safety of 
% 1 and those that converge below, other than the sharp incline the low
% factor of safety slopes show at the slip exit.
% </latex>

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat6 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
for i = 1:length(ix_high)
    ghigh = plot ( cslipVEC6{ix_high(i)}(1,:), cslipVEC6{ix_high(i)}(2,:), 'b', 'LineWidth', 2);
    hold on;
end
for i = 1:length(ix_low)
    glow = plot ( cslipVEC6{ix_low(i)}(1,:), cslipVEC6{ix_low(i)}(2,:), 'g', 'LineWidth', 2);
    hold on;
end
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat6, ghigh, glow], 'Strat surface', 'Accurate calculated slips', 'Error calculated slip', 'Location', 'northeast')
title('Ex6 - Accuracy - calculated v comparison slips')

%%
% <latex>
% The next figure shows the progression of the factors of safety through
% the generations for each search. The results tend to not show a single
% specific path the slips with factors of safety less than 1 take towards
% convergence. 
% </latex>

figure;
for i = 1:length(ix_high)
    gGenhigh = plot ( 1:length(FgensVEC6{ix_high(i)}) , FgensVEC6{ix_high(i)} , 'b');
    hold on
    g7 = plot ( switch7VEC6(ix_high(i)) , FgensVEC6{ix_high(i)}(switch7VEC6(ix_high(i))) , '*c');
    hold on
    g13 = plot ( switch13VEC6(ix_high(i)) , FgensVEC6{ix_high(i)}(switch13VEC6(ix_high(i))) , '*m');
    hold on
    gFin = plot ( length(FgensVEC6{ix_high(i)}) , FgensVEC6{ix_high(i)}(length(FgensVEC6{ix_high(i)})) , '*r');
end
for i = 1:length(ix_low)
    gGenlow = plot ( 1:length(FgensVEC6{ix_low(i)}) , FgensVEC6{ix_low(i)} , 'g');
        hold on
    g7 = plot ( switch7VEC6(ix_low(i)) , FgensVEC6{ix_low(i)}(switch7VEC6(ix_low(i))) , '*c');
    hold on
    g13 = plot ( switch13VEC6(ix_low(i)) , FgensVEC6{ix_low(i)}(switch13VEC6(ix_low(i))) , '*m');
    hold on
    gFin = plot ( length(FgensVEC6{ix_low(i)}) , FgensVEC6{ix_low(i)}(length(FgensVEC6{ix_low(i)})) , '*r');
end
title('Progression of Factor of Safety')
xlabel('Generation #')
ylabel('Factor of Safety')
legend ([gGenhigh, gGenlow, g7, g13, gFin], 'FS>1 Progress', 'FS<1 Progress', '7 vertices', '13 vertices',  'Converged', 'Location', 'northeast')



%%
% <latex>
% The next figure of the distribution of factors of safety clearly shows
% the bimodal distribution of critical factors of safety generated by the 
% search.
% </latex>

figure;
histfit(FSVEC6)
title('Distribution of Critical Factors of Safety')
xlabel('Value of FS')
ylabel('# of Occurences')

%%
% <latex>
% The genetic algorithm operates by using the Morgenstern Price algorithm
% to calculate factors of safety. The factor of safety calculated for the
% critical slip by the Morgenstern Price solver is compared to the Rigid
% finite element algorithm. Results found in the following table.
% </latex>

fprintf('Type       Morgenstern Price        RFEM    relative error\n')
for i = 1:length(ix_high)
    FSRFEM6 = RFEMSolver (cslipVEC6{ix_high(i)}, params_layers, params_piez,...
    params_soln, params_load);

    fprintf('Accurate    %10.4f    %14.4f   %10.4f\n', FSVEC6(ix_high(i)), FSRFEM6, 100*abs(FSRFEM6 - FSVEC6(ix_high(i)))/FSVEC6(ix_high(i)) )
end
for i = 1:length(ix_low)
    FSRFEM6 = RFEMSolver (cslipVEC6{ix_low(i)}, params_layers, params_piez,...
    params_soln, params_load);

    fprintf('Error    %13.4f    %14.4f    %10.4f\n', FSVEC6(ix_low(i)), FSRFEM6, 100*abs(FSRFEM6 - FSVEC6(ix_low(i)))/FSVEC6(ix_low(i)) )
end

%%
% <latex>
% These results show disagreement between the Morgenstern Price and RFEM
% algorithms, specifically towards the slip surfaces that produce factors
% of safety less than 1 in the Morgenstern Price algorithm. This could
% suggest that slip surfaces in with the sharp angles shape seen are 
% difficult for the Morgenstern Price algorithm to calculate accurately.
% </latex>

%%
% <latex>
% Next the sharp angle seen produced by the failure cases is studied by 
% measuring the angle of the final rise of the slip surfaces. Results in 
% the table below show that the $FS < 1$ cases have a significantly sharper
% exit angle. This continues to suggest that the shape of the failure slips
% may be the performance issues.
% </latex>

rise_high = zeros(1,length(ix_high));
rise_low = zeros(1,length(ix_low));

fprintf('Type        exit angle        Kin Pass    Failure Code\n')
for i = 1:length(ix_high)
    [pass, newslip, ReportCode, CodeSlip, thetalist] = ...
   KinAdm( cslipVEC6{ix_high(i)}, strat{1}, params_soln);
    
    fprintf('Accurate    %10.4f    %12.0f   %7s\n', min(thetalist), pass, ReportCode)
    
    rise_high(i) = min(thetalist);
end

for i = 1:length(ix_low)
    [pass, newslip, ReportCode, CodeSlip, thetalist] = ...
   KinAdm( cslipVEC6{ix_low(i)}, strat{1}, params_soln);
    
    fprintf('Error    %13.4f    %12.0f    %7s\n', min(thetalist), pass, ReportCode)
    
    rise_low(i) = min(thetalist);
end

fprintf('\n')
fprintf('The average rise angle of the slopes with factors of safety\n greater than 1 is %.3f rads, while the average rise angle \n of slopes with factors of safety less than 1 is %.3f rads.', mean(rise_high), mean(rise_low));

%%
% <latex>
% Using a special test case, where the minimum allowable angle was raised
% (to 2.3 rads, 130 deg) the occurrence rate of low factor of safety 
% results produced by the genetic algorithm is seen to drop. This could be 
% a case that the shape of the failure surface is the performance issue, 
% but may also simply be masking a different performance issue. This change 
% produced no noticeable affect on the results of the other genetic 
% algorithm calculations performed in \ref{sec:GenAlgTests}. A case could 
% be made for raising the minimum allowable angle, but a wider range of 
% test cases would have to be studied before making this decision.
% </latex>