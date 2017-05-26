function SSA_GenAlgTester
clear all; close all;  

%%
% <latex>
% \section{Genetic Algorithm Tester} \label{sec:GenAlgTests}
% This script file tests the function GenAlg.m program, a module in the SSA
% program. The program will be tested for the relative error of the
% critical factor of safety for an example slip compared to the critical
% factor of safety from a paper analyzing the same example slip. Repeated
% analysis of an example slip will also be performed to analyze the
% consistency of the algorithm.
% </latex>
%

VertInspect = 10;

%%
% <latex>
% \subsection{Example 1} 
% Comparing results for the example from Greco (1996), Malkawi et al 
% (2001), Cheng et al (2007), Li et al (2010).
% </latex>

F1_grec = 1.327; % reported values of Fs
F1_malk = 1.238;
F1_cheng = 1.325;
F1_li = 1.327;

data = dlmread('../data files/Ex1_Greco1996.surf')'; % read in slip surfaces
slip1_greco = data(1:2,:);
slip1_grecoVert = MatchSlice (slip1_greco, VertInspect);

data = dlmread('../data files/Ex1_MalkawiEtAl2001.surf')'; % read in slip surfaces
slip1_malk = data(1:2,:);
slip1_malkVert = MatchSlice (slip1_malk, VertInspect);

data = dlmread('../data files/Ex1_ChengEtAl2007.surf')'; % read in slip surfaces
slip1_cheng = data(1:2,:);
slip1_chengVert = MatchSlice (slip1_cheng, VertInspect);

data = dlmread('../data files/Ex1_LiEtAl2010.surf')'; % read in slip surfaces
slip1_li = data(1:2,:);
slip1_liVert = MatchSlice (slip1_li, VertInspect);


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex1.dat');
strat = params_layers.strat;

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_search = struct ('Xetr',[0,10],'Xext',[10,20],'Ylim',[0,12]);
params_soln = struct ('cncvu',1,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);

niterations = 15; % Analysis
Vertices = cell(niterations,1);
Fgens = cell(niterations,1);
FS = zeros(niterations,1);
rt = zeros(niterations,1);
delr1_greco = zeros(niterations,1);
delr1_malk = zeros(niterations,1);
delr1_cheng = zeros(niterations,1);
delr1_li = zeros(niterations,1);


%%
% <latex>
% \subsubsection{Consistency Testing}
% Firstly this example is used as a method of testing the consistency of 
% the implemented genetic algorithm. Using the same input for the genetic 
% algorithm to solve for the critical slip surface 15 times the physical 
% and critical factor of safety range the algorithm generates as critical
% clip surfaces will be investigated.
% </latex>
%

for i=1:niterations

  [cslip1, F1, ~, ~, Fgens1, ~, rt1] = ...
      GenAlg( @MorgPriceSolver, params_layers, ...
            params_piez, params_search, params_soln, params_load );
                  
  Vertices{i} = cslip1;
  Fgens{i} = Fgens1;
  FS(i)=F1;
  rt(i)=rt1;

  slipGen = MatchSlice (Vertices{i}, VertInspect);
  delr1_greco(i) = distDiff( slipGen, slip1_grecoVert, VertInspect);
  delr1_malk(i) = distDiff( slipGen, slip1_malkVert, VertInspect);
  delr1_cheng(i) = distDiff( slipGen, slip1_chengVert, VertInspect);
  delr1_li(i) = distDiff( slipGen, slip1_liVert, VertInspect);
end

%%
% <latex>
% For each solution a second order polynomial is fit to the vertexes
% describing that solutions critical slip surface. The approximately
% quadratic shape of the slip surface makes a second order polynomial an
% appropriate fit. Solutions are of the form:
% \[ y(x) = C_{\text{1}} \cdot x^2 + C_{\text{2}} \cdot x + C_{\text{3}} \]
% Where $y$ is the vertical height of the slip surface at horizontal point
% $x$. The following histograms show the spread of the fitting constants
% $C_\text{1}$, $C_\text{2}$, and $C_\text{3}$. If the algorithm is
% consistent the slip surfaces will follow similar shapes and the
% constants will have little spread.
% </latex>
%

C1=zeros(niterations,1); % y(x) = C1*X^2 + C2*X + C3
C2=zeros(niterations,1);
C3=zeros(niterations,1);

for i=1:niterations
    X=Vertices{i,1}(1,:);
    Y=Vertices{i,1}(2,:);
    P=polyfit(X,Y,2);
    C1(i)=P(1);
    C2(i)=P(2);
    C3(i)=P(3);
end

figure;
histfit(C1)
title('Distribution of constant C1')
xlabel('Value of C1')
ylabel('# of Occurences')

figure;
histfit(C2)
title('Distribution of constant C2')
xlabel('Value of C2')
ylabel('# of Occurences')

figure;
histfit(C3)
title('Distribution of constant C3')
xlabel('Value of C3')
ylabel('# of Occurences')


%%
% <latex>
% The results in the figures show that the normal distribution of results
% is approximately followed with few outlying data points, and that the 
% spread of the constants for the different fits is small, differing over a 
% small range. This suggests a consistent solution.
% </latex>

fprintf('C1 has a standard deviation of %.2f\n', std(C1))
fprintf('C2 has a standard deviation of %.2f\n', std(C2))
fprintf('C3 has a standard deviation of %.2f', std(C3))

%%
% <latex>
% The next figure shows a plot of all the critical slip surfaces generated,
% supporting the previous findings by showing all the slip surfaces
% existing along a narrow band. The band width was measured at the entry
% and exit points of the slope for context.
% </latex>

EntryPoints = zeros(1,niterations);
ExitPoints = zeros(1,niterations);
for i = 1:niterations
    EntryPoints(i) = Vertices{i}(1,1);
    ExitPoints(i) = Vertices{i}(1,end);
end
EntryBand = max(EntryPoints) - min(EntryPoints);
ExitBand = max(ExitPoints) - min(ExitPoints);

fprintf('The entry band width was : %.3f\n', EntryBand);
fprintf('The exit band width was : %.3f\n', ExitBand);

figure;
hold on;
for ilayer=1:nlayer % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat1 = plot(X,Y,'k','LineWidth',3);
end
for i = 1:niterations
    gSlip1 = plot ( Vertices{i}(1,:), Vertices{i}(2,:) , 'b');
    hold on;
end
xlabel('x (m)')
ylabel('y (m)')
legend([gStrat1,gSlip1], 'Strat surface', 'Calculated slips', 'Location', 'northwest')
title('Ex1 - Consistency - Slip Soloutions Range')


%%
% <latex>
% The previous results demonstrate that the algorithm generates spatially 
% consistent solutions. The consistency of the Factor of Safety for the 
% critical slips surface will now be investigated.  The following histogram
% summarizes the distribution of calculated critical factors of safety. The
% figure shows a consistent factor of safety calculation, with results 
% differing over a small range.
% </latex>
%

FSavg=mean(FS);
FSstd=std(FS);
FSmin=min(FS);

figure;
histfit(FS)
title('Distribution of Critical Factors of Safety')
xlabel('Value of FS')
ylabel('# of Occurences')

fprintf('The average critical slip Factor of Safety \n is FS=%.4f, with a standard deviation \n of %.4f, and a minimum of FS=%.4f',FSavg,FSstd,FSmin);

%%
% <latex>
% The following figure shows the progression of the critical slip factor of
% safety over each generation of the genetic algorithm. The red dot
% represents the final generation. The figure generally shows convergence 
% after approximately 130 generations. All solutions also seem to follow
% the same general path to convergence.
% </latex>
%

terminalGens = zeros(1,niterations);
figure;
for i = 1:niterations
    gGen1 = plot ( 1:length(Fgens{i}) , Fgens{i} , 'k');
    hold on
    gFin1 = plot ( length(Fgens{i}) , Fgens{i}(length(Fgens{i})) , '*r');
    terminalGens(i) = length(Fgens{i});
end
title('Progression of Factor of Safety')
xlabel('Generation #')
ylabel('Factor of Safety')
legend ([gGen1, gFin1], 'Progress', 'Converged')

fprintf('The average number of generations for convergence is %0.0f\n The maximum is %.0f, and the minimum is %0.0f\n The standard deviation of all trials is %0.0f', mean(terminalGens), max(terminalGens), min(terminalGens), std(terminalGens) )

%%
% <latex>
% The results seen in this section all suggest that the genetic algorithm
% can consistently converge to a common critical slip, with a common
% critical factor of safety.
% </latex>
%

%%
% <latex>
% \subsubsection{Error Test}
% The Example 1 slope problem will now be measured for accuracy based on
% the difference between results generated by the algorithm, and results
% found scientific papers that analyzed the same example slope. A
% comparison between the slip surfaces and factors of safety is made. For
% this example the average of all generated slip surfaces was used.
% \newline\newline\noindent
% The accuracy of the slip is shown visually with a plot, and also
% measured using a \textit{distance error} metric. The metric slices the
% calculated slip surface, and comparison slip surface equally into 10
% description vertexes. The average of distance between vertexes of the 
% same indice, is then considered the \textit{distance error}.
% \newline\newline\noindent
% The accuracy of the factor of safety is measured based on relative error
% with the comparison factor of safety.
% </latex>
%

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat1 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
Xvertsum = zeros(1,length(cslip1));
Yvertsum = zeros(1,length(cslip1));
for j = 1:length(cslip1)
    for i = 1:niterations
        Xvertsum(j) = Xvertsum(j) + Vertices{i}(1,j);
        Yvertsum(j) = Yvertsum(j) + Vertices{i}(2,j);
    end
end
gSlip1 = plot ( Xvertsum/niterations , Yvertsum/niterations, 'LineWidth', 2 );
hold on;
gComp1 = plot ( slip1_greco(1,:), slip1_greco(2,:) , 'r');
hold on;
plot ( slip1_malk(1,:), slip1_malk(2,:) , 'r' );
hold on;
plot ( slip1_cheng(1,:), slip1_cheng(2,:) , 'r' );
hold on;
plot ( slip1_li(1,:), slip1_li(2,:) , 'r');
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat1, gSlip1, gComp1], 'Strat surface', 'Calculated slip', 'Comparison slips', 'Location', 'northwest')
title('Ex1 - Accuracy - calculated v comparison slips')


Xent=zeros(1,niterations);
Xext=zeros(1,niterations);
for i=1:niterations
    Xent(i)=Vertices{i,1}(1,1);
    Xext(i)=Vertices{i,1}(1,end);
end

Xent_greco = slip1_greco(1,1); %greco
Xext_greco = slip1_greco(1,end);
Xent_malk = slip1_malk(1,1); %malk
Xext_malk = slip1_malk(1,end);
Xent_cheng = slip1_cheng(1,1); %cheng
Xext_cheng = slip1_cheng(1,end);
Xent_li = slip1_li(1,1); %li
Xext_li = slip1_li(1,end);

XentAVG=mean(Xent);
XextAVG=mean(Xext);

fprintf('Example 1 - Slip\n');
fprintf('Author                    entry x       exit x    distance error\n');
fprintf('Calculated                %7.4f     %8.4f\n', XentAVG, XextAVG);
fprintf('Greco (1996)              %7.4f     %8.4f    %12.4f\n', Xent_greco, Xext_greco, mean(delr1_greco));
fprintf('Malkawi et al (2001)      %7.4f     %8.4f    %12.4f\n', Xent_malk, Xext_malk, mean(delr1_malk));
fprintf('Cheng et al (2007)        %7.4f     %8.4f    %12.4f\n', Xent_cheng, Xext_cheng, mean(delr1_cheng));
fprintf('Li et al (2010)           %7.4f     %8.4f    %12.4f\n\n', Xent_li, Xext_li, mean(delr1_li));

F1Star = mean (FS);
rt1Star = mean (rt);

fprintf('Example 1 - Factor of Safety\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Calculated                %7.4f               %7.4f\n', F1Star, rt1Star);
fprintf('Greco (1996)              %7.4f     %7.4f\n', F1_grec, abs(F1Star-F1_grec)/F1_grec*100);
fprintf('Malkawi et al (2001)      %7.4f     %7.4f\n', F1_malk, abs(F1Star-F1_malk)/F1_malk*100);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F1_cheng, abs(F1Star-F1_cheng)/F1_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f', F1_li, abs(F1Star-F1_li)/F1_li*100);


%%
% <latex>
% \subsection{Examples 2-7}
% Examples 2-7 compare critical factor of safety results for the program to
% the results of many different papers. The tables show that results are
% very accurate compared to the results in the paper, with a relative
% factor of safety errors generally less than 5\% for at least one of the 
% comparison slips. The plots of the slip surfaces also show a critical 
% slip that is reasonably similar to the comparison slips. 
% \newline\newline\noindent
% The only exception comes in Example 6. Approximately half the time a 
% critical factor of safety equal to the result in the paper will be 
% calculated, while the other half a factor of safety of approximately 0.6 
% will be calculated. Further investigation of this is found in
% \ref{sec:Ex6Tests}.
% </latex>
%

%%
% <latex>
% \subsubsection*{Example 2}
% \textbf{Papers:} Zolfaghari et al (2005), Cheng et al (2007), 
% Li et al (2010)
% </latex>

F2_zolf = 1.240; % reported values of Fs
F2_cheng = 1.101;
F2_li = 1.113;

data = dlmread('../data files/Ex2_ZolfaghariEtAl2005.surf')'; % read in slip surfaces
slip2_zolf = data(1:2,:);
slip2_zolfVert = MatchSlice (slip2_zolf, VertInspect);

data = dlmread('../data files/Ex2_ChengEtAl2007.surf')'; % read in slip surfaces
slip2_cheng = data(1:2,:);
slip2_chengVert = MatchSlice (slip2_cheng, VertInspect);

data = dlmread('../data files/Ex2_LiEtAl2010.surf')'; % read in slip surfaces
slip2_li = data(1:2,:);
slip2_liVert = MatchSlice (slip2_li, VertInspect);


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex2.dat');
strat = params_layers.strat;

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_search = struct ('Xetr',[10,15],'Xext',[27,34],'Ylim',[40,52]);
params_soln = struct ('cncvu',1,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);


[ cslip2, F2, ~, ~, ~, ~, rt2] = ... % pass on to GenAlg
    GenAlg ( @MorgPriceSolver, params_layers, ...
        params_piez, params_search, params_soln, params_load );
    
slipGen2 = MatchSlice (cslip2, VertInspect);
delr2_zolf(i) = distDiff( slipGen2, slip2_zolfVert, VertInspect);
delr2_cheng(i) = distDiff( slipGen2, slip2_chengVert, VertInspect);
delr2_li(i) = distDiff( slipGen2, slip2_liVert, VertInspect);

Xent2_zolf = slip2_zolf(1,1); %zolf
Xext2_zolf = slip2_zolf(1,end);
Xent2_cheng = slip2_cheng(1,1); %cheng
Xext2_cheng = slip2_cheng(1,end);
Xent2_li = slip2_li(1,1); %li
Xext2_li = slip2_li(1,end);
Xent2 = cslip2(1,1); %calc
Xext2 = cslip2(1,end);

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat2 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
gSlip2 = plot ( cslip2(1,:), cslip2(2,:) , 'b', 'LineWidth', 2);
hold on;
gComp2 = plot ( slip2_zolf(1,:), slip2_zolf(2,:) , 'r');
hold on;
plot ( slip2_cheng(1,:), slip2_cheng(2,:) , 'r')
hold on;
plot ( slip2_li(1,:), slip2_li(2,:) , 'r')
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat2, gSlip2, gComp2], 'Strat surface', 'Calculated slip', 'Comparison slips', 'Location', 'northeast')
title('Ex2 - Accuracy - calculated v comparison slips')

fprintf('Example 2 - Slip\n');
fprintf('Author                    entry x       exit x    distance error\n');
fprintf('Calculated                %7.4f     %8.4f\n', Xent2, Xext2);
fprintf('Zolfaghari et al (2005)   %7.4f     %8.4f    %12.4f\n', Xent2_zolf, Xext2_zolf, mean(delr2_zolf));
fprintf('Cheng et al (2007)        %7.4f     %8.4f    %12.4f\n', Xent2_cheng, Xext2_cheng, mean(delr2_cheng));
fprintf('Li et al (2010)           %7.4f     %8.4f    %12.4f\n\n', Xent2_li, Xext2_li, mean(delr2_li));

fprintf('Example 2 - Factor of Safety\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Calculated                %7.4f               %7.4f\n', F2, rt2);
fprintf('Zolfaghari et al (2005)   %7.4f     %7.4f\n', F2_zolf, abs(F2-F2_zolf)/F2_zolf*100);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F2_cheng, abs(F2-F2_cheng)/F2_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f', F2_li, abs(F2-F2_li)/F2_li*100);

%%
% <latex>
% \subsubsection*{Example 3}
% \textbf{Papers:} Zolfaghari et al (2005), Cheng et al (2007), 
% Li et al (2010)
% </latex>

F3_zolf = 1.480; % reported values of Fs
F3_cheng = 1.349;
F3_li = 1.335;

data = dlmread('../data files/Ex3_ZolfaghariEtAl2005.surf')'; % read in slip surfaces
slip3_zolf = data(1:2,:);
slip3_zolfVert = MatchSlice (slip3_zolf, VertInspect);

data = dlmread('../data files/Ex3_ChengEtAl2007.surf')'; % read in slip surfaces
slip3_cheng = data(1:2,:);
slip3_chengVert = MatchSlice (slip3_cheng, VertInspect);

data = dlmread('../data files/Ex3_LiEtAl2010.surf')'; % read in slip surfaces
slip3_li = data(1:2,:);
slip3_liVert = MatchSlice (slip3_li, VertInspect);


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex3.dat');
strat = params_layers.strat;

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_search = struct ('Xetr',[10,15],'Xext',[23,30],'Ylim',[40,52]);
params_soln = struct ('cncvu',1,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);


[ cslip3, F3, ~, ~, ~, ~, rt3] = ... % pass on to GenAlg
    GenAlg ( @MorgPriceSolver, params_layers, ...
        params_piez, params_search, params_soln, params_load );

slipGen3 = MatchSlice (cslip3, VertInspect);
delr3_zolf(i) = distDiff( slipGen3, slip3_zolfVert, VertInspect);
delr3_cheng(i) = distDiff( slipGen3, slip3_chengVert, VertInspect);
delr3_li(i) = distDiff( slipGen3, slip3_liVert, VertInspect);

Xent3_zolf = slip3_zolf(1,1); %zolf
Xext3_zolf = slip3_zolf(1,end);
Xent3_cheng = slip3_cheng(1,1); %cheng
Xext3_cheng = slip3_cheng(1,end);
Xent3_li = slip3_li(1,1); %li
Xext3_li = slip3_li(1,end);
Xent3 = cslip3(1,1); %calc
Xext3 = cslip3(1,end);

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat3 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
gSlip3 = plot ( cslip3(1,:), cslip3(2,:) , 'b', 'LineWidth', 2);
hold on;
gComp3 = plot ( slip3_zolf(1,:), slip3_zolf(2,:) , 'r');
hold on;
plot ( slip3_cheng(1,:), slip3_cheng(2,:) , 'r')
hold on;
plot ( slip3_li(1,:), slip3_li(2,:) , 'r')
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat3, gSlip3, gComp3], 'Strat surface', 'Calculated slip', 'Comparison slips', 'Location', 'northeast')
title('Ex3 - Accuracy - calculated v comparison slips')

fprintf('Example 3 - Slip\n');
fprintf('Author                    entry x       exit x    distance error\n');
fprintf('Calculated                %7.4f     %8.4f\n', Xent3, Xext3);
fprintf('Zolfaghari et al (2005)   %7.4f     %8.4f    %12.4f\n', Xent3_zolf, Xext3_zolf, mean(delr3_zolf));
fprintf('Cheng et al (2007)        %7.4f     %8.4f    %12.4f\n', Xent3_cheng, Xext3_cheng, mean(delr3_cheng));
fprintf('Li et al (2010)           %7.4f     %8.4f    %12.4f\n\n', Xent3_li, Xext3_li, mean(delr3_li));
    
fprintf('Example 3 - Factor of Safety\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Calculated                %7.4f               %7.4f\n', F3, rt3);
fprintf('Zolfaghari et al (2005)   %7.4f     %7.4f\n', F3_zolf, abs(F3-F3_zolf)/F3_zolf*100);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F3_cheng, abs(F3-F3_cheng)/F3_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f', F3_li, abs(F3-F3_li)/F3_li*100);

%%
% <latex>
% \subsubsection*{Example 4}
% \textbf{Papers:} Cheng et al (2007), Li et al (2010)
% </latex>

F4_cheng = 1.184; % reported values of Fs
F4_li = 1.197;

data = dlmread('../data files/Ex4_ChengEtAl2007.surf')'; % read in slip surfaces
slip4_cheng = data(1:2,:);
slip4_chengVert = MatchSlice (slip4_cheng, VertInspect);

data = dlmread('../data files/Ex4_LiEtAl2010.surf')'; % read in slip surfaces
slip4_li = data(1:2,:);
slip4_liVert = MatchSlice (slip4_li, VertInspect);


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex4.dat');
strat = params_layers.strat;

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_search = struct ('Xetr',[10,15],'Xext',[30,35],'Ylim',[40,52]);
params_soln = struct ('cncvu',0,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);


[cslip4,F4,~,~,~,~,rt4] = ... % pass on to GenAlg
    GenAlg ( @MorgPriceSolver, params_layers, ...
        params_piez, params_search, params_soln, params_load );

slipGen4 = MatchSlice (cslip4, VertInspect);

delr4_cheng(i) = distDiff( slipGen4, slip4_chengVert, VertInspect);
delr4_li(i) = distDiff( slipGen4, slip4_liVert, VertInspect);

Xent4_cheng = slip4_cheng(1,1); %cheng
Xext4_cheng = slip4_cheng(1,end);
Xent4_li = slip4_li(1,1); %li
Xext4_li = slip4_li(1,end);
Xent4 = cslip4(1,1); %calc
Xext4 = cslip4(1,end);

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat4 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
gSlip4 = plot ( cslip4(1,:), cslip4(2,:) , 'b', 'LineWidth', 2);
hold on;
gComp4 = plot ( slip4_cheng(1,:), slip4_cheng(2,:) , 'r');
hold on;
plot ( slip4_li(1,:), slip4_li(2,:) , 'r')
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat4, gSlip4, gComp4], 'Strat surface', 'Calculated slip', 'Comparison slips', 'Location', 'northeast')
title('Ex4 - Accuracy - calculated v comparison slips')

fprintf('Example 4 - Slip\n');
fprintf('Author                    entry x       exit x    distance error\n');
fprintf('Calculated                %7.4f     %8.4f\n', Xent4, Xext4);
fprintf('Cheng et al (2007)        %7.4f     %8.4f    %12.4f\n', Xent4_cheng, Xext4_cheng, mean(delr4_cheng));
fprintf('Li et al (2010)           %7.4f     %8.4f    %12.4f\n\n', Xent4_li, Xext4_li, mean(delr4_li));
    
fprintf('Example 4 - Factor of Safety\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Calculated                %7.4f               %7.4f\n', F4, rt4);
fprintf('Cheng et al (2007)        %7.4f     %7.4f\n', F4_cheng, abs(F4-F4_cheng)/F4_cheng*100);
fprintf('Li et al (2010)           %7.4f     %7.4f', F4_li, abs(F4-F4_li)/F4_li*100);

%%
% <latex>
% \subsubsection*{Example 5}
% \textbf{Papers:} Pham and Fredlund (2003), Li et al (2010)
% </latex>

F5_pham = 1.413; % reported values of Fs
F5_li = 1.408;

data = dlmread('../data files/Ex5_PhamFredlund2003.surf')'; % read in slip surfaces
slip5_pham = data(1:2,:);
slip5_phamVert = MatchSlice (slip5_pham, VertInspect);

data = dlmread('../data files/Ex5_LiEtAl2010.surf')'; % read in slip surfaces
slip5_li = data(1:2,:);
slip5_liVert = MatchSlice (slip5_li, VertInspect);


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex2.dat');
strat = params_layers.strat;

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_search = struct ('Xetr',[10,20],'Xext',[35,50],'Ylim',[7,30]);
params_soln = struct ('cncvu',1,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);


[cslip5,F5,~,~,~,~,rt5] = ... % pass on to GenAlg
    GenAlg ( @MorgPriceSolver, params_layers, ...
        params_piez, params_search, params_soln, params_load );

slipGen5 = MatchSlice (cslip5, VertInspect);
delr5_pham(i) = distDiff( slipGen5, slip5_phamVert, VertInspect);
delr5_li(i) = distDiff( slipGen5, slip5_liVert, VertInspect);

Xent5_pham = slip5_pham(1,1); %cheng
Xext5_pham = slip5_pham(1,end);
Xent5_li = slip5_li(1,1); %li
Xext5_li = slip5_li(1,end);
Xent5 = cslip5(1,1); %calc
Xext5 = cslip5(1,end);

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat5 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
gSlip5 = plot ( cslip5(1,:), cslip5(2,:) , 'b', 'LineWidth', 2);
hold on;
gComp5 = plot ( slip5_pham(1,:), slip5_pham(2,:) , 'r');
hold on;
plot ( slip5_li(1,:), slip5_li(2,:) , 'r')
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat5, gSlip5, gComp5], 'Strat surface', 'Calculated slip', 'Comparison slips', 'Location', 'northeast')
title('Ex5 - Accuracy - calculated v comparison slips')

fprintf('Example 5 - Slip\n');
fprintf('Author                    entry x       exit x    distance err\n');
fprintf('Calculated                %7.4f     %8.4f\n', Xent5, Xext5);
fprintf('Pham and Fredlund (2003)  %7.4f     %8.4f    %12.4f\n', Xent5_pham, Xext5_pham, mean(delr5_pham));
fprintf('Li et al (2010)           %7.4f     %8.4f    %12.4f\n\n', Xent5_li, Xext5_li, mean(delr5_li));  
  
fprintf('Example 5 - factor of Safety\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Calculated                %7.4f               %7.4f\n', F5, rt5);
fprintf('Pham and Fredlund (2003)  %7.4f     %7.4f\n', F5_pham, abs(F5-F5_pham)/F5_pham*100);
fprintf('Li et al (2010)           %7.4f     %7.4f', F5_li, abs(F5-F5_li)/F5_li*100);


%%
% <latex>
% \subsubsection*{Example 6}
% \textbf{Papers:} Pham and Fredlund (2003), Li et al (2010)
% </latex>

F6_pham = 1.000; % reported values of Fs
F6_li = 1.017;

data = dlmread('../data files/Ex6_PhamFredlund2003.surf')'; % read in slip surfaces
slip6_pham = data(1:2,:);
slip6_phamVert = MatchSlice (slip6_pham, VertInspect);

data = dlmread('../data files/Ex6_LiEtAl2010.surf')'; % read in slip surfaces
slip6_li = data(1:2,:);
slip6_liVert = MatchSlice (slip6_li, VertInspect);


[params_layers, params_piez, ltor] = RecieveInput('../data files/Ex6.dat');
strat = params_layers.strat;

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_search = struct ('Xetr',[10,20],'Xext',[40,55],'Ylim',[11,30]);
params_soln = struct ('cncvu',1,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);


[cslip6,F6,~,~,~,~,rt6] = ... % pass on to GenAlg
    GenAlg ( @MorgPriceSolver, params_layers, ...
        params_piez, params_search, params_soln, params_load );

slipGen6 = MatchSlice (cslip6, VertInspect);
delr6_pham(i) = distDiff( slipGen6, slip6_phamVert, VertInspect);
delr6_li(i) = distDiff( slipGen6, slip6_liVert, VertInspect);

Xent6_pham = slip6_pham(1,1); %cheng
Xext6_pham = slip6_pham(1,end);
Xent6_li = slip6_li(1,1); %li
Xext6_li = slip6_li(1,end);
Xent6 = cslip6(1,1); %calc
Xext6 = cslip6(1,end);

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat6 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
gSlip6 = plot ( cslip6(1,:), cslip6(2,:) , 'b', 'LineWidth', 2);
hold on;
gComp6 = plot ( slip6_pham(1,:), slip6_pham(2,:) , 'r');
hold on;
plot ( slip6_li(1,:), slip6_li(2,:) , 'r')
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat6, gSlip6, gComp6], 'Strat surface', 'Calculated slip', 'Comparison slips', 'Location', 'northeast')
title('Ex6 - Accuracy - calculated v comparison slips')

fprintf('Example 6 - Slip\n');
fprintf('Author                    entry x       exit x    distance error\n');
fprintf('Calculated                %7.4f     %8.4f\n', Xent6, Xext6);
fprintf('Pham and Fredlund (2003)  %7.4f     %8.4f    %12.4f\n', Xent6_pham, Xext6_pham, mean(delr6_pham));
fprintf('Li et al (2010)           %7.4f     %8.4f    %12.4f\n\n', Xent6_li, Xext6_li, mean(delr6_li));  
  
fprintf('Example 6 - Factor of Safety\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Calculated                %7.4f               %7.4f\n', F6, rt6);
fprintf('Pham and Fredlund (2003)  %7.4f     %7.4f\n', F6_pham, abs(F6-F6_pham)/F6_pham*100);
fprintf('Li et al (2010)           %7.4f     %7.4f', F6_li, abs(F6-F6_li)/F6_li*100);


%%
% <latex>
% \subsubsection*{Example 7}
% \textbf{Papers:} Fredlund and Krahn (1977)
% </latex>

F7_fred = 1.245; % reported values of Fs

data = dlmread('../data files/FredlundKrahn1977_noncirc.surf')'; % read in slip surfaces
slip7_fred = data(1:2,:);
slip7_fredVert = MatchSlice (slip7_fred, VertInspect);


[params_layers, params_piez, ltor] = RecieveInput('../data files/FredlundKrahn1977.dat');
strat = params_layers.strat;

params_load = struct('Kc',0, 'Q',[], 'omega',[]);
params_search = struct ('Xetr',[10,20],'Xext',[40,48],'Ylim',[4,20]);
params_soln = struct ('cncvu',1,'obtu',1,'evnslc',1,'ftype',0,'ltor',ltor);


[cslip7,F7,~,~,~,~,rt7] = ... % pass on to GenAlg
    GenAlg ( @MorgPriceSolver, params_layers, ...
        params_piez, params_search, params_soln, params_load );

slipGen7 = MatchSlice (cslip7, VertInspect);
delr7_fred(i) = distDiff( slipGen7, slip7_fredVert, VertInspect);

Xent7_fred = slip7_fred(1,1); %cheng
Xext7_fred = slip7_fred(1,end);
Xent7 = cslip7(1,1); %calc
Xext7 = cslip7(1,end);

figure;
for ilayer=1:nlayer     % loop through layers
    X=strat{ilayer}(1,:);   % read strat geom
    Y=strat{ilayer}(2,:);
    gStrat7 = plot(X,Y,'k','LineWidth',3);
    hold on;
end
gSlip7 = plot ( cslip7(1,:), cslip7(2,:) , 'b', 'LineWidth', 2);
hold on;
gComp7 = plot ( slip7_fred(1,:), slip7_fred(2,:) , 'r');
xlabel('x (m)')
ylabel('y (m)')
legend( [gStrat7, gSlip7, gComp7], 'Strat surface', 'Calculated slip', 'Comparison slips', 'Location', 'northeast')
title('Ex7 - Accuracy - calculated v comparison slips')

fprintf('Example 7 - Slip\n');
fprintf('Author                    entry x       exit x    distance error\n');
fprintf('Calculated                %7.4f     %8.4f\n', Xent7, Xext7);
fprintf('Fredlund and Krahn (1977) %7.4f     %8.4f    %12.4f\n\n', Xent7_fred, Xext7_fred, mean(delr7_fred)); 

fprintf('Example 7 - Factor of Safety\n');
fprintf('Author                         Fs     err (%%)      time\n');
fprintf('Calculated                %7.4f               %7.4f\n', F7, rt7);
fprintf('Fredlund and Krahn (1977) %7.4f     %7.4f', F7_fred, abs(F7-F7_fred)/F7_fred*100);

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

function slipnew = MatchSlice( slip, nSlices )

XMin = slip (1,1);
XMax = slip (1,length(slip));
XLine = slip (1,:);
YPoints = slip (2,:);

SliceDist = (XMax-XMin)/(nSlices);
XSlices = XMin:SliceDist:XMax;
YSlices = zeros(1,length(XSlices));

XSlices(1) = XLine(1);
XSlices(nSlices+1) = XLine(end);
YSlices(1) = YPoints(1);
YSlices(nSlices+1) = YPoints(end);
for i = 2:nSlices
   k = find(XLine<XSlices(i),1,'last');
   DeltaX = XLine(k+1)-XLine(k);
   DeltaY = YPoints(k+1) - YPoints(k);
   DeltaS = XSlices(i) - XLine(k);
   YSlices(i) = YPoints(k) + (DeltaS*DeltaY)/(DeltaX);
end

slipnew=[XSlices;YSlices];
end

function sum_r = distDiff ( slipA, slipB, Points )
sum_r = 0;
for i = 1:Points
    del_x = slipA(1,i) - slipB(1,i);
    del_y = slipA(2,i) - slipB(2,i);
    r = sqrt ( del_x^2 + del_y^2 );
    sum_r = sum_r + r;
end
end

