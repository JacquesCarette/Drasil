function SSA_PropertySorterTester
clear all; close all; clc;

%% 
% <latex>
% \section{Property Sorting Tester}
%
% Testing results obtained from the SSS_PropertySorter.m program, a module 
% in the SlopeStabilityAnalysis.m program. Results from the are compared to 
% expected results at boundary cases. minor changes are then made to inputs
% to see the change in output.
%
% \newpage
% </latex>
%

strat1={[0,10,20,30;20,20,10,10]}; % strat1 homogenous straight line slope, no piez.
gamly1=[1];
gamsly1=[3];

strat2={[0,10,20,30;20,20,10,10];[0,10,20,30;19,19,9,9]}; % strat2 2 layers below each other. Straight line slope.
gamly2=[1,2];
gamsly2=[3,4];

piez1=[]; % piez1 no piez
gamw1=1;

piez2=[0,10,20,30;18,18,8,8]; % piez2 1 unit below strat1
gamw2=10;

slip1=[4.9999,5,10,20,25,25.0001;20,19,19,9,9,10]; % slip 1
slip2=[4.9999,5,10,20,25,25.0001;20,17,17,7,7,10]; % slip 2

%%
% <latex>
% \subsection{Setup}
% The figures below demonstrate the original set ups, that the tester 
% expects results to match with. 
% ~\newline~\newline\noindent
% Figure 1 shows stratigraphic geometry 1, with a single stratigraphic 
% layer. Slip surface 1 is the red dotted line, and is a slip surface 1 
% unit below the uppermost strat layer. Slip surface 2 is the green dotted 
% line, 3 units below the uppermost stratigraphic layer. The piezometric
% surface is the blue dotted line, 2 units below the uppermost
% stratigraphic layer.
% ~\newline~\newline\noindent
% Figure 2 shows stratigraphic geometry 2, with the same uppermost layer as
% stratigraphic geometry 1, but switches to a new layer 1 unit below the
% uppermost layer. Slip and piezometric surfaces remain the same.
% </latex>
%

figure(1)
plot(strat1{1}(1,:),strat1{1}(2,:),'k','linewidth',3)
hold on
plot(slip1(1,:),slip1(2,:),'--r')
hold on
plot(slip2(1,:),slip2(2,:),'--g')
hold on
plot(piez2(1,:),piez2(2,:),'--b')
axis([-1 31 5 25])
xlabel('x (m)')
ylabel('y (m)')
title('Figure 1')
legend('Strat Layer','Slip Surface A','Slip Surface B','Piez Surface')

figure(2)
h1=plot(strat2{1}(1,:),strat2{1}(2,:),'k','linewidth',3);
hold on
plot(strat2{2}(1,:),strat2{2}(2,:),'k','linewidth',3)
hold on
h2=plot(slip1(1,:),slip1(2,:),'--r');
hold on
h3=plot(slip2(1,:),slip2(2,:),'--g');
hold on
h4=plot(piez2(1,:),piez2(2,:),'--b');
axis([-1 31 5 25])
xlabel('x (m)')
ylabel('y (m)')
title('Figure 2')
legend([h1,h2,h3,h4],{'Strat Layers','Slip Surface A','Slip Surface B','Piez Surface'})

%%
% <latex>
% The results will be studied in 8 cases
% 
% \begin{center}
%    \begin{tabular}{| m{3cm} | m{3cm} | m{3cm} | m{3cm} |}
%    Case & Stratigraphic Geometry & Slip Surface & Piezometric Surface \\ \hline
%    Case(1) & Strat1 & Slip Surface 1 & No \\ \hline
%    Case(2) & Strat2 & Slip Surface 1 & No \\ \hline
%    Case(3) & Strat1 & Slip Surface 2 & No \\ \hline
%    Case(4) & Strat2 & Slip Surface 2 & No \\ \hline
%    Case(5) & Strat1 & Slip Surface 1 & Piez Surface \\ \hline
%    Case(6) & Strat2 & Slip Surface 1 & Piez Surface \\ \hline
%    Case(7) & Strat1 & Slip Surface 2 & Piez Surface \\ \hline
%    Case(8) & Strat2 & Slip Surface 2 & Piez Surface \\ \hline
%    \end{tabular}
% \end{center}
%
% </latex>
%

%%
% <latex>
% \subsection{Testing}
% The results of testing follow below. In the tables a '.' represents the
% property matched its expected reulsts based on the original results. A
% 'FAIL' represents the property did not match.
% ~\newline~\newline\noindent
% The Slice Height and Interslice Properties values test the interslice
% property sorting of the module. The Base Properties values test the base 
% property sorting of the module. The Slice Weight value tests the 
% stratigraphic geometry sorting of the module. The base water pressure 
% forces, surface water pressure forces, and interslice water forces test 
% the piezometric sorting of the module. 
%
% </latex>

%%
% <latex>
% \subsubsection*{Test (i)}
% \textbf{Adjustment:} None
% ~\newline\noindent
% \textbf{Effect:} No failures
% </latex>
fprintf('Test(i) \n\n')
Results (strat1,gamly1,gamsly1,strat2,gamly2,gamsly2,...
    piez1,gamw1,piez2,gamw2,slip1,slip2)
fprintf('\n\n')

%%
% <latex>
% \subsubsection*{Test (ii)}
% \textbf{Adjustment:} Raise piez line to just below slip 1.
% ~\newline\noindent
% \textbf{Effect:} Piezometric now interacts differently with slip 2, but 
% not slip 1. Only affects properties related to the piezometric surface
% and weights of the slices due to change in wter weight.
% ~\newline\noindent
% \textbf{Results:} Changes in Base Water Forces, Slice Weights, and 
% Interslice Water Forces for cases (7),(8)
% </latex>
fprintf('Test(ii) \n\n')
piez2=[0,10,20,30;18.99,18.99,8.99,8.99];
Results (strat1,gamly1,gamsly1,strat2,gamly2,gamsly2,...
    piez1,gamw1,piez2,gamw2,slip1,slip2)
fprintf('\n')
piez2=[0,10,20,30;18,18,8,8]; %Reset

%%
% <latex>
% \subsubsection*{Test (iii)}
% \textbf{Adjustment:} Raise piez line to just above slip 1.
% ~\newline\noindent
% \textbf{Effect:} Piezometric surface now interacts differently with slip 
% 1 also. Still only affects properties related to the piezometric surface 
% and slice weights.
% ~\newline\noindent
% \textbf{Results:} Changes in Base Water Forces, Slice Weights, and 
% Interslice Water Forces for cases (5),(6)(7),(8)
% </latex>
fprintf('Test(iii) \n\n')
piez2=[0,10,20,30;19.01,19.01,9.01,9.01];
Results (strat1,gamly1,gamsly1,strat2,gamly2,gamsly2,...
    piez1,gamw1,piez2,gamw2,slip1,slip2)
fprintf('\n')
piez2=[0,10,20,30;18,18,8,8]; % Reset

%%
% <latex>
% \subsubsection*{Test (iv)}
% \textbf{Adjustment:} Lower slip 1 to just above the piezometric surface.
% ~\newline\noindent
% \textbf{Effect:} Slip 1 interaction with the piezometric surface doesn't 
% change. For stratigraphic geometry 2 now interacts differently with the
% second layer. Lowering will cause changes in height, interslice 
% properties and slice weights for all slip 1 cases. It will also cause
% changes to base properties for slip 1, stratigraphic geometry 2 cases.
% ~\newline\noindent
% \textbf{Results:} Changes in Slice Heights, Slice Weights, and Interslice 
% Properties for cases (1),(2),(5),(6). Changes in Base Properties for 
% cases (2),(6).
% </latex>  
fprintf('Test(iv) \n\n')
slip1=[4.9999,5,10,20,25,25.0001;20,18.01,18.01,8.01,8.01,10];
Results (strat1,gamly1,gamsly1,strat2,gamly2,gamsly2,...
    piez1,gamw1,piez2,gamw2,slip1,slip2)
fprintf('\n')
slip1=[4.9999,5,10,20,25,25.0001;20,19,19,9,9,10]; % Reset

%%
% <latex>
% \subsubsection*{Test (v)}
% \textbf{Adjustment:} Lower slip 1 to just below the piezometric surface.
% ~\newline\noindent
% \textbf{Effect:} Slip 1 now interacts with the piezometric surface. For 
% stratigraphic geometry 2 now interacts differently with the
% second layer. Now new values for cases with slip 1, and piezometric
% surfaces, for water related properties.
% ~\newline\noindent
% \textbf{Results:} Changes in Slice Heights, Slice Weights, and Interslice
% Properties for cases (1),(2),(5),(6). Changes in base properties for 
% cases (2),(6). Changes in Base Water Forces, and Interslice Water Forces 
% for cases (5),(6)
% </latex>  

fprintf('Test(v) \n\n')
slip1=[4.9999,5,10,20,25,25.0001;20,17.99,17.99,7.99,7.99,10];
Results (strat1,gamly1,gamsly1,strat2,gamly2,gamsly2,...
    piez1,gamw1,piez2,gamw2,slip1,slip2)
fprintf('\n')
slip1=[4.9999,5,10,20,25,25.0001;20,19,19,9,9,10]; % Reset


end

function Results(strat1,gamly1,gamsly1,strat2,gamly2,gamsly2,...
    piez1,gamw1,piez2,gamw2,slip1,slip2)
eps=1E-5;

nvtx1=length(slip1);
nsubslice1=5;
nslice1=(nsubslice1)*(nvtx1-1);
evalslip1=SlicerModule(1,slip1,nvtx1,nslice1,nsubslice1);
evalslip2=SlicerModule(1,slip2,nvtx1,nslice1,nsubslice1);

iInt1={[1],[2]};
iInt2={[1,1],[2,1]};
iBase1={[1],[2]};
iBase2={[1,4],[2,3]};

[ubi1,uti1,wi1,Hi1,hwi1,oInti1,oBasei1]=...
    SSA_PropertySorter(evalslip1,strat1,piez1,gamw1,gamly1,gamsly1,iInt1,iBase1);
[ubi2,uti2,wi2,Hi2,hwi2,oInti2,oBasei2]=...
    SSA_PropertySorter(evalslip1,strat2,piez1,gamw1,gamly2,gamsly2,iInt2,iBase2);
[ubi3,uti3,wi3,Hi3,hwi3,oInti3,oBasei3]=...
    SSA_PropertySorter(evalslip2,strat1,piez1,gamw1,gamly1,gamsly1,iInt1,iBase1);
[ubi4,uti4,wi4,Hi4,hwi4,oInti4,oBasei4]=...
    SSA_PropertySorter(evalslip2,strat2,piez1,gamw1,gamly2,gamsly2,iInt2,iBase2);

[ubi5,uti5,wi5,Hi5,hwi5,oInti5,oBasei5]=...
    SSA_PropertySorter(evalslip1,strat1,piez2,gamw2,gamly1,gamsly1,iInt1,iBase1);
[ubi6,uti6,wi6,Hi6,hwi6,oInti6,oBasei6]=...
    SSA_PropertySorter(evalslip1,strat2,piez2,gamw2,gamly2,gamsly2,iInt2,iBase2);
[ubi7,uti7,wi7,Hi7,hwi7,oInti7,oBasei7]=...
    SSA_PropertySorter(evalslip2,strat1,piez2,gamw2,gamly1,gamsly1,iInt1,iBase1);
[ubi8,uti8,wi8,Hi8,hwi8,oInti8,oBasei8]=...
    SSA_PropertySorter(evalslip2,strat2,piez2,gamw2,gamly2,gamsly2,iInt2,iBase2);

% -------------------------------------------------------------------------
% HEIGHT %
%
% i1
if abs(oInti1{1} - ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH1=('.');
else CodeH1=('FAIL'); end
% i2
if abs(oInti2{1} - ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH2=('.');
else CodeH2=('FAIL');end
% i3
if abs(oInti3{1} - 3*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH3=('.');
else CodeH3=('FAIL'); end
% i4
if abs(oInti4{1} - 3*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH4=('.');
else CodeH4=('FAIL'); end
% i5
if abs(oInti5{1} - ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH5=('.');
else CodeH5=('FAIL'); end
% i6
if abs(oInti6{1} - ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH6=('.');
else CodeH6=('FAIL'); end
% i7
if abs(oInti7{1} - 3*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH7=('.');
else CodeH7=('FAIL'); end
% i8
if abs(oInti8{1} - 3*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeH8=('.');
else CodeH8=('FAIL');end
%
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% SURFACE PRESSURE %
%
% i1
if abs(uti1 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT1=('.');
else CodeUT1=('FAIL'); end
% i2
if abs(uti2 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT2=('.');
else CodeUT2=('FAIL');end
% i3
if abs(uti3 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT3=('.');
else CodeUT3=('FAIL'); end
% i4
if abs(uti4 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT4=('.');
else CodeUT4=('FAIL'); end
% i5
if abs(uti5 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT5=('.');
else CodeUT5=('FAIL'); end
% i6
if abs(uti6 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT6=('.');
else CodeUT6=('FAIL'); end
% i7
if abs(uti7 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT7=('.');
else CodeUT7=('FAIL'); end
% i8
if abs(uti8 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUT8=('.');
else CodeUT8=('FAIL');end
%
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% BASE PRESSURE %
%
% i1
if abs(ubi1 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUB1=('.');
else CodeUB1=('FAIL'); end
% i2
if abs(ubi2 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUB2=('.');
else CodeUB2=('FAIL');end
% i3
if abs(ubi3 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUB3=('.');
else CodeUB3=('FAIL'); end
% i4
if abs(ubi4 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUB4=('.');
else CodeUB4=('FAIL'); end
% i5
if abs(ubi5 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUB5=('.');
else CodeUB5=('FAIL'); end
% i6
if abs(ubi6 - zeros(1,nslice1+1)) < eps*ones(1,nslice1+1), CodeUB6=('.');
else CodeUB6=('FAIL'); end
% i7
if abs(ubi7 - [0,10*ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeUB7=('.');
else CodeUB7=('FAIL'); end
% i8
if abs(ubi8 - [0,10*ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeUB8=('.');
else CodeUB8=('FAIL');end
%
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% WEIGHT %
%
% i1
if abs(wi1 - [0,ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW1=('.');
else CodeW1=('FAIL'); end
% i2
if abs(wi2 - [0,ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW2=('.');
else CodeW2=('FAIL');end
% i3
if abs(wi3 - [0,3*ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW3=('.');
else CodeW3=('FAIL'); end
% i4
if abs(wi4 - [0,5*ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW4=('.');
else CodeW4=('FAIL'); end
% i5
if abs(wi5 - [0,ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW5=('.');
else CodeW5=('FAIL'); end
% i6
if abs(wi6 - [0,ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW6=('.');
else CodeW6=('FAIL'); end
% i7
if abs(wi7 - [0,5*ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW7=('.');
else CodeW7=('FAIL'); end
% i8
if abs(wi8 - [0,7*ones(1,nslice1-1),0]) < eps*ones(1,nslice1+1), CodeW8=('.');
else CodeW8=('FAIL');end
%
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% INTERSLICE WATER FORCE %
%
% i1
if abs(Hi1 - zeros(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi1 - zeros(1,nslice1-1) < eps*ones(1,nslice1-1) 
        CodeHW1=('.');
    else CodeHW1=('FAIL'); end
else CodeHW1=('FAIL'); end
% % i2
if abs(Hi2 - zeros(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi2 - zeros(1,nslice1-1) < eps*ones(1,nslice1-1)
        CodeHW2=('.');
    else CodeHW2=('FAIL'); end
else CodeHW2=('FAIL'); end
% % i3
if abs(Hi3 - zeros(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi3 - zeros(1,nslice1-1) < eps*ones(1,nslice1-1)
        CodeHW3=('.');
    else CodeHW3=('FAIL'); end
else CodeHW3=('FAIL'); end
% % i4
if abs(Hi4 - zeros(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi4 - zeros(1,nslice1-1) < eps*ones(1,nslice1-1) 
        CodeHW4=('.');
    else CodeHW4=('FAIL'); end
else CodeHW4=('FAIL'); end
% % i5
if abs(Hi5 - zeros(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi5 - zeros(1,nslice1-1) < eps*ones(1,nslice1-1) 
        CodeHW5=('.');
    else CodeHW5=('FAIL'); end
else CodeHW5=('FAIL'); end
% % i6
if abs(Hi6 - zeros(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi6 - zeros(1,nslice1-1) < eps*ones(1,nslice1-1)
        CodeHW6=('.');
    else CodeHW6=('FAIL'); end
else CodeHW6=('FAIL'); end
% % i7
if abs(Hi7 - 5*ones(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi7 - (1/3)*ones(1,nslice1-1) < eps*ones(1,nslice1-1)
        CodeHW7=('.');
    else CodeHW7=('FAIL'); end
else CodeHW7=('FAIL'); end
% % i8
if abs(Hi8 - 5*ones(1,nslice1-1)) < eps*ones(1,nslice1-1)
    if hwi8 - (1/3)*ones(1,nslice1-1) < eps*ones(1,nslice1-1)
        CodeHW8=('.');
    else CodeHW8=('FAIL'); end
else CodeHW8=('FAIL'); end
%
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% BASE PROPERTIES %
%
% i1
if abs(oBasei1{1} - ones(1,nslice1)) < eps*ones(1,nslice1)
    if abs(oBasei1{2} - 2*ones(1,nslice1)) < eps*ones(1,nslice1)
        CodeBP1=('.');
    else CodeBP1=('FAIL'); end
else fCodeBP1=('FAIL'); end
% % i2
if abs(oBasei2{1} - [1,4*ones(1,nslice1-2),1]) < eps*ones(1,nslice1)
    if abs(oBasei2{2} - [2,3*ones(1,nslice1-2),2]) < eps*ones(1,nslice1)
        CodeBP2=('.');
    else CodeBP2=('FAIL'); end
else CodeBP2=('FAIL'); end
% % i3
if abs(oBasei3{1} - ones(1,nslice1)) < eps*ones(1,nslice1)
    if abs(oBasei3{2} - 2*ones(1,nslice1)) < eps*ones(1,nslice1)
        CodeBP3=('.');
    else CodeBP3=('FAIL'); end
else CodeBP3=('FAIL'); end
% % i4
if abs(oBasei4{1} - [3,4*ones(1,nslice1-2),3]) < eps*ones(1,nslice1)
    if abs(oBasei4{2} - [8/3,3*ones(1,nslice1-2),8/3]) < eps*ones(1,nslice1)
        CodeBP4=('.');
    else CodeBP4=('FAIL'); end
else CodeBP4=('FAIL'); end
% % i5
if abs(oBasei5{1} - ones(1,nslice1)) < eps*ones(1,nslice1)
    if abs(oBasei5{2} - 2*ones(1,nslice1)) < eps*ones(1,nslice1)
        CodeBP5=('.');
    else CodeBP5=('FAIL'); end
else CodeBP5=('FAIL'); end
% % i6
if abs(oBasei6{1} - [1,4*ones(1,nslice1-2),1]) < eps*ones(1,nslice1)
    if abs(oBasei6{2} - [2,3*ones(1,nslice1-2),2]) < eps*ones(1,nslice1)
        CodeBP6=('.');
    else CodeBP6=('FAIL'); end
else CodeBP6=('FAIL'); end
% % i7
if abs(oBasei7{1} - ones(1,nslice1)) < eps*ones(1,nslice1)
    if abs(oBasei7{2} - 2*ones(1,nslice1)) < eps*ones(1,nslice1)
        CodeBP7=('.');
    else CodeBP7=('FAIL'); end
else CodeBP7=('FAIL'); end
% % i8
if abs(oBasei8{1} - [3,4*ones(1,nslice1-2),3]) < eps*ones(1,nslice1)
    if abs(oBasei8{2} - [8/3,3*ones(1,nslice1-2),8/3]) < eps*ones(1,nslice1)
        CodeBP8=('.');
    else CodeBP8=('FAIL'); end
else CodeBP8=('FAIL'); end
%
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% INTERSLICE PROPERTIES %
%
% i1
if abs(oInti1{2} - (1*2)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP1=('.');
else CodeIP1=('FAIL'); end
% % i2
if abs(oInti2{2} - (1*2+0*1)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP2=('.');
else CodeIP2=('FAIL'); end
% % i3
if abs(oInti3{2} - (3*2)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP3=('.');
else CodeIP3=('FAIL'); end
% % i4
if abs(oInti4{2} - (1*2+2*1)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP4=('.');
else CodeIP4=('FAIL'); end
% % i5
if abs(oInti5{2} - (1*2)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP5=('.');
else CodeIP5=('FAIL'); end
% % i6
if abs(oInti6{2} - (1*2+0*1)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP6=('.');
else CodeIP6=('FAIL'); end
% % i7
if abs(oInti7{2} - (3*2)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP7=('.');
else CodeIP7=('FAIL'); end
% % i8
if abs(oInti8{2} - (1*2+2*1)*ones(1,nslice1-1)) < eps*ones(1,nslice1-1), CodeIP8=('.');
else CodeIP8=('FAIL'); end
%
% -------------------------------------------------------------------------

fprintf('Property                 Case(1)   Case(2)   Case(3)   Case(4)   Case(5)   Case(6)   Case(7)   Case(8)\n\n')
fprintf('Slice Heights   %13s   %7s   %7s   %7s   %7s   %7s   %7s   %7s\n',CodeH1,CodeH2,CodeH3,CodeH4,CodeH5,CodeH6,CodeH7,CodeH8)
fprintf('Surface Water Forces   %6s   %7s   %7s   %7s   %7s   %7s   %7s   %7s\n',CodeUT1,CodeUT2,CodeUT3,CodeUT4,CodeUT5,CodeUT6,CodeUT7,CodeUT8)
fprintf('Base Water Forces   %9s   %7s   %7s   %7s   %7s   %7s   %7s   %7s\n',CodeUB1,CodeUB2,CodeUB3,CodeUB4,CodeUB5,CodeUB6,CodeUB7,CodeUB8)
fprintf('Interslice Water Forces   %3s   %7s   %7s   %7s   %7s   %7s   %7s   %7s\n',CodeHW1,CodeHW2,CodeHW3,CodeHW4,CodeHW5,CodeHW6,CodeHW7,CodeHW8)
fprintf('Slice Weights   %13s   %7s   %7s   %7s   %7s   %7s   %7s   %7s\n',CodeW1,CodeW2,CodeW3,CodeW4,CodeW5,CodeW6,CodeW7,CodeW8)
fprintf('Interslice Properties   %5s   %7s   %7s   %7s   %7s   %7s   %7s   %7s\n',CodeIP1,CodeIP2,CodeIP3,CodeIP4,CodeIP5,CodeIP6,CodeIP7,CodeIP8)
fprintf('Base Properties   %11s   %7s   %7s   %7s   %7s   %7s   %7s   %7s\n',CodeBP1,CodeBP2,CodeBP3,CodeBP4,CodeBP5,CodeBP6,CodeBP7,CodeBP8)
end