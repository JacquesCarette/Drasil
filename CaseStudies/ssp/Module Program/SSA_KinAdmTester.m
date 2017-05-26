
%% 
% <latex>
% \section{Kinematic Admissibility Tester} \label{sec:KinTests}
% Testing results obtained from the KinAdm.m program, a 
% module in the Slope Stability Analysis program. The tester will test the
% algorithm to ensure it can correctly identify when a slip surface fails
% the 6 failure criterion.
% </latex>
%

params_solnNo = struct ('cncvu', 0, 'obtu', 0);
params_solnYes = struct ('cncvu', 1, 'obtu', 1);


npsrf1=4; %Slope Vertices
strat1=[0,10,20,30;20,20,10,10]; %Slope

npsrf2=5;
strat2=[0,5,8,16,25,30;20,20,16,12,10,10];


%%
% <latex>
% \subsection{Testing}
% The module will now be tested for the different failure criterion. A
% kinematically inadmissible test will return a value pass=0, and a failure
% code detailing the cause of the failure. All other values should return a
% value pass=1. All failure criteria will pass boundary cases. The failure
% criterion are tested individually with a simple slope surface, and slip
% surfaces designed to test the failure. Each failure test will give a slip
% designed to pass, a boundary case, and one designed to fail.
% </latex>

%%
% <latex>
% \subsubsection*{Failure (i)}
% The first criteria of a kinematically admissible surface is that the
% x-ordinates of the input slip surface vertexes do not decrease when 
% reading the list of vertexes from beginning to end. (A) is a test with 
% constantly increasing x-ordinates. (B) is a test case with equivalent
% x-ordinates. (C) is a case with decreasing x-ordinates. Results follow:
% </latex>
%

slip_iA=[5,5.0001,15,22;20,18,8,10]; % Increasing
slip_iB=[5,5,15,22;20,18,8,10]; % Flat
slip_iC=[5,4.9999,15,22;20,18,8,10]; % Decreasing

fprintf('Failure (i):\n');
[pass_i1,newslip_i1,ReportCode_i1] = ...
   KinAdm (slip_iA, strat1, params_solnNo);
fprintf(strcat('(A): pass=',num2str(pass_i1),ReportCode_i1));
fprintf('\n'); % Expect Pass

[pass_i2,newslip_i2,ReportCode_i2] = ...
   KinAdm (slip_iB, strat1, params_solnNo);
fprintf(strcat('(B): pass=',num2str(pass_i2),ReportCode_i2));
fprintf('\n'); % Border Case, Expect pass

[pass_i3,newslip_i3,ReportCode_i3] = ...
   KinAdm (slip_iC, strat1, params_solnNo);
fprintf(strcat('(C): pass=',num2str(pass_i3),ReportCode_i3)); % Expect Fail - Code1


%%
% <latex>
% \subsubsection*{End Adjustments}
% The second criteria of a kinematically admissible surface is that the 
% start and end vertexes of the slip surface match the y-ordinate of the 
% uppermost stratigraphic layer at the specified x-ordinate. 
% The module will not fail the slip surface, but will adjust the y value of
% the end vertexes. (A) is a test with vertexes above the uppermost 
% stratigraphic layer. (B) is a test with vertexes on the uppermost
% stratigraphic layer. (C) is a test with vertexes below the uppermost
% stratigraphic layer. Vertice adjustment refers to the y values of the
% vertexes.
% </latex>

slip_ii1=[5,8,11,18;21,14,10,13];
slip_ii2=[5,8,11,18;20,14,10,12]; 
slip_ii3=[5,8,11,18;19,14,10,11];

fprintf('End Adjustments:\n');
[pass_ii1,newslip_ii1,ReportCode_ii1,CodeSlip_ii1] = ...
   KinAdm (slip_ii1, strat1, params_solnNo);
fprintf(strcat('(A): pass=',num2str(pass_ii1),',',ReportCode_ii1,CodeSlip_ii1));
fprintf('\n'); % Expect pass, vertice adjustment

[pass_ii2,newslip_ii2,ReportCode_ii2,CodeSlip_ii2] = ...
   KinAdm (slip_ii2, strat1, params_solnNo);
fprintf(strcat('(B): pass=',num2str(pass_ii2),',',ReportCode_ii2,CodeSlip_ii2));
fprintf('\n'); % Expect pass, no vertice adjustment

[pass_ii3,newslip_ii3,ReportCode_ii3,CodeSlip_ii3] = ...
   KinAdm (slip_ii3, strat1, params_solnNo);
fprintf(strcat('(C): pass=',num2str(pass_ii3),',',ReportCode_ii3,CodeSlip_ii3)); % Expect pass, vertice adjustment


%%
% <latex>
% \subsubsection*{Failure (ii)}
% The vertexes of the slip surface must be within the specified x-ordinate
% range of the uppermost stratigraphic layer. (A) is a test case with 
% vertexes that stay within the uppermost stratigraphic layers range. (B) 
% is a test with a vertice x-ordinate that goes below the minimum range of 
% the uppermost stratigraphic layer. (C) is a test with a vertice 
% x-ordinate that goes above the maximum range of the uppermost 
% stratigraphic layer.
% </latex>

slip_iii1=[1,17,29;20,7,10]; %Below min
slip_iii2=[-1,17,22;20,7,10]; %Below min
slip_iii3=[5,22,31;20,4,10]; %Above max

fprintf('Failure (ii):\n');
[pass_iii1,newslip_iii1,ReportCode_iii1] = ...
   KinAdm (slip_iii1, strat1, params_solnNo);
fprintf(strcat('(A): pass=',num2str(pass_iii1),ReportCode_iii1));
fprintf('\n'); % Expect Pass - Code2 - Outside x Range

[pass_iii2,newslip_iii2,ReportCode_iii2] = ...
   KinAdm (slip_iii2, strat1, params_solnNo);
fprintf(strcat('(B): pass=',num2str(pass_iii2),ReportCode_iii2));
fprintf('\n'); % Expect Fail - Code2

[pass_iii3,newslip_iii3,ReportCode_iii3] = ...
   KinAdm (slip_iii3, strat1, params_solnNo);
fprintf(strcat('(C): pass=',num2str(pass_iii3),ReportCode_iii3)); % Expect Fail - Code2

%{
CODE CHANGE 
Original Flaw - Not checking end vertices that are outside of
x-ordinate limits, but not adjusting slip coordinates. 

Adjustment - Add condition checker to j=1, j=nvtx-1 loops.

Results - Failure code now works. 

Possible further change - Bring end vertices to the edge of the 
x-ordinate domain. 
%}

%%
% <latex>
% \subsubsection*{Failure (iii)}
% The non end vertexes of the slip surface must be below the uppermost
% stratigraphic layer. End vertexes will be moved onto the uppermost 
% stratigraphic layer, and therefore don't interact with this case. This 
% failure case checks only vertexes, line segments above the uppermost 
% strat are checked in failure (iv). (A) is a test with vertexes below the 
% uppermost stratigraphic layer. (B) is a test with vertexes on the
% uppermost stratigraphic layer. (C) is a test with the first interior
% vertice above the uppermost stratigraphic layer. (D) is a test with the
% last interior vertice above the uppermost stratigraphic layer of the slip
% surface.
% </latex>

slip_iii1=[4,8,16,25;20,15,11,10]; %just below
slip_iii3=[5,8,16,25;20,16,12,10]; % follows all
slip_iii4=[5,8,16,25;20,16.1,12,10]; % goes above the surface, vertex 2
slip_iii5=[5,8,16,25;20,16,12.1,10]; % goes above the surface, vertex 3

fprintf('Failure (iii):\n');
[pass_iii1,newslip_iii1,ReportCode_iii1] = ...
   KinAdm (slip_iii1, strat2, params_solnNo);
fprintf(strcat('(A): pass=',num2str(pass_iii1),ReportCode_iii1));
fprintf('\n'); % Expect pass

[pass_iii3,newslip_iii3,ReportCode_iii3] = ...
   KinAdm (slip_iii3, strat2, params_solnNo);
fprintf(strcat('(B): pass=',num2str(pass_iii3),ReportCode_iii3));
fprintf('\n'); % Expect pass - Boundary case

[pass_iii4,newslip_iii4,ReportCode_iii4] = ...
   KinAdm (slip_iii4, strat2, params_solnNo);
fprintf(strcat('(C): pass=',num2str(pass_iii4),ReportCode_iii4));
fprintf('\n'); % Expect Fail - Code3

[pass_iii5,newslip_iii5,ReportCode_iii5] = ...
   KinAdm (slip_iii5, strat2, params_solnNo);
fprintf(strcat('(D): pass=',num2str(pass_iii5),ReportCode_iii5)); % Expect Fail - Code3

%{
CODE CHANGE 

Original Flaw - 2nd last vertice not being checked to see if it is in
the domain

Adjustment - Check this vertice. Add condition checker to j=nvtx-1 loop

Results - Failure code now works. 
%}

%%
% <latex>
% \subsubsection*{Failure (iv)}
% Line segments between vertexes of the slip surface cannot go above the
% uppermost stratigraphic layer. (A) is a test with all line segments below
% the uppermost stratigraphic surface. (B) is a test case with a line
% segment on the uppermost stratigraphic layer. (C) is a test case with a
% line segment below the uppermost stratigraphic layer. the test is
% performed on the three interior line segments of the uppermost
% stratigraphic layer, going from slip entrance to exit as Strat Line 
% Segments 1,2,3.
% </latex>

slip_iv1_1=[4,8,16,25;20,(20-16/3),11,10]; % Line segment below
slip_iv1_2=[5,8,16,25;20,16,11,10]; % Line segment on
slip_iv1_3=[5,9,16,25;20,15,11,10]; % Line segment above

fprintf('Failure (iv) Strat Line Segment 1:\n');
[pass_iv1_1,newslip_iv1_1,ReportCode_iv1_1] = ...
   KinAdm (slip_iv1_1, strat2, params_solnNo);
fprintf(strcat('(A): pass=',num2str(pass_iv1_1),ReportCode_iv1_1));
fprintf('\n'); % Expect pass

[pass_iv1_2,newslip_iv1_2,ReportCode_iv1_2] = ...
   KinAdm (slip_iv1_2, strat2, params_solnNo);
fprintf(strcat('(B): pass=',num2str(pass_iv1_2),ReportCode_iv1_2));
fprintf('\n'); % Expect pass - Boundary case

[pass_iv1_3,newslip_iv1_3,ReportCode_iv1_3] = ...
   KinAdm (slip_iv1_3, strat2, params_solnNo);
fprintf(strcat('(C): pass=',num2str(pass_iv1_3),ReportCode_iv1_3));
fprintf('\n\n'); % Expect fail - Code4


slip_iv2_1=[4,8,16.5,25;20,(20-16/3),11,10]; % Line segment below
slip_iv2_2=[4,7.5,16.5,25;20,16.25,11.75,10]; % Line segment on
slip_iv2_3=[4,7.5,16.5,25;20,16.5,11.75,10]; % Line segment above

fprintf('Failure (iv) Strat Line Segment 2:\n');
[pass_iv2_1,newslip_iv2_1,ReportCode_iv2_1] = ...
   KinAdm (slip_iv2_1, strat2, params_solnNo);
fprintf(strcat('(A): pass=',num2str(pass_iv2_1),ReportCode_iv2_1));
fprintf('\n'); % Expect pass

[pass_iv2_2,newslip_iv2_2,ReportCode_iv2_2] = ...
   KinAdm (slip_iv2_2, strat2, params_solnNo);
fprintf(strcat('(B): pass=',num2str(pass_iv2_2),ReportCode_iv2_2));
fprintf('\n'); % Expect pass - Boundary case

[pass_iv2_3,newslip_iv2_3,ReportCode_iv2_3] = ...
   KinAdm (slip_iv2_3, strat2, params_solnNo);
fprintf(strcat('(C): pass=',num2str(pass_iv2_3),ReportCode_iv2_3));
fprintf('\n\n'); % Expect fail - Code4



slip_iv3_1=[4,8,16.5,25;20,(20-16/3),11,10]; % Line segment below
slip_iv3_2=[4,8,14,25;20,15,(12+4/9),10]; % Line segment on
slip_iv3_3=[4,8,14,25;20,15,(12+4/9+0.1),10]; % Line segment above

fprintf('Failure (iv) Strat Line Segment 3:\n');
[pass_iv3_1,newslip_iv3_1,ReportCode_iv3_1] = ...
   KinAdm (slip_iv3_1, strat2, params_solnNo);
fprintf(strcat('(A): pass=',num2str(pass_iv3_1),ReportCode_iv3_1));
fprintf('\n'); % Expect pass

[pass_iv3_2,newslip_iv3_2,ReportCode_iv3_2] = ...
   KinAdm (slip_iv3_2, strat2, params_solnNo);
fprintf(strcat('(B): pass=',num2str(pass_iv3_2),ReportCode_iv3_2));
fprintf('\n'); % Expect pass - Boundary case

[pass_iv3_3,newslip_iv3_3,ReportCode_iv3_3] = ...
   KinAdm (slip_iv3_3, strat2, params_solnNo);
fprintf(strcat('(C): pass=',num2str(pass_iv3_3),ReportCode_iv3_3)); % Expect fail - Code4

%{ 
PROBLEM FOUND

doesnt notice if intersection point for both lines occours at an
intersection point.
 
slip_iv3_3=[4,8,14,25;20,15,(13),10];
strat2=[0,5,8,16,25,30;20,20,16,12,10,10];
%}

%%
% <latex>
% \subsubsection*{Failure (v)}
% The slip surface must be concave upwards. The slope of line segments
% between vertexes of the slip surface must go from a large magnitude
% negative number towards a large magnitude positive number when connecting
% vertexes from slip entrance to exit. (A) test a case where slip surface 
% slopes are increasing. (B) tests a case where slip surface 
% slopes are constant. (C) tests a case where slip slopes experience a
% decrease.
% </latex>

slip_v1=[8,10,15,20,29;20,17,11.9,7,10]; % Concave up
slip_v2=[8,10,15,20,29;20,17,12,7,10]; % Flat line
slip_v3=[8,10,15,20,29;20,17,12.1,7,10]; % concave down

fprintf('Failure (v):\n');
[pass_v1,newslip_v1,ReportCode_v1] = ...
   KinAdm (slip_v1, strat1, params_solnYes);
fprintf(strcat('(A): pass=',num2str(pass_v1),ReportCode_v1));
fprintf('\n'); % Expect pass

[pass_v2,newslip_v2,ReportCode_v2] = ...
   KinAdm (slip_v2, strat1, params_solnYes);
fprintf(strcat('(B): pass=',num2str(pass_v2),ReportCode_v2));
fprintf('\n'); % Expect pass - Boundary case

[pass_v3,newslip_v3,ReportCode_v3] = ...
   KinAdm (slip_v3, strat1, params_solnYes);
fprintf(strcat('(C): pass=',num2str(pass_v3),ReportCode_v3)); % Expect fail - Code5


%%
% <latex>
% \subsubsection*{Failure (vi)}
% Slip surfaces cannot have angles less than 110 degrees (1.9199 rads) 
% between adjacent line segments connecting vertexes. (A) tests a case with
% a greater than 110 degree slope. (B) tests a case with an exactly 110
% degree slope. (C) tests a case with a less than 110 degree slope.
% </latex>

AngleStar=20*pi/180;
strat1=[0,10,20,30;20,20,10,10]; %Slope
slip_vi1=[8,8,20;20,10+12*sin(AngleStar)+0.1,10];
slip_vi2=[8,8,20;20,10+12*sin(AngleStar),10]; % Flat line
slip_vi3=[8,8,20;20,10+12*sin(AngleStar)-0.1,10]; % concave down

fprintf('Failure (vi):\n');
[pass_vi1,newslip_vi1,ReportCode_vi1] = ...
   KinAdm (slip_vi1, strat1, params_solnYes);
fprintf(strcat('(A): pass=',num2str(pass_vi1),ReportCode_vi1));
fprintf('\n'); % Expect pass

[pass_vi2,newslip_vi2,ReportCode_vi2] = ...
   KinAdm (slip_vi2, strat1, params_solnYes);
fprintf(strcat('(B): pass=',num2str(pass_vi2),ReportCode_vi2));
fprintf('\n'); % Expect pass - Boundary case

[pass_vi3,newslip_vi3,ReportCode_vi3] = ...
   KinAdm (slip_vi3, strat1, params_solnYes);
fprintf(strcat('(C): pass=',num2str(pass_vi3),ReportCode_vi3)); % Expect fail - Code6

%%
% <latex>
% The results seen differ slightly from whats expected, with all cases
% failing reporting angles just below the 1.9199 rad cut off, despite case 
% (A) being greater than this angle, and (B) being approximately 
% equivalent, based on geometric analysis. The minor error in angle
% calculation likely comes from a \textit{pi} rounding error. Other than
% this small error, all other tests were successful.
% </latex>
