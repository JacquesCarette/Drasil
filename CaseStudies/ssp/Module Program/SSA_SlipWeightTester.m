clear all; close all; clc; 

%%
% <latex>
% \section{Slip Weighter Tester} \label{sec:SlipWtTests}
% This script file tests the function SlipWeighter.m program, a module in 
% the SSA program. The program will be tested to see that that the module
% correctly orders slopes based on their factors of safety, and that the
% weighting scheme is reasonable.
% </latex>
%

% TestA
% Slip_poolA = ...
%     { [], 10, [] ;
%       [], 2, [] ;
%       [], 1, []; };
% 
% M_poolA = length(Slip_poolA);
% 
% Out_PoolA = SlipWeighter ( M_poolA, Slip_poolA )

% TestB
% Roll = 1;
% Wt1 = zeros(1,length(Roll));
% Wt2 = zeros(1,length(Roll));
% Wt3 = zeros(1,length(Roll));

Slip_poolB = ...
    { ['S1'], 1.35, [] ;
      ['S2'], 1.3, [] ;
      ['S3'], 1.19, [] ;
      ['S4'], 1.2, []; };

Out_PoolB = SlipWeighter ( 4, Slip_poolB )
  
% for i = 1:length(Roll)
%     Slip_poolB {3,2} = Roll(i);
% 
%     M_poolB = length(Slip_poolB);
% 
%     Out_PoolB = SlipWeighter ( M_poolB, Slip_poolB );
%     
%     Wt1(i) = Out_PoolB {1,3};
%     Wt2(i) = Out_PoolB {2,3};
%     Wt3(i) = Out_PoolB {3,3};
% end
% 
% figure;
% plot (Roll, Wt1, 'r')
% hold on;
% plot (Roll, Wt2, 'b')
% hold on;
% plot (Roll, Wt3, '-k')
