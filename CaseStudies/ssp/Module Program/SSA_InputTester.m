clear all; close all; clc;

%%
% <latex>
% \section{Input Tester} \label{sec:InputTests}
% Testing results obtained for the Input.m program a module in the Slope 
% Stability Analysis program. Tested using the SSA\_InputTester.m script. 
% Tests used the script SSA\_InputSpecial.m in place of Input.m, where 
% SSA\_InputSpecial is identical with the exception of predetermined command 
% line inputs. The tester will test the algorithm to ensure it can
% correctly identify faulty input files, through various failure
% mechanisms.
% </latex>

%%
% <latex>
% \renewcommand*{\arraystretch}{1.5}
% \begin{longtable}{| m{0.3\textwidth} | m{0.25\textwidth} | 
% m{0.45\textwidth} |} \hline
% \textbf{Error Type} & \textbf{Case} & \textbf{Error Code}
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% good file & / & 
% </latex>

% Ex3.dat
SSA_InputSpecial ( '../data files/Ex3.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{2}{0.3\textwidth} {Dimensions \\ - input file incorrectly 
% identifies the number of stratigraphic layers describing the slope} & 
% understate &
% </latex>

% Error_WrongNumLayerslow.dat --> 2 layers, file says 1
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_NumLayerslow.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & overstate &
% </latex>

% Error_NumLayershigh.dat --> 2 layers, file says 3
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_NumLayershigh.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{2}{0.3\textwidth} {Dimensions \\ - input file incorrectly 
% identifies the number of vertexes describing a soil layer} & understate &
% </latex>

% Error_NumStratPointslow.dat --> 5 points, file says 4, layer 1
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_NumStratPointslow.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & overstate &
% </latex>

% Error_NumStratPointshigh.dat --> 5 points, file says 6, layer 2
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_NumStratPointshigh.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{2}{0.3\textwidth} {Dimensions \\ - input file incorrectly gives 
% the number of soil properties necessary to describe a layer} & missing 
% properties &
% </latex>

% Error_MissingSoilData.dat 
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_MissingSoilData.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & extra properties &
% </latex>

% Error_ExtraSoilData.dat 
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_ExtraSoilData.dat', 0);
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{2}{0.3\textwidth} {Analysis \\ - input files given soil motion 
% direction does not match the geometry of the slope} & left to right &
% </latex>

% Error_ltor_lefttoright.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_ltor_lefttoright.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & right to left &
% </latex>

% Error_ltor_righttoleft.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_ltor_righttoleft.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{2}{0.3\textwidth} {Analysis \\ - input files given vertices of 
% a soil layer or piezometric surface not ordered in terms of increasing 
% x-ordinates.} & soil layer &
% </latex>

% Error_nonmotonic_layer.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_nonmotonic_layer.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & piezometric surface &
% </latex>

% Error_nonmotonic_piez.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_nonmotonic_piez.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{4}{0.3\textwidth} {Analysis \\ - input file gives end vertexes 
% of a soil layer that does not match the end x-ordinate range of the 
% uppermost stratigraphic layer.} & end vertice \newline - high &
% </latex>

% Error_xrange_layerendhigh.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_layerendhigh.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & end vertice \newline - low &
% </latex>

% Error_xrange_layerendlow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_layerendlow.dat', 0 );

%%
% <latex>
% \\ \cline{2-3} & start vertice \newline - high &
% </latex>

% Error_xrange_layerendlow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_layerstarthigh.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & start vertice \newline - low &
% </latex>

% Error_xrange_layerendlow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_layerstartlow.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{4}{0.3\textwidth} {Analysis \\ - input file gives end vertexes 
% of a piezometric surface that does not match the end x-ordinate range of 
% the uppermost stratigraphic layer.} & end vertice \newline - high &
% </latex>

% Error_xrange_piezendhigh.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_piezendhigh.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & end vertice \newline - low &
% </latex>

% Error_xrange_piezendlow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_piezendlow.dat', 0 );

%%
% <latex>
% \\ \cline{2-3} & start vertice \newline - high &
% </latex>

% Error_xrange_piezendlow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_piezstarthigh.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & start vertice \newline - low &
% </latex>

% Error_xrange_piezendlow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Error_xrange_piezstartlow.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{2}{0.3\textwidth} {Constraints \\ - The effective angle of 
% friction given for all soil layers must be between 0 and 90 degrees.} & 
% $> 90$ &
% </latex>

% Constraint_phihigh.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Constraint_phihigh.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & $< 90$ &
% </latex>

% Constraint_philow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Constraint_philow.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% Constraints \newline - The cohesion given for all soil layers must be 
% greater than 0. & $< 0$ & 
% </latex>

% Constraint_coh.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Constraint_coh.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% Constraints \newline - The soil weight given for all soil layers must be 
% greater than 0. & $< 0$ & 
% </latex>

% Constraint_gam.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Constraint_gam.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% Constraints \newline - The saturated soil weight given for all soil 
% layers must be greater than 0. & $< 0$ & 
% </latex>

% Constraint_gams.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Constraint_gams.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \multirow{2}{0.3\textwidth} {Constraints \\ - The poisson's ratio 
% given for all soil layers must be between 0 and 90 degrees.} & 
% $> 1$ &
% </latex>

% Constraint_nuhigh.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Constraint_nuhigh.dat', 0 );
%%
% <latex>
% \\ \cline{2-3} & $< 0$ &
% </latex>

% Constraint_nulow.dat
SSA_InputSpecial ( '../data files/Faulty_Input_Files/Constraint_nulow.dat', 0 );
%%
% <latex>
% \\ \hline
% </latex>

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

%%
% <latex>
% \end{longtable}
% </latex>

