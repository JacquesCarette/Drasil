
clear all; close all; clc;

%%
% <latex>
% \title{Validation Report for Slope Stability Analysis Program}
% \author{Henry Frankis}
% \date{\today}
% \maketitle
% \tableofcontents
% </latex>

%%
% <latex>
% \section{Introduction}
% This document is a report on the results of testing a Slope Stability 
% Analysis program for validity. The tests are introduced, the results are 
% displayed, and the meaning of the results are analyzed. Possible further 
% studies are suggested.
% </latex>
%

%%
% <latex>
% \input{./VV_SubDocuments/SSA_MPSliceTester.tex}
% </latex>
 publish('SSA_MPSliceTester',...
     'format','latex','showCode',false,'imageFormat','png',...
     'stylesheet','../Documentation Files/VV_SubDocuments/latexsheet_Blank.xsl',...
    'outputDir','../Documentation Files/VV_SubDocuments')

%%
% <latex>
% \input{./VV_SubDocuments./SSA_RFEMSliceTester.tex}
% </latex>
 publish('SSA_RFEMSliceTester',...
     'format','latex','showCode',false,'imageFormat','png',...
     'stylesheet','../Documentation Files/VV_SubDocuments/latexsheet_Blank.xsl',...
    'outputDir','../Documentation Files/VV_SubDocuments')


%%
% <latex>
% \input{./VV_SubDocuments./SSA_GenAlgTester.tex}
% </latex>
 publish('SSA_GenAlgTester',...
     'format','latex','showCode',false,'imageFormat','png',...
     'stylesheet','../Documentation Files/VV_SubDocuments/latexsheet_Blank.xsl',...
    'outputDir','../Documentation Files/VV_SubDocuments')


%%
% <latex>
% \input{./VV_SubDocuments/SSA_KinAdmTester.tex}
% </latex>
publish('SSA_KinAdmTester',...
    'format','latex','showCode',false,'imageFormat','png',...
    'stylesheet','../Documentation Files/VV_SubDocuments/latexsheet_Blank.xsl',...
    'outputDir','../Documentation Files/VV_SubDocuments')

%%
% <latex>
% \input{./VV_SubDocuments/SSA_InputTester.tex}
% </latex>
publish('SSA_InputTester',...
    'format','latex','showCode',false,'imageFormat','png',...
    'stylesheet','../Documentation Files/VV_SubDocuments/latexsheet_Blank_NoVerbatim.xsl',...
    'outputDir','../Documentation Files/VV_SubDocuments')

%%
% <latex>
% \input{./VV_SubDocuments/SSA_Ex6Tester.tex}
% </latex>
publish('SSA_Ex6Tester',...
    'format','latex','showCode',false,'imageFormat','png',...
    'stylesheet','../Documentation Files/VV_SubDocuments/latexsheet_Blank.xsl',...
    'outputDir','../Documentation Files/VV_SubDocuments')



clear all; close all;
publish('SSA_TesterMain',... 
    'format','latex','showCode',false,'evalCode',false,'imageFormat','png',...
    'stylesheet','../Documentation Files/VV_SubDocuments/latexsheet_Main.xsl',...
    'outputDir','../Documentation Files')

