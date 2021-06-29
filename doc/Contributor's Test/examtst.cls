%
% Copyright (C) 2001-2012 John Nakamura. All rights reserved.
%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{examtst}[2003/01/01]
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
\RequirePackage{ifthen,verbatim}

% Setting up margins
% This is for page width
% The original settings have a 1 inch border and margins
% start from that.  The 1 inch border needs to be removed.
% Making a margin of 1.5 cm

\newlength{\EXAM@margin}
\setlength{\EXAM@margin}{1.5cm}
\setlength{\textwidth}{\paperwidth}
\addtolength{\textwidth}{-2\EXAM@margin}
\setlength{\oddsidemargin}{\EXAM@margin}
\addtolength{\oddsidemargin}{-1in}
\setlength{\evensidemargin}{\oddsidemargin}


%%  This is for text height


\setlength{\textheight}{\paperheight}
\addtolength{\textheight}{-\headheight}
\addtolength{\textheight}{-\headsep}
\addtolength{\textheight}{-\footskip}
\addtolength{\textheight}{-1.5\EXAM@margin}   % was -2\EXAM@margin
\setlength{\topmargin}{\EXAM@margin}
\addtolength{\topmargin}{-1in}

%%
%% The END label will be used for figuring out the
%% last page and also the total number of questions
%%

\AtEndDocument{~\label{END}}


%%
%%  Header and Footer setup
%%  The page numbering will not get printed on the first page
%%

\newcommand{\examheader}[1] {\renewcommand{\DH@header}{#1}}
\newcommand{\DH@header}{}

\newcounter{temppg}
\newcommand{\ps@examhead}{%

\renewcommand{\@oddhead}{%
\DH@header \hfill%
}

\renewcommand{\@evenhead}{%
\hfill\DH@header
}%

\renewcommand{\@oddfoot}{%
%   \setcounter{temppg}{\value{page}}%
%   \addtocounter{temppg}{1}%
   \hfill
   \ifthenelse{\value{page}=\pageref{END}}%
   {\mbox{THE~END.}\hfill}%  true part
   {}% false part
   \mbox{Page \arabic{page} of \pageref{END}}
}%
\renewcommand{\@evenfoot}{%
%   \setcounter{temppg}{\value{page}}%
%   \addtocounter{temppg}{1}%
\mbox{Page \arabic{page} of \pageref{END}}
\ifthenelse{\value{page}=\pageref{END}}%
{\hfill\mbox{THE~END.}}%  true part
{}% false part
\hfill
}%
}

\pagestyle{examhead}

\newcommand{\noofpages}{\pageref{END}~}
\newcommand{\noofquestions}{\ref{END}~}

%%
%% Here are some commands for aiding setup of questions
%%



%% QUESTION
%%
%%  Print question number
%%
%%  Requires one argument which can be almost anything, but
%%  usually is the number of marks the question is worth.
%%

\newcounter{qctr}     % main question counter
\setcounter{qctr}{0}
\newcommand{\question}[1] {  % print a counter and increment it as well
\refstepcounter{qctr}%       % the next label command will ref. this counter
\noindent%
\textbf{%
\rule{0cm}{1cm}%                add a little vertical space
Question \arabic{qctr} [#1]}~~% display counter and a message
}% end question



%% SECTION
%%
%%  Print Section number
%%
\newcommand{\sect}[1]  {  % print a counter and increment it as well
\addtocounter{#1}{1}%     % increment counter by 1
Section \arabic{#1}%      % display counter
} % end section


%
%   A marker for indicating the correct answer for a multiple choice
%   question.
%      \marker is for the most correct answer
%      \markera is for a correct answer
%
\newcommand{\marker}{\ifthenelse{\equal{\soln}{y}}{$\Longleftarrow$}{}}
\newcommand{\markera}{\ifthenelse{\equal{\soln}{y}}{$\leftarrow$}{}}
%
%  Underline of a certain length
%
\newcommand{\uline}[1]{\rule[-1mm]{#1}{0.25 mm}}



%%
%%  Fill in the blank
%%
%%  1st argument is the answer
%%  2nd argument is the length of the line
%%

\newcommand{\fitb}[2]{%
\ifthenelse{\equal{\soln}{y}}%
{\underline{~#1~}}%
{~\rule[-1mm]{#2}{0.25mm}~}}



%%
%%  Answer to a short answer question or put in space for an answer.
%%
%%  1st argument is the answer if the solution is required
%%  2nd and 3rd arguments are the width and height dimensions
%%  to create a solid rectangular box.  If width or height is
%%  zero, this can be used to create white space.
%%

\newcommand{\answer}[3]{%
\ifthenelse{\equal{\soln}{y}}%
{\begin{flushleft}ANSWER:\\#1\end{flushleft}}%
{\par\rule[-1mm]{#2}{#3}}}


%%
%%  Answer to a question nothing else
%%
%%  1st argument is the answer if the solution is required
%%

\newcommand{\ans}[1]{%
\ifthenelse{\equal{\soln}{y}}%
{#1}%
{}}


%%
%%  Answer to a question alternative
%%
%%  1st argument is the answer if the solution is required
%%  2nd other text
%%

\newcommand{\ansb}[2]{\ifthenelse{\equal{\soln}{y}}{#1}{#2}}

%%
%%  True/False environment
%%  Setup:
%%
%%  \begin{truefalse}{\number type}{n}
%%  \item[x] .........
%%  \end{truefalse}
%%
%%  1st argument is the number type (e.g. \roman, \arabic)
%%  2nd argument is how many T/F questions
%%  The value x in the [] should be either T or F
%%  The answer will appear if the solution option is picked
%%

\newcounter{tfi}
\newlength{\tmp}
\newenvironment{truefalse}[2]{%
\begin{list}
{}
{%start list declaration
\setlength{\labelsep}{0.3 cm}
\setcounter{tfi}{0}
%
% This loop will find the longest label
% Go through all the labels
% If the current label is the longest then
% Store it in \labelwidth
%
\whiledo{\not\(\value{tfi}>#2\)}%
{%
\settowidth{\tmp}{\uline{1cm}~#1{tfi}.}
\ifthenelse{\lengthtest{\labelwidth < \tmp}}%
{\setlength{\labelwidth}{\tmp}}
{}
\addtocounter{tfi}{1}
}% body
%
% Reset the counter
% Add a little more space to the label
% Set the left margin
%
\setcounter{tfi}{0}
\addtolength{\labelwidth}{\labelsep}
\setlength{\leftmargin}{\labelwidth}
\addtolength{\leftmargin}{\labelsep}
%\setlength{\rightmargin}{1 cm}
\renewcommand{\makelabel}[1]{
  \addtocounter{tfi}{1}
  \fitb{##1}{1cm}\hfill #1{tfi}.}
}}%
{\end{list}}


%%
%%  Multiple choice
%%
%%  Setup:
%%
%%  \begin{mc}{\number type}{n}
%%  \item[x] .........
%%  \end{mc}
%%
%%  1st argument is the number type (e.g. \roman, \arabic)
%%  2nd argument is how many answers
%%  If there is an x in [], then the answer will appear if the
%%  solution option is picked
%%

\newcounter{mci}
\newenvironment{mc}[2]{%
\begin{list}
{}
{%start list declaration
\setlength{\labelsep}{0.3 cm}
\setcounter{mci}{0}
%
% This loop will find the longest label
% Go through all the labels
% If the current label is the longest then
% Store it in \labelwidth
%
\whiledo{\not\(\value{mci}>#2\)}%
{%
\settowidth{\tmp}{#1{mci}.}
\ifthenelse{\lengthtest{\labelwidth < \tmp}}%
{\setlength{\labelwidth}{\tmp}}
{}
\addtocounter{mci}{1}
}% body
%
% Reset the counter
% Add a little more space to the label
% Set the left margin
%
\setcounter{mci}{0}
%\addtolength{\labelwidth}{\labelsep}
\setlength{\leftmargin}{\labelwidth}
\addtolength{\leftmargin}{\labelsep}
%\setlength{\rightmargin}{1 cm}
\renewcommand{\makelabel}[1]{
  \addtocounter{mci}{1}
% \ifthenelse{\equal{##1}{x}}{\marker}{}\hfill #1{mci}.}
  \ifthenelse{\equal{##1}{x}}{\marker}{}\ifthenelse{\equal{##1}{y}}{\markera}{}%
\hfill #1{mci}.}
}}%
{\end{list}}

%%
%% Ask if solutions are to be printed
%%
%%\typein[\soln]{Print solutions? (y/n)}

%
%  If you don't want to be prompted then
%  1) comment out the \typein command above
%  2) uncomment the following command and at the command prompt:
%     (pdf)latex "\newcommand{\soln}{y} \input{filename}"
%     to produce a solution
%
%\providecommand{\soln}{n}% fallback definition
