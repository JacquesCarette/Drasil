\NeedsTeXFormat{LaTeX2e}[2005/12/01]
\def\P@banner{The Art, Science, and Engineering of Programming}
\ProvidesClass{programming}%
    [2022/09/07 v1.4a ^^J Articles for \P@banner^^J]


%%
% `ifplatform` raises a confusing warning when an info would suffice.
% We hack the warning out to not confuse users.
% begin: hack
\RequirePackage{infwarerr}
\let\P@PWNL\PackageWarningNoLine\relax%
\long\def\PackageWarningNoLine#1#2{%
  \def\tempa{ifplatform}\def\tempb{#1}%
  \ifx\tempa\tempb\@PackageInfoNoLine{#1}{#2}%
  \else\@PackageWarningNoLine{#1}{#2}\fi}%
\RequirePackage{hardwrap}
\let\PackageWarningNoLine\P@PWNL\relax%
% end: hack

\GenerateLogMacros{class}[P]{programming}

\RequirePackage{etoolbox,xstring}
\RequirePackage{scrbase}[2014/10/28]
\@ifpackagelater{scrbase}{2014/10/28}{}{%
  \P@error{KOMA-Script too old}{Please upgrade to at least TeXLive 2015 or similar, or upgrade your MiKTeX 2.9 packages.}}
%%
\RequirePackage{ifpdf,ifluatex,ifxetex}
\ifboolexpr{bool{luatex}}%
  {\IfFileExists{luatex85.sty}%
    {\RequirePackage{luatex85}
      \@ifpackagelater{luatex85}{2016/06/15}%
        {}% creationdate-problem fixed
        {{\outputmode=1 \xdef\pdfcreationdate{\pdffeedback creationdate}}}}
    {}
    \IfFileExists{luatexbase.sty}{\RequirePackage{luatexbase}}{}}%
  {}

%%%%
%% DATA
%%%%
\newcommand*\P@@DOI@Prefix{10.22152/programming-journal.org/}
\providecommand*\P@DOI{}
\newcommand*\P@@DOI@Setup{%
  \xdef\P@DOI{\P@@DOI@Prefix \P@Year/\P@Volume/\P@ArticleNumber}}



%% OPTIONS
% from scrkbase
\DefineFamily{programming}
\def\P@Family{\DefineFamilyMember{programming}}
\P@Family%

\newcommand*{\P@Key}[1][.\@currname.\@currext]{%
  \DefineFamilyKey[#1]{programming}}
\newcommand*{\P@ExecuteOptions}[1][.\@currname.\@currext]{%
  \FamilyExecuteOptions[#1]{programming}}

\newcommand*{\P@Options}{\FamilyOptions{programming}}
\let\paperdetails\P@Options
\newcommand*{\AfterP@Options}{}
\let\AfterP@Options\AtEndOfFamilyOptions
\newcommand*{\P@Option}{\FamilyOption{programming}}

\newcommand*{\P@newif}{\P@Family\FamilyBoolKey{programming}}
\newcommand*{\P@setif}{\FamilySetBool{programming}}
\newcommand*{\P@newnum}{\P@Family\FamilyNumericalKey{programming}}
\newcommand*{\P@setnum}{\FamilySetNumerical{programming}}

\newcommand*{\P@newifStd}[1]{%
  \newbool{P@#1}
  \P@newif{#1}{P@#1}}

\newcommand*{\P@curropt}{}
\newcommand*{\P@StdOption}[3][]{%
  \let\P@curropt\CurrentOption
  \DeclareOption{#2}{
    #1%
    \P@ExecuteOptions{#3}}
  \let\CurrentOption\P@curropt
}


\newbool{P@cleveref}
\newbool{P@varioref}
\P@Key{references}[hyperonly]{
  \P@setnum{references}{@tempa}{%
    {clever}{0},{cleveref}{0},{cleverref}{0}% account for common typo
    {noclever}{1},{nocleveref}{1},%
    {vario}{2},{varioref}{2},%
    {novario}{3},{novarioref}{3},%
    {hyperonly}{4},{hyperrefonly}{4},%
  }{#1}
  \ifcase \@tempa\relax
      \booltrue{P@cleveref}
  \or \boolfalse{P@cleveref}
  \or \booltrue{P@varioref}
  \or \boolfalse{P@varioref}
  \or \boolfalse{P@cleveref}
      \boolfalse{P@varioref}
  \fi
  \FamilyKeyStateProcessed}
\P@StdOption{cleveref}{references=cleveref}
\P@StdOption{varioref}{references=varioref}


\newbool{P@submission}
\P@Key{phase}[final]{%
  \ifstrequal{#1}{final}{\boolfalse{P@submission}}{}
  \ifstrequal{#1}{submission}{\booltrue{P@submission}}{}
  \FamilyKeyStateProcessed}
\P@StdOption{submission}{phase=submission}
\P@StdOption{crc}{phase=final}

\providecommand*{\P@SubmissionKey}{}
\def\P@SubmissionKey#1#2{\P@Key{#1}{%
    \AfterP@Options{\ifboolexpr{bool{P@submission}}%
      {#2}{}}}}
\providecommand*{\P@CRCKey}{}
\def\P@CRCKey#1#2{\P@Key{#1}{%
    \AfterP@Options{\ifboolexpr{bool{P@submission}}%
      {}{#2}}}}


\providecommand*\P@codefont{sf}
\P@Key{code}{
  \P@setnum{code}{@tempa}{%
    {sf}{0},{sans}{0},{sansserif}{0},%
    {tt}{1},{mono}{1},{monospace}{1},%
    {ttx}{2},{monoextended}{2},{monospaceexended}{2},%
  }{#1}
  \ifcase \@tempa\relax
      \renewcommand*\P@codefont{sf}
  \or \renewcommand*\P@codefont{tt}
  \or \renewcommand*\P@codefont{ttx}
  \fi
  \FamilyKeyStateProcessed}


\providecommand*\P@license{by}

\newcommand*\P@set@license[1]{%
  \renewcommand*\P@license{#1}
  \if@atdocument
  \P@error{License set too late, move \string\paperdetails up in the document.}%
  {The license cannot be set to ``#1'' at this point in the process. Please
    move the \string\paperdetails command before the \string\begin\{document\}
    command.}
  \else
  \@ifpackageloaded{doclicense}%
    {\undef\doclicenseURL%
      \setkeys{doclicense}{modifier=#1}%
      % hack: propagate settings
      \doclicense@set}
    {\PassOptionsToPackage{modifier=#1}{doclicense}}
  \fi}
 
\P@Key{license}{
  \P@setnum{license}{@tempa}{%
    {ccby}{0},{cc-by}{0},%
    {ccbync}{1},{cc-by-nc}{1},%
  }{#1}
  \ifcase \@tempa\relax
    \P@set@license{by}
  \or
    \P@set@license{by-nc}
  \fi
  \FamilyKeyStateProcessed}

\P@StdOption{ccby}{license=ccby}
\P@StdOption{ccbync}{license=ccbync}


% for paperdetails

\providecommand*\P@area{%
  \P@warning@noline{No area specified}%
  {\larger\textbf{No area specified. Please add a \texttt{area=..} option to \string\paperdetails{...}.}}}
\P@SubmissionKey{area}{\gdef\P@area{#1}\FamilyKeyStateProcessed}

\providecommand*\P@perspective{%
  \P@warning@noline{No perspective selected}%
  {\larger\textbf{No perspective selected. Please add a \texttt{perspective=...}  option to \string\paperdetails{...}.}}}%
\P@SubmissionKey{perspective}{
  \P@setnum{perspective}{@tempa}{%
    {art}{0},{theart}{0},%
    {sciencetheoretical}{1},{theoreticalscience}{1},%
        {theoretical}{1},{science-theoretical}{1},%
        {theoretical-science}{1},%
    {scienceempirical}{2},{empiricalscience}{2},%
        {empirical}{2},{science-empirical}{2},%
        {empirical-science}{2},%
    {engineering}{3},%
  }{#1}
  \ifcase \@tempa\relax
    \renewcommand*\P@perspective{The Art of Programming}
  \or
    \renewcommand*\P@perspective{The Theoretical Science of Programming}
  \or
    \renewcommand*\P@perspective{The Empirical Science of Programming}
  \or
    \renewcommand*\P@perspective{The Engineering of Programming}
  \fi
  \FamilyKeyStateProcessed}

\P@StdOption{art}{perspective=art}
\P@StdOption{theart}{perspective=theart}
\P@StdOption{empirical}{perspective=empirical}
\P@StdOption{science-empirical}{perspective=science-empirical}
\P@StdOption{empirical-science}{perspective=empirical-science}
\P@StdOption{theoretical}{perspective=theoretical}
\P@StdOption{science-theoretical}{perspective=science-theoretical}
\P@StdOption{theoretical-science}{perspective=theoretical-science}
\P@StdOption{engineering}{perspective=engineering}

\providecommand*\P@Year{}
\providecommand*\P@Volume{}
\providecommand*\P@Issue{}
\providecommand*\P@ArticleNumber{}

\newcommand*\P@DOI@BASE{https://doi.org/}

\P@CRCKey{year}{\renewcommand*\P@Year{#1}\P@@DOI@Setup\FamilyKeyStateProcessed}
\P@CRCKey{volume}{\renewcommand*\P@Volume{#1}\P@@DOI@Setup\FamilyKeyStateProcessed}
\P@CRCKey{issue}{\renewcommand*\P@Issue{#1}\P@@DOI@Setup\FamilyKeyStateProcessed}
\P@CRCKey{articlenumber}{\renewcommand*\P@ArticleNumber{#1}\P@@DOI@Setup\FamilyKeyStateProcessed}
\P@CRCKey{submitted}{\DTMsavedate{P@date@submitted}{#1}\FamilyKeyStateProcessed}
\P@CRCKey{published}{\DTMsavedate{P@date@published}{#1}\FamilyKeyStateProcessed}

\P@ExecuteOptions{phase=final,code=sf}


\FamilyProcessOptions{programming}\relax

%%

%% CLASS LOADING
% \PassOptionsToPackage{latest}{latexrelease} 
\IfFileExists{latexrelease.sty}%
   % % Hack. we want the latest, but it spits out a warning that should be an info.
   % {\bgroup\let\PackageWarningNoLine\@gobbletwo\relax
   %   \RequirePackage{latexrelease}\egroup}%
   {\PreventPackageFromLoading*[%
     \typeout{avoid fixltx2e if it does nothing in the first place}]%
   {fixltx2e}}
   {\RequirePackage{fixltx2e}}%

% % don't be too noisy.
%  \PassOptionsToPackage{log-declarations=false}{xparse}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fixes for intermediate versions.
% TL 2015
\@ifpackagelater{scrbase}{2015/09/29}%
 {}% newer, ok
 {\def\set@pdftextpagesize{\set@pdftexpagesize}}

% TL 2016
\@ifpackagelater{scrbase}{2015/11/22}%
  {\@ifpackagelater{scrbase}{2016/06/09}%
     {}% new enough, ok.
     {% There's a bug in tocbasic that got fixed post TL2016.
       % see https://sourceforge.net/p/koma-script/code/2475/
       % workaround by specifying tocdepths manually.
       \@namedef{subsectiontocdepth}{2}%
       \@namedef{subsubsectiontocdepth}{3}%
       \@namedef{subparagraphtocdepth}{5}}}%
  {}% old enough, ok

\LoadClass{scrartcl}

\KOMAoptions{
  fontsize=11pt,
  paper=a4,
  pagesize=automedia,
  DIV=9,
  twoside=semi,
  twocolumn=false,
  titlepage=false,
  footinclude=false,
  headinclude=false,
  headings=normal,
  numbers=noenddot,
}
\KOMAoption{captions}{nooneline,tableheading}

\newcommand*\P@settopbottommargins{%
  \setlength{\topmargin}{-15pt}%
  \setlength{\headsep}{20pt}%
  \setlength{\headheight}{17pt}%
  \setlength{\footheight}{\headheight}%
  \setlength{\footskip}{49pt}%
  \setlength{\marginparpush}{6pt}%
  \setlength{\marginparwidth}{48pt}%
}  
\AfterCalculatingTypearea{\P@settopbottommargins}
\AfterSettingArea{\P@settopbottommargins}


\AtEndPreamble{\areaset[current]{\textwidth}{1.61803399\textwidth}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{babel}

\RequirePackage[LGR,OT1,LY1,T1]{fontenc}
\ifboolexpr{bool{xetex}or bool{luatex}}{}{%
  \RequirePackage[utf8]{inputenx}
  \input{ix-utf8enc.dfu}
  \RequirePackage{alphabeta}}
\RequirePackage{textcomp,textcase,relsize}

%%% Clashes with mathdesign
\PreventPackageFromLoading{amssymb,amsfont}
%
% Mathdesign spuriously redefines things it should not. {{{
\csedef{P@MDback@fboxrule}{\the\fboxrule}
\csedef{P@MDback@fboxsep}{\the\fboxsep}
\csedef{P@MDback@arrayrulewidth}{\the\arrayrulewidth}
\cslet{P@MDback@bfdefault}{\bfdefault}
\cslet{P@MDback@footnoterule}{\footnoterule}
\cslet{P@MDback@hrulefill}{\hrulefill}
%
\RequirePackage[charter,expert]{mathdesign}
%
\setlength{\fboxrule}{\csuse{P@MDback@fboxrule}}
\setlength{\fboxsep}{\csuse{P@MDback@fboxsep}}
\setlength{\arrayrulewidth}{\csuse{P@MDback@arrayrulewidth}}
\letcs{\bfdefault}{P@MDback@bfdefault}
\letcs{\footnoterule}{P@MDback@footnoterule}
\letcs{\hrulefill}{P@MDback@hrulefill}
% cleanup
\csundef{P@MDback@fboxrule}
\csundef{P@MDback@fboxsep}
\csundef{P@MDback@arrayrulewidth}
\csundef{P@MDback@bfdefault}
\csundef{P@MDback@footnoterule}
\csundef{P@MDback@hrulefill}
% }}}
\ifboolexpr{bool{xetex}or bool{luatex}}%
  {\@ifpackagelater{scrbase}{2015/11/22}%
    {% >= TL 2016; not much is tuenc ready.
      \PassOptionsToPackage{euenc}{fontspec}}
    {}
    \RequirePackage{fontspec}[2013/05/20]
    \ifboolexpr{bool{xetex}}%
      {\setmainfont[Numbers=OldStyle,SmallCapsFeatures={Letters=SmallCaps,LetterSpace=5}]{XCharter}\relax}%
      {\setmainfont[Numbers=OldStyle]{XCharter}\relax}
    \def\P@fontscale{MatchLowercase}}%
  {\RequirePackage[osfI,sups]{XCharter}
    \def\P@fontscale{.92}}
\RequirePackage[scaled=\P@fontscale,semibold]{FiraSans}
\ifdefstring{\P@codefont}{sf}%
 {\global\let\ttfamily\sffamily}
 {\ifdefstring{\P@codefont}{tt}%
   {\RequirePackage[scaled=\P@fontscale,lining,nomap]{FiraMono}}
   {}}
\linespread{1.05}

\AtBeginDocument{
\defcaptionname{english}{\figureautorefname}{figure}
\defcaptionname{english}{\tableautorefname}{table}
\defcaptionname{english}{\lstlistingautorefname}{listing}
\defcaptionname{english}{\lstnumberautorefname}{line}
\defcaptionname{english}{\subsectionautorefname}{\sectionautorefname}
\defcaptionname{english}{\subsubsectionautorefname}{\sectionautorefname}
\defcaptionname{english}{\appendixautorefname}{appendix}
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\ifboolexpr{bool{luatex}}%
  {\RequirePackage[final]{microtype}[2013/05/23]}%
  {\RequirePackage[final,babel=true]{microtype}[2013/05/23]}
\ifboolexpr{bool{xetex}}%
  {\let\textls\undefined
    \DeclareRobustCommand{\textls}[2][80]{%
      {{\addfontfeature{LetterSpace=\the\numexpr#1/10\relax}#2}}}}%
  {}

\ifboolexpr{bool{xetex}}%
  {}% see above
  {\microtypesetup{letterspace=50}
   \ifboolexpr{bool{luatex}}{}{\microtypesetup{tracking}}
   \DisableLigatures{encoding = *, family = tt* }}

\newcommand*\P@CodeSetup{%
 \ifboolexpr{bool{xetex}or bool{luatex}}%
   {\ifboolexpr{bool{luatex}}%
      {\addfontfeatures{RawFeature={-rlig,-tlig,-trep,-onum,+zero}}}%
      {\addfontfeatures{Mapping=,Ligatures={NoCommon}}}}%
    {}}%

\AtBeginEnvironment{verbatim}{\microtypesetup{activate=false}\P@CodeSetup}


\AfterPackage{url}{
  \urlstyle{sf}
  % path is tt-only and actually, urls are better.
  \DeclareUrlCommand\path{}% fool emacs: }
}

%%
% Order is important here:
% 1. Varioref (directly loaded here)
% 2. Hyperref (as late as possible, but not AtEndPreamble,
%              as this would break biblatex)
% 3. Cleveref (as late as possible, /after/ hyperref)
% Selection based on user options.
%

\ifboolexpr{bool{P@varioref}}%
  {\RequirePackage{varioref}}
  {} % nothing

\PassOptionsToPackage{log-declarations=false}{xparse}
\PassOptionsToPackage{hyphens}{url}
\PassOptionsToPackage{all}{hypcap}
\PassOptionsToPackage{%
  final,%
  unicode=true,%
  plainpages=false,%
  pdfpagelabels=true,%
  breaklinks=true,
  pdfborder={0 0 0},
  bookmarksnumbered = true,
  pdfpagemode = UseOutlines,
}{hyperref}
\BeforeBeginEnvironment{document}{
  \RequirePackage{hyperref}
  \RequirePackage{bookmark}
  \RequirePackage{hyperxmp}
  \@ifpackagelater{hyperxmp}{2019/03/31}%
    {\hypersetup{keeppdfinfo}}% new
    {}% ok
  \RequirePackage{hypcap}
  \RequirePackage{accsupp}
}

\PassOptionsToPackage{nameinlink,noabbrev}{cleveref}
\ifboolexpr{bool{P@cleveref}}%
  {\BeforeBeginEnvironment{document}{\RequirePackage{cleveref}}}
  {} % nothing


\RequirePackage{color}
\PassOptionsToPackage{table,svgnames,dvipsnames*,x11names}{xcolor}
\ifboolexpr{bool{luatex} or bool{xetex}}%
  {}%
  {\PassOptionsToPackage{fixpdftex}{xcolor}}%

% from Volume 6 on
\AfterPackage{xcolor}%
  {\definecolors{Aquamarine,Black,Blue,BlueViolet,Brown,CadetBlue,%
      CornflowerBlue,Cyan,DarkOrchid,ForestGreen,Fuchsia,Goldenrod,Gray,%
      Green,GreenYellow,Lavender,LimeGreen,Magenta,Maroon,MidnightBlue,%
      NavyBlue,Orange,OrangeRed,Orchid,Plum,Purple,Red,Salmon,%,RoyalBlue
      SeaGreen,SkyBlue,SpringGreen,Tan,Thistle,Turquoise,Violet,%
      VioletRed,White,Yellow,YellowGreen}}%
\RequirePackage{xcolor}

\definecolor[named]{RoyalBlue}{RGB/cmyk}{65,105,225/.8,.61,0,0}
\definecolor{P@Blue}{named}{RoyalBlue}
\definecolor{P@ColorOnBlue}{gray}{.95}
\definecolor{P@GrayFG}{named}{darkgray}
\definecolor{P@GrayBG}{gray}{.90}

\newlength{\doclicenseimagewidth}
\setlength{\doclicenseimagewidth}{2.7cm}
\RequirePackage[
  type={CC},
  modifier=\P@license,
  version={4.0},
  imagewidth=\doclicenseimagewidth,
]{doclicense}

% Fixes for intermediate versions.
% TL 2015
% this is actually an installation bug for TL 2015,
% but we cannot reliably test that, so we resort to the check
% if the package is new enough for TL 2016.
\@ifpackagelater{doclicense}{2016/04/24}%
  {\let\P@doclicenseImage\doclicenseImage}% we're good.
  {\def\P@doclicenseImage{%
      \makebox[\doclicenseimagewidth]{%
        \setlength{\fboxsep}{5pt}%
        \raisebox{.5\baselineskip}{\colorbox{P@GrayBG}{\Huge\selectfont\doclicenseIcon}}}}}

\RequirePackage[tbtags,fleqn]{amsmath}
\PassOptionsToPackage{final}{graphicx}
\RequirePackage{graphicx,grffile,grfext}
% prefer pdf over png
\PrependGraphicsExtensions{.pdf,.PDF}
% ai's are just pdfs.
\ifboolexpr{bool{xetex}}%
  {\DeclareGraphicsRule{.ai}{QTm}{*}{}}
  {\DeclareGraphicsRule{.ai}{pdf}{*}{}}
\AppendGraphicsExtensions{.ai,.AI}

\ifboolexpr{bool{xetex}}%
  {% XeTeX can naturally process eps. This here avoids a warning
    \PreventPackageFromLoading{epstopdf}}
  {}

\RequirePackage{multirow,tabularx,booktabs,threeparttable}
\RequirePackage[figuresright]{rotating}
\RequirePackage{caption,subcaption} % subfig
\RequirePackage{totpages}
\RequirePackage[useregional]{datetime2}


\ifboolexpr{bool{xetex}or bool{luatex}}%
  {\newfontfamily\firaoldstylemono
      [ Path,
        Numbers = {Monospaced,OldStyle},
        UprightFont    = *-Book ,
        ItalicFont     = *-BookItalic ,
        BoldFont    = *-Bold ,
        BoldItalicFont     = *-BoldItalic ,
      ]
      {FiraSans}}{}


\newcommand*\P@listingnumbersfont{%
  \sffamily\small\color{P@GrayFG!40}%
  \ifboolexpr{bool{xetex}or bool{luatex}}%
    {\firaoldstylemono}
    {\tbfigures}}%


\RequirePackage{listings}
\lstset{%
  basicstyle=\small\ttfamily,%
  columns=fullflexible,
  extendedchars=true,
  upquote=true,
  captionpos=t,
  backgroundcolor=\color{P@GrayBG},%
  breaklines=true,
  postbreak=\raisebox{0ex}[0ex][0ex]{\ensuremath{\color{P@Blue}\hookrightarrow\space}},
  numbers=left,
  numberstyle=\P@listingnumbersfont,
  numbersep=.5em,
  frame=single,
  framerule=0pt}
\lst@AddToHook{FontAdjust}{\P@CodeSetup}
\lst@AddToHook{DisplayStyle}{\def\baselinestretch{1}\@currsize}
\lst@AddToHook{TextStyle}{\lstset{breaklines=false}}
% work around listings being too smart
% \lst@AddToHook{Init}{\let\ttdefault\f@family\edef\ttdefault{\ttdefault}}
\patchcmd{\lst@ttfamily}%
  {\ifx\f@family\ttdefault#1\relax\else#2\fi}%
  {#1\relax}%
  {}{\P@warning@noline{Cannot patch listings internals. Ignoring.}}

\AfterPackage{minted}{%
  \AtEndPreamble{%
    \RequirePackage{mdframed}
    \mdfdefinestyle{P@md@minted}%
      {backgroundcolor=P@GrayBG,hidealllines=true}
    \surroundwithmdframed[style=P@md@minted]{minted}}
    \renewcommand{\theFancyVerbLine}%
      {{\P@listingnumbersfont \arabic{FancyVerbLine}}}
    \setminted{%
      autogobble,%
      linenos,%
      numbersep=.5em,%
      xleftmargin=-.7em,%
      fontsize=\small,%
      breaklines=true}
    \setmintedinline{fontsize=\normalsize}}

% XXX: 2021-07-15: require v2 until everything is upgraded
\@ifpackagelater{scrbase}{2017/09/06} % gues TL from KOMA
  {\AtEndPreamble{\RequirePackage{siunitx}[=v2]}} % syntax 2017+ ok
  {\AtEndPreamble{\RequirePackage{siunitx}}} % not needed/possible <=2016 anyway
\AfterPackage{siunitx}{%
  \sisetup{
    binary-units,
    detect-all,
    free-standing-units,
    space-before-unit,
    use-xspace,
    unit-optional-argument,
    parse-units = false}
  \AtEndPreamble{
    % If siunitx support pgf-setting, use it.
    \@ifpackageloaded{pgf}{\csuse{SendSettingsToPgf}}{}
    \addto\extrasenglish{\sisetup{locale = US}}}}

\PassOptionsToPackage{style=numeric,sortcites}{biblatex}
\AfterPackage{biblatex}{
  \setcounter{biburllcpenalty}{7000}
  \setcounter{biburlucpenalty}{8000}
  \ExecuteBibliographyOptions{
    url=true,%
    abbreviate=false,%
    maxnames=999}
  \@ifpackagelater{biblatex}{2016/09/10}%
    {\@ifpackagelater{biblatex}{2017/12/19}%
      {\ExecuteBibliographyOptions{urldate=iso,seconds=true}}%
      {\ExecuteBibliographyOptions{urldate=edtf,seconds=true}}}%
    {\ExecuteBibliographyOptions{urldate=iso8601}}
  \PreventPackageFromLoading[\message{%
    ^^J^^JERROR: You tried to load  the cite package that is not compatible
    with biblatex.^^J^^J%
  }]{cite}
}
\AtEndPreamble{
  \@ifpackageloaded{biblatex}%
    {\pretocmd{\printbibliography}%
      {\pdfbookmark[section]{\refname}{bib}}
      {}{\P@warning@noline{Cannot prepend to printbibliography. Ignoring.}}}
    {% normal bibtex
    \bibliographystyle{plainurl}
    \pretocmd{\bibliography}%
      {\pdfbookmark[section]{\refname}{bib}}
      {}{\P@warning@noline{Cannot prepend to bibliography. Ignoring.}}}}


\setfootnoterule{4cc}
\setkomafont{footnoterule}{\color{P@GrayFG}}
\setlength{\skip\footins}{2\baselineskip}
\setlength{\footskip}{0pt}
\addtokomafont{footnote}{\small}
\deffootnote[1em]{1.5em}{1em}{\textsuperscript{\thefootnotemark}\,}
\RequirePackage{fnpct}
\setfnpct{after-punct-space={-.2em}}

% koma-hacks
\AtEndPreamble{\RequirePackage{scrhack}}

\setkomafont{pageheadfoot}{\firamedium}

\newkomafont{listlabel}{\sffamily\mdseries\color{P@GrayFG}}
\addtokomafont{descriptionlabel}{\sffamily\color{P@GrayFG}}

\setlength\leftmargini  {1.2\parindent}
\setlength\leftmarginii {1.2em}
\setlength\leftmarginiii{1.2em}
\setlength\leftmarginiv {1.2em}
\setlength\leftmarginv  {1.2em}
\setlength\leftmarginvi {1.2em}
\setlength{\leftmargin}{\leftmargini}

\newcommand*\P@Bullet{\color{P@GrayFG}\rule[1.5pt]{3pt}{3pt}}
\renewcommand*\labelenumi{\usekomafont{listlabel}\theenumi.}
\renewcommand*\labelenumii{\usekomafont{listlabel}\theenumii.}
\renewcommand*\labelenumiii{\usekomafont{listlabel}\theenumiii.}
\renewcommand*\labelenumiv{\usekomafont{listlabel}\theenumiv.}
\renewcommand*\labelitemi{\usekomafont{listlabel}\P@Bullet}%
\renewcommand*\labelitemii{\usekomafont{listlabel}--}%
\renewcommand*\labelitemiii{\usekomafont{listlabel}\raisebox{-0.3ex}{\textasteriskcentered}}%
\renewcommand*\labelitemiv{\usekomafont{listlabel}\textperiodcentered}%

\renewcommand*{\@list@extra}{%
  \topsep 2pt\parsep\parskip\itemsep 2pt
}


\AtEndPreamble{%
  \RedeclareSectionCommand[%
    beforeskip=-2\baselineskip,
    afterskip=1\baselineskip]{section}
  \RedeclareSectionCommand[%
    beforeskip=-1\baselineskip,
    afterskip=1\baselineskip]{subsection}
  \RedeclareSectionCommand[%
    beforeskip=-1\baselineskip,
    afterskip=1sp plus -1sp minus 1sp]{subsubsection}
  \RedeclareSectionCommand[%
    beforeskip=1\baselineskip]{paragraph}
  \RedeclareSectionCommand[%
    beforeskip=1\baselineskip]{subparagraph}
}

\providecommand*\hairspace{\ifmmode\mskip1mu\else\kern0.08em\fi}
\providecommand*\email[1]{\href{mailto:#1}{\nolinkurl{#1}}\xspace}
\newenvironment{wide}
    {\list{}{%
            \leftmargin-1.5cm
            \rightmargin\leftmargin}
        \item\relax}
    {\endlist}

\newcommand*\acks{\paragraph*{Acknowledgements}}


\renewcommand*{\sectionformat}{% 
  \colorbox{P@Blue}{%
    {\hspace*{.15em}%
      \color{P@ColorOnBlue}%
      \large\thesection\autodot%
      \hspace*{.17em}}}%
  \quad}

\usepackage{scrlayer-scrpage}
\pagestyle{scrheadings}
\clearpairofpagestyles


\newlength{\P@corebaselineskip}
\setlength{\P@corebaselineskip}{\baselineskip}
\newcommand*\P@pagemarkseparator{\kern.06667em :\kern.06667em\relax}


\AtBeginDocument{%
  \setlength{\P@corebaselineskip}{\baselineskip}}

\RequirePackage{comment}
\excludecomment{CCSXML}


\let\@ACMccs\@empty
\newcommand*\ccsdesc[2][100]{%
  \@@ACMccs#2@#1@}
\def\@@ACMccs#1~#2@#3@{%
  \def\@tempa{\relax}%
  \ifnumgreater{#3}{299}{\def\@tempa{\itshape}}{}%
  \ifnumgreater{#3}{499}{\def\@tempa{\bfseries}}{}%
  %
  \ifcsdef{@ACMccs@#1}{}{%
    \csgdef{@ACMccs@#1}{\item{}\textbf{#1} → }
    \gappto\@ACMccs{\csuse{@ACMccs@#1}}}%
  \ifstrempty{#2}{}{%
    \csxappto{@ACMccs@#1}{\begingroup\expandonce\@tempa}
    \csgappto{@ACMccs@#1}{#2\endgroup; }}}

\let\P@keywords\@empty
\newcommand*\keywords[1]{\gdef\P@keywords{#1}}

\addtokomafont{caption}{\small}
\addtokomafont{captionlabel}{\sffamily\bfseries}
\renewcommand*{\captionformat}{\quad}

\newcommand*\P@BlueBox{%
  \kern0.05em{\color{P@Blue}\rule{0.73em}{0.73em}}%
  \hspace*{0.67em}}
\DeclareCaptionLabelFormat{withbox}{%
  \P@BlueBox\bothIfFirst{#1}{~}#2}
\captionsetup{labelformat=withbox,singlelinecheck=false}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Authors and Affiliation
\newkomafont{affiliation}{\footnotesize}

\providecommand*\P@last@author{}
\providecommand*\P@authors{}
\newcounter{P@authors}
\providecommand*\P@affiliations{}
\newcounter{P@affiliations}
\providecommand*\P@authorinfos{}


\newcommand*\P@author@link[1]{%
  \texorpdfstring%
    {\hyperlink{P:authorinfo:\detokenize{#1}}{#1}}%
    {#1}}%

\newcommand*\P@author@[2][]{%
  \gdef\P@last@author{#2}%
  \listgadd\P@authors{%
    \P@author@link{#2}\ignorespaces%
    \P@affil{#1}\relax}%
  \stepcounter{P@authors}%
  \P@author@serialize%
}
\newcommand*\P@author@@{%
  \addtocounter{P@affiliations}{1}%
  \edef\@tempa{\alph{P@affiliations}}%
  \addtocounter{P@affiliations}{-1}%
  \expandafter\P@author@\expandafter[\@tempa]}

\renewcommand*\author{%
  \@ifnextchar[ %]
  \P@author@%
  \P@author@@}

% Long.
\newcommand\authorinfo[2][]{%
  \expandafter\listgadd%
  \expandafter\P@authorinfos%
  \expandafter{%
    \expandafter{\P@last@author}{#1}{#2}}}


\newlength{\P@authorinfo@imagewidth}
\newlength{\P@current@parindent}
\setlength{\P@authorinfo@imagewidth}{2.5cm}
\newcommand\P@makeauthorinfo[3]{%
  \begingroup%
  \setlength\P@current@parindent\parindent
  \noindent%
  \begin{minipage}[t]{\dimexpr\linewidth - \P@authorinfo@imagewidth - 2\columnsep\relax}
    \setlength\parindent\P@current@parindent
    \vspace*{0pt}%
    \noindent%
    \strut\hypertarget{P:authorinfo:\detokenize{#1}}{\textbf{#1}}\space\ignorespaces #3\par
  \end{minipage}%
  \hspace*{1.5\columnsep}%
  \begin{minipage}[t]{\P@authorinfo@imagewidth}%
    \vspace*{0pt}
    \ifstrempty{#2}{\hfil}{%
      \includegraphics[width=\P@authorinfo@imagewidth]{#2}}%
  \end{minipage}%
  \par\vspace{\baselineskip}
  \endgroup}

\providecommand*\abouttheauthorsname{%
  About the author\ifnum 1<\value{P@authors}s\fi}

\newcommand*\P@printauthorinfos{%
  \ifdefvoid{\P@authorinfos}{}\P@@printauthorinfos}
\newcommand*\P@@printauthorinfos{%
  \begingroup
  \clearpage%
  \phantomsection
  \addcontentsline{toc}{section}{\abouttheauthorsname}
  \section*{\abouttheauthorsname}
  \renewcommand\do[1]{\P@makeauthorinfo##1}
  \dolistloop{\P@authorinfos}
  \endgroup}
\AtEndDocument{\P@printauthorinfos}

\newcommand*\affiliation[2][]{%
  \begingroup
  \def\@tempa{#1}
  \stepcounter{P@affiliations}%
  \ifdefempty{\@tempa}{\edef\@tempa{\alph{P@affiliations}}}{}%
  \expandafter\listgadd%
  \expandafter\P@affiliations%
  \expandafter{%
    \expandafter{\@tempa}{#2}}
  \endgroup}

\newcommand*\printaffiliations{%
  \begin{list}{}{%
      \setlength{\topsep}{0pt}%
      \setlength{\partopsep}{0pt}%
      \setlength{\parskip}{0pt}%
      \setlength{\labelsep}{0pt}%
      \setlength{\labelwidth}{1.5em}%
      \setlength{\leftmargin}{\labelwidth}}
    \def\makelabel##1{\rlap{##1}\hss}
    \def\@tempa##1##2{\item[##1]{##2}}
    \renewcommand*{\do}[1]{\@tempa##1}
    \dolistloop{\P@affiliations}
  \end{list}\ignorespaces}


\newcommand*\P@and{,\space}
\newcommand*\P@midand{\space and\space}
\newcommand*\P@lastand{, and\space}
\let\and\P@and
\let\midand\P@midand
\let\lastand\P@lastand
\let\P@affil\@gobble

\newcommand\P@author@serialize{%
  \gdef\@author{}
  \def\P@i{1}
  \forlistloop{%
    \ifnumequal{1}{\P@i}%
      {}% nothing
      {\ifnumless{\P@i}{\value{P@authors}}%
        {\xappto\@author\and}%
        {\ifnumequal{2}{\P@i}%
          {\xappto\@author\midand}%
          {\xappto\@author\lastand}}}
    \edef\P@i{\number\expandafter\numexpr\P@i + 1\relax}
    \gappto\@author}{\P@authors}}


\newtoks\authorrunning
\newtoks\titlerunning
\providecommand*\@authorrunning{\@author}
\providecommand*\@titlerunning{\@title}
\rohead{\usekomafont{pageheadfoot}\@authorrunning}
\lehead{\usekomafont{pageheadfoot}\@titlerunning}
\setkomafont{pagenumber}{\firamedium}
\cfoot{%
  \ifdefvoid\P@ArticleNumber{}{%
    \sffamily\mdseries\P@ArticleNumber\P@pagemarkseparator}%
  \pagemark}


%%
%%
\newcommand*\P@firstpage{\pageref{P:firstpage}}
\newcommand*\P@lastpage{\pageref{TotPages}}

\defpagestyle{P@titlepage}{{}{}{}}{%
{}{%
  \begin{minipage}[b][0pt][b]{\textwidth}
    \null\noindent%
    \normalfont%
    \hspace{-1mm}\P@doclicenseImage%
    \hfill%
    \begin{minipage}[b]%
      {\dimexpr\linewidth - \doclicenseimagewidth - 1\columnsep\relax}%
      \ignorespaces
      \scriptsize%
      \textcopyright\ {\@authorrunning}\\
      \doclicenseText\\
      \ifboolexpr{bool{P@submission}}%
        {Submitted to \textit{\P@banner}.}%
        {In \textit{\P@banner}\ignorespaces%
          \ifcsvoid{P@Volume}{}{, vol.\,\P@Volume\ignorespaces}%
          \ifcsvoid{P@Issue}{}{, \IfInteger{\P@Issue}{no.\,}{}\P@Issue\ignorespaces}%
          \ifcsvoid{P@Year}{}{, \P@Year\ignorespaces}%
          \ifcsvoid{P@ArticleNumber}{}{, article \P@ArticleNumber}%
          ; \P@lastpage\space pages.}%
    \end{minipage}%
  \end{minipage}
}{}
}
\renewcommand*{\titlepagestyle}{P@titlepage}

\addtokomafont{title}{\Large\sffamily\bfseries\mathversion{bold}}
\addtokomafont{author}{\large}
\addtokomafont{subject}{\sffamily\itshape\color{P@GrayFG}}

\renewcommand*\@maketitle{%
  \label{P:firstpage}%
  \unskip% just in case
  \begingroup
  \parindent\z@ \raggedright%
  \normalfont%
  \raggedright%
  \ifdefvoid\@subject%
    {}%
    {\vspace*{-2.5\baselineskip}%
      {\usekomafont{subject}\selectfont%
          \@subject \par}}%
  {\usekomafont{title}\@title\par}%
  \ifdefvoid\@subtitle{}{%
    {\usekomafont{subtitle}\selectfont%
      \vskip .5\baselineskip%
      \@subtitle \par}}
  \bigskip%
  %
  {\let\P@affil\textsuperscript
    \usekomafont{author}\@author \par}
  \smallskip%
  %
  {\usekomafont{affiliation}%
    \printaffiliations}%
  \endgroup
  % 
  \if!\the\authorrunning!\relax\else
    \xdef\@authorrunning{\the\authorrunning}\fi
  \if!\the\titlerunning!\else\relax
    \xdef\@titlerunning{\the\titlerunning}\fi
  \begingroup
  \let\@@author\@author
  \def\and{, }\def\midand{, }\def\lastand{, }
  \P@author@serialize
  \hypersetup{pdfauthor=\@author}
  \global\let\@author\@@author
  \hypersetup{pdfkeywords = {\P@keywords}}
  \ifdefvoid{\@subtitle}%
    {\hypersetup{pdftitle={\@title}}}%
    {\hypersetup{pdftitle={\@title: \@subtitle}}}%
  \relax%
  \hypersetup{pdfcreator={programming},pdfproducer={programming}}% WOAMI
  \ifboolexpr{bool{pdf}}%
    {\ifboolexpr{bool{luatex}}%
      {\gdef\P@aftertitle{\directlua{pdf.setsuppressoptionalinfo(15)}}}%
      {\gdef\P@aftertitle{\ifx\pdfsuppressptexinfo\undefined\relax\else\global\pdfsuppressptexinfo=-1\fi}}}%
    {\gdef\P@aftertitle{}}%
    \aftergroup\P@aftertitle\relax
  \endgroup%
  \global\let\P@affil\@gobble%
}

\newcommand*\P@headersection[1]{%
  \par\vspace*{.5\baselineskip}%
  \footnotesize\sffamily%
  \noindent{\bfseries\color{P@GrayFG} #1}\enspace}

\newkomafont{abstract}{\fontsize{8.5}{10}\selectfont}
\renewenvironment{abstract}%
  {\par\smallskip%
    \begingroup
    \newcommand\P@gobblepar[1]{\@ifnextchar\par{\P@gobblepar}{}}
    {\P@headersection{\abstractname}}
    \begingroup\usekomafont{abstract}
    \@ifnextchar\par{\P@gobblepar}{}%
}{%
  \par
  \endgroup
  \begingroup
  \raggedright
  \ifcsvoid{@ACMccs}{}{%
    {\P@headersection{ACM CCS 2012}      
      \begin{itemize}\@ACMccs\end{itemize}%
      \vspace{-\topsep}}}%
  \ifcsvoid{P@keywords}{}{%
    {\P@headersection{Keywords}
    \P@keywords}}%
  \par%
  \vfill%
  \noindent\null\begin{minipage}[t][6\P@corebaselineskip][t]{\linewidth}{}%
    \vspace*{0pt}%
    \ifthenelse{\value{page} > 1}{\P@warning{%
        Your abstract seems to extend over the first page. Please consider
        adding \string\smaller\ right after \string\begin{abstract}.
      }}{}%
  {\microtypesetup{activate=false}%
    \Large\selectfont \P@banner\par}
  {\color{P@Blue}\rule{\linewidth}{.5\P@corebaselineskip}\par}%
  \sffamily
    \ifboolexpr{bool{P@submission}}{%
      {{\P@headersection{\rlap{Perspective}\phantom{Area of Submission}}}{\normalsize\P@perspective}}\strut%
      {{\P@headersection{Area of Submission}}{\normalsize\P@area}}\strut%
    }{%
      \DTMifsaveddate{P@date@submitted}{%
        {{\P@headersection{{Submitted}}}
            \DTMUsedate{P@date@submitted}}}{}\strut%
      \DTMifsaveddate{P@date@published}{%
        {{{\P@headersection{\rlap{Published}\phantom{Submitted}}}
            \DTMUsedate{P@date@published}}}}{}\strut%
      \ifcsvoid{P@DOI}{}{%
        {{\P@headersection{\rlap{\textsc{doi}}\phantom{Submitted}}}
          \href{\P@DOI@BASE\P@DOI}{\P@DOI}}}%
    }\par
    \vfill
  \end{minipage}
  % \setlength{\skip\footins}{.5\baselineskip}
  \endgroup
  \endgroup
  \clearpage
  \ignorespaces}

% From the TeX FAQ
\renewcommand{\topfraction}{.85}
\renewcommand{\bottomfraction}{.7}
\renewcommand{\textfraction}{.15}
\renewcommand{\floatpagefraction}{.66}
\renewcommand{\dbltopfraction}{.66}
\renewcommand{\dblfloatpagefraction}{.66}
\setcounter{topnumber}{9}
\setcounter{bottomnumber}{9}
\setcounter{totalnumber}{20}
\setcounter{dbltopnumber}{9}

% \emergencystretch=1em
\clubpenalty10000
\widowpenalty10000
\displaywidowpenalty=10000
\frenchspacing
\raggedbottom

\tolerance=100
\emergencystretch=2pt

\endinput
% EOF
