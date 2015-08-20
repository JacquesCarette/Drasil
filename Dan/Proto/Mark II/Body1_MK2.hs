{-# OPTIONS -Wall #-} 
module Body1_MK2 where
import Example1_MK2
import ASTInternal_MK2

s1, s1_intro, s1_table, s2, s2_intro, s2_table, s3, s3_dd1 :: LayoutObj

s1 = Section (S "Table of Units") [s1_intro, s1_table]

s1_intro = Paragraph (S "Throughout this document SI (Syst" :+: 
           (F Grave (S "e")) :+: S "me International d'Unit" :+:
           (F Acute (S "e")) :+: S "s) is employed as the unit system." :+:
           S " In addition to the basic units, several derived units are" :+: 
           S " employed as described below. For each unit, the symbol is" :+: 
           S " given followed by a description of the unit with the SI" :+: 
           S " name in parentheses.")

s1_table = Table si_units [Symbol, Description]

s2 = Section (S "Table of Symbols") [s2_intro, s2_table]

s2_intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made with the goal of being consistent with the nuclear " :+:
  S "physics literature and that used in the FP manual.  The SI " :+:
  S "units are listed in brackets following the definition of " :+:
  S "the symbol."
  
s2_table = Table [h_g,h_c] [Symbol, Description, SIU]

s3 = Section (S "Data Definitions") [s3_dd1, s3_dd2]

s3_dd1 = Definition Data h_g

s3_dd2 = Definition Data h_c

srsBody :: Document
srsBody = Document ((S "SRS for ") :+: (CS h_g) :+: (S " and ") :+: (CS h_c)) 
  (S "Spencer Smith") [s1,s2,s3] 

-- srsBody = vcat [s1,s1_intro,s1_table,s2,s2_intro,s2_table,s3,
          -- text "~\\newline \\noindent \\begin{minipage}{\\textwidth}",
          -- text "\\begin{tabular}{p{\\colAwidth} p{\\colBwidth}}",
          -- text "\\toprule \\textbf{Number} & \\textbf{DD\\refstepcounter{datadefnum}\\thedatadefnum}",
          -- text "\\label{hg}\\\\ \\midrule",
          -- text "Label &"<+>get Symbol h_g Pg<> text "\\\\ \\midrule",
          -- text "Units & $ML^0t^{-3}T^{-1}$\\\\ \\midrule",
          -- text "SI equivalent & $\\mathrm{\\frac{kW}{m^{2\\circ} C}}$\\\\",
          -- text "\\midrule Equation &"<+>get Symbol h_g Pg<+> eq <+> dlr <>
          -- get Equation h_g Eqn <> dlr <> text "\\\\ \\midrule",
          -- text "Description &"<+>get Symbol h_g Pg<+> text "is the" <+>
          -- get Description h_g Pg, text "\\newline", 
          -- (vcat $ (writeDep [Symbol,Description] h_g_dep "is the" "\\newline" Pg)),
          -- text "NOTE: Equation taken from the code\\\\ \\midrule",
          -- text "Sources & source code\\\\", 
          -- text "\\bottomrule \\end{tabular} \\end{minipage}\\\\",
          -- text "~\\newline \\noindent \\begin{minipage}{\\textwidth}",
          -- text "\\begin{tabular}{p{\\colAwidth} p{\\colBwidth}}", 
          -- text "\\toprule \\textbf{Number} & \\textbf{DD\\refstepcounter{datadefnum}\\thedatadefnum \\label{hc}}\\\\ \\midrule Label & ",
          -- get Symbol h_c Pg, text "\\\\ \\midrule", 
          -- text "Units & $ML^0t^{-3}T^{-1}$\\\\ \\midrule",
          -- text "SI equivalent & $\\mathrm{\\frac{kW}{m^{2o}C}}$\\\\ \\midrule",
          -- text "Equation &"<+>get Symbol h_c Pg<+> eq <> dlr,
          -- get Equation h_c Eqn<> dlr <> text "\\\\ \\midrule",
          -- text "Description & "<+>get Symbol h_c Pg<+> text "is the" <+>
          -- get Description h_c Pg,
          -- text "\\newline",
          -- (vcat $ (writeDep [Symbol,Description] h_c_dep "is the" "\\newline" Pg)),
          -- text "NOTE: Equation taken from the code\\\\ \\midrule  Sources & source code \\\\ \\bottomrule \\end{tabular} \\end{minipage}\\\\ "] 

-- lpmBody = 
  -- vcat 
   -- [text "@ First we define the overall structure of the library of functions.",
    -- text "@c", text "@<Header files@>@/", text "@<Functions@>@/",
    -- text "@ Although not necessary for this simple example, we will include the",
    -- text "math library, since future expansion will likely require it.",
    -- text "@<Header files@>=", text "#include <math.h>",
    -- text "@ This library will consist of a set of functions.",
    -- text "@<Functions@>=",
    -- text "@<Function to Calculate hg@>@/", text "@<Function to Calculate hc@>@/", 
    -- text "@ DD\\ref{L-hc} in the SRS gives the heat transfer coefficient (" <>
    -- get Symbol h_c Pg <> text ") as: \n\\begin{equation}\nh_{c} =" <>
    -- get Equation h_c Eqn <> text ", \\label{eq:hc}",
    -- text "\\end{equation}", text "The corresponding C code is given by:",
    -- text "@<Function to Calculate hc@>=",
    -- text (C.code h_c Calc),
    -- text "@ DD\\ref{L-hg} in the SRS gives the gap conductance (" <>
    -- get Symbol h_g Pg <> text ") as:", text "\\begin{equation}", text "h_{g} ="<>
    -- get Equation h_g Eqn<> text "\\label{eq:hg}", text "\\end{equation}",
    -- text "The corresponding C code is given by:",
    -- text "@<Function to Calculate hg@>=", 
    -- text (C.code h_g Calc),
    -- text "@"
   -- ]