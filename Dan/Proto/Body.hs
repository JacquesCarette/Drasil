{-# OPTIONS -Wall #-} 
module Body where
import Text.PrettyPrint
import Example1
import SI_Units
import Helpers
import TeXHelpers
import ASTInternal
import PrintTex
import qualified PrintC as C

s1, s1_intro, s1_table, s2, s2_intro, s2_table, s3, srsBody, lpmBody :: Doc
s1 = sec "Table of Units"

--The breakdown is for clarity, not technical purposes.
s1_intro = text "Throughout this document SI" <+> 
           text "(Syst\\`{e}me International d'Unit\\'{e}s) is employed as" <+> 
           text "the unit system.  In addition to the basic units, several" <+>
           text "derived units are employed as described below. For each" <+>
           text "unit, the symbol is given followed by a description of the" <+>
           text "unit with the SI name in parentheses."
           
s1_table = 
  vcat ([text "~\\newline \\begin{longtable}{l p{11cm}}"] ++ 
  (printSIU si_units [Description,SIU] (text "& \\blt for") dbs) ++ 
    [text "\\end{longtable}"])
    
s2 = sec "Table of Symbols"

s2_intro = text "The table that follows summarizes the symbols used in this" <+>
           text "document along with their units.  The choice of symbols was"<+>
           text "made with the goal of being consistent with the nuclear" <+>
           text "physics literature and that used in the FP manual.  The SI" <+>
           text "units are listed in brackets following the definition of" <+>
           text "the symbol."

s2_table =
  vcat ([text "\\begin{longtable}{l p{10.5cm}}", (get Symbol h_c Pg) <+> 
  text "& \\blt" <+> (get Description h_c Pg) <+> (get SIU h_c Pg) <> dbs, 
  (get Symbol h_g Pg) <+> text "& \\blt" <+> (get Description h_g Pg) <+> 
  (get SIU h_g Pg), text "\\end{longtable}"])

s3 = sec "Data Definitions"
  
srsBody = vcat [s1,s1_intro,s1_table,s2,s2_intro,s2_table,s3,
          text "~\\newline \\noindent \\begin{minipage}{\\textwidth}",
          text "\\begin{tabular}{p{\\colAwidth} p{\\colBwidth}}",
          text "\\toprule \\textbf{Number} & \\textbf{DD\\refstepcounter{datadefnum}\\thedatadefnum}",
          text "\\label{hg}\\\\ \\midrule",
          text "Label &"<+>get Symbol h_g Pg<> text "\\\\ \\midrule",
          text "Units & $ML^0t^{-3}T^{-1}$\\\\ \\midrule",
          text "SI equivalent & $\\mathrm{\\frac{kW}{m^{2\\circ} C}}$\\\\",
          text "\\midrule Equation &"<+>get Symbol h_g Pg<+> eq <+> dlr <>
          get Equation h_g Eqn <> dlr <> text "\\\\ \\midrule",
          text "Description &"<+>get Symbol h_g Pg<+> text "is the" <+>
          get Description h_g Pg, text "\\newline", 
          (vcat $ (writeDep [Symbol,Description] h_g_dep "is the" "\\newline" Pg)),
          text "NOTE: Equation taken from the code\\\\ \\midrule",
          text "Sources & source code\\\\", 
          text "\\bottomrule \\end{tabular} \\end{minipage}\\\\",
          text "~\\newline \\noindent \\begin{minipage}{\\textwidth}",
          text "\\begin{tabular}{p{\\colAwidth} p{\\colBwidth}}", 
          text "\\toprule \\textbf{Number} & \\textbf{DD\\refstepcounter{datadefnum}\\thedatadefnum \\label{hc}}\\\\ \\midrule Label & ",
          get Symbol h_c Pg, text "\\\\ \\midrule", 
          text "Units & $ML^0t^{-3}T^{-1}$\\\\ \\midrule",
          text "SI equivalent & $\\mathrm{\\frac{kW}{m^{2o}C}}$\\\\ \\midrule",
          text "Equation &"<+>get Symbol h_c Pg<+> eq <> dlr,
          get Equation h_c Eqn<> dlr <> text "\\\\ \\midrule",
          text "Description & "<+>get Symbol h_c Pg<+> text "is the" <+>
          get Description h_c Pg,
          text "\\newline",
          (vcat $ (writeDep [Symbol,Description] h_c_dep "is the" "\\newline" Pg)),
          text "NOTE: Equation taken from the code\\\\ \\midrule  Sources & source code \\\\ \\bottomrule \\end{tabular} \\end{minipage}\\\\ "]

lpmBody = 
  vcat 
   [text "@ First we define the overall structure of the library of functions.",
    text "@c", text "@<Header files@>@/", text "@<Functions@>@/",
    text "@ Although not necessary for this simple example, we will include the",
    text "math library, since future expansion will likely require it.",
    text "@<Header files@>=", text "#include <math.h>",
    text "@ This library will consist of a set of functions.",
    text "@<Functions@>=",
    text "@<Function to Calculate hg@>@/", text "@<Function to Calculate hc@>@/", 
    text "@ DD\\ref{L-hc} in the SRS gives the heat transfer coefficient (" <>
    get Symbol h_c Pg <> text ") as: \n\\begin{equation}\nh_{c} =" <>
    get Equation h_c Eqn <> text ", \\label{eq:hc}",
    text "\\end{equation}", text "The corresponding C code is given by:",
    text "@<Function to Calculate hc@>=",
    text (C.code h_c Calc),
    text "@ DD\\ref{L-hg} in the SRS gives the gap conductance (" <>
    get Symbol h_g Pg <> text ") as:", text "\\begin{equation}", text "h_{g} ="<>
    get Equation h_g Eqn<> text "\\label{eq:hg}", text "\\end{equation}",
    text "The corresponding C code is given by:",
    text "@<Function to Calculate hg@>=", 
    text (C.code h_g Calc),
    text "@"
   ]