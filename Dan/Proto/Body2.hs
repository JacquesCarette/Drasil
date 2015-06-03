{-# OPTIONS -Wall #-} 
module Body2 where
import Text.PrettyPrint
import Example2
import Helpers
import TeXHelpers
import ASTInternal
import PrintTex

s1, s1_intro, s1_1, s1_1intro, s1_1table, s1_2, s1_2intro, 
  s1_2table, s3, srsBody:: Doc
s1 = sec "Reference Material"

--The breakdown is for clarity, not technical purposes.
s1_intro = text "This section records information for easy reference."

s1_1 = subsec "Table of Units"
s1_1intro = text "Throughout this document SI" <+> 
            text "(Syst\\`{e}me International d'Unit\\'{e}s) is employed as" <+> 
            text "the unit system.  In addition to the basic units, several" <+>
            text "derived units are employed as described below. For each" <+>
            text "unit, the symbol is given followed by a description of the" <+>
            text "unit with the SI name in parentheses."
s1_1table = 
  vcat ([text "~\\newline \\begin{longtable}{l p{11cm}}"] ++ 
  (printSIU si_units [Description,SIU] (text "& \\blt for") dbs) ++ 
    [text "\\end{longtable}"])
    
s1_2 = subsec "Table of Symbols"

s1_2intro = text "The table that follows summarizes the symbols used in this" <+>
           text "document along with their units.  The choice of symbols was"<+>
           text "made with the goal of being consistent with the heat" <+>
           text "transfer literature and with existing documentation for" <+>
           text "solar water heating systems. The symbols are listed in" <+>
           text "alphabetical order."

s1_2table =
  vcat ([text "\\begin{longtable}{l p{10.5cm}}", (get Symbol h_c Pg) <+> 
  text "& \\blt" <+> (get Description h_c Pg) <+> (get SIU h_c Pg) <> dbs, 
  (get Symbol h_g Pg) <+> text "& \\blt" <+> (get Description h_g Pg) <+> 
  (get SIU h_g Pg), text "\\end{longtable}"])

s3 = sec "Data Definitions"
  
srsBody = vcat [s1, s1_intro, s1_1, s1_1intro, s1_1table, s1_2, 
          s1_2intro, s1_2table, s3,
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