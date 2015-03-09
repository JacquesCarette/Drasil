module Body where
import Text.PrettyPrint
import Config
import Chunk
import H_g
import H_c
import SI_Units
import Helpers


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
  (getWFormat si_units ["m","kg","s","K","$^oC$","J","cal","mol","W"] 
    (text "& \\blt") dbs) ++ [text "\\end{longtable}"])
    
s2 = sec "Table of Symbols"

s2_intro = text "The table that follows summarizes the symbols used in this document along with their units.  The choice of symbols was made with the goal of being consistent with the nuclear physics literature and that used in the FP manual.  The SI units are listed in brackets following the definition of the symbol."

s2_table =
  vcat ([text "\\begin{longtable}{l p{10.5cm}}", (get "Symbol" h_c) <+> text "& \\blt" <+> (get "Description" h_c) <+> (get "SIU" h_c) <> dbs, (get "Symbol" h_g) <+> text "& \\blt" <+> (get "Description" h_g) <+> (get "SIU" h_g), text "\\end{longtable}"])

s3 = sec "Data Definitions"
  
srsBody = vcat [s1,s1_intro,s1_table,s2,s2_intro,s2_table,s3,
          text "~\\newline \\noindent \\begin{minipage}{\\textwidth} \\begin{tabular}{p{\\colAwidth} p{\\colBwidth}} \\toprule \\textbf{Number} & \\textbf{DD\\refstepcounter{datadefnum}\\thedatadefnum} \\label{hg}\\\\ \\midrule Label &"<+>get "Symbol" h_g<> text "\\\\ \\midrule Units & $ML^0t^{-3}T^{-1}$\\\\ \\midrule SI equivalent & $\\mathrm{\\frac{kW}{m^{2\\circ} C}}$\\\\ \\midrule Equation &"<+>get "Symbol" h_g<+> eq <+> dlr <> get "Equation" h_g <> dlr <> text "\\\\ \\midrule Description &"<+>get "Symbol" h_g<+> text "is the  gap conductance\\newline $\\tau_c$ is the clad thickness\\newline $h_p$ is initial gap film conductance\\newline $k_c$ is the clad conductivity\\newline NOTE: Equation taken from the code\\\\ \\midrule  Sources & source code\\\\ \\bottomrule \\end{tabular} \\end{minipage}\\\\  ~\\newline \\noindent \\begin{minipage}{\\textwidth} \\begin{tabular}{p{\\colAwidth} p{\\colBwidth}} \\toprule \\textbf{Number} & \\textbf{DD\\refstepcounter{datadefnum}\\thedatadefnum \\label{hc}}\\\\ \\midrule Label & "<>get "Symbol" h_c<> text "\\\\ \\midrule Units & $ML^0t^{-3}T^{-1}$\\\\ \\midrule SI equivalent & $\\mathrm{\\frac{kW}{m^{2o}C}}$\\\\ \\midrule Equation &"<+>get "Symbol" h_c<+> eq <> dlr <> get "Equation" h_c <> dlr <> text "\\\\ \\midrule Description & "<+>get "Symbol" h_c<+> text "is the  effective heat transfer coefficient between the clad and the coolant \\newline $\\tau_c$ is the clad thickness\\newline $h_b$ is initial coolant film conductance\\newline $k_c$ is the clad conductivity\\newline NOTE: Equation taken from the code\\\\ \\midrule  Sources & source code \\\\ \\bottomrule \\end{tabular} \\end{minipage}\\\\ "]

lpmBody = text "@ First we define the overall structure of the library of functions.\n@c\n@<Header files@>@/\n@<Functions@>@/\n@ Although not necessary for this simple example, we will include themath library, since future expansion will likely require it.\n@<Header files@>=\n#include <math.h>\n@ This library will consist of a set of functions.\n@<Functions@>=\n@<Function to Calculate hg@>@/\n@<Function to Calculate hc@>@/\n@ DD\\ref{L-hc} in the SRS gives the heat transfer coefficient ("<>get "Symbol" h_c<> text ") as: \n\\begin{equation}\nh_{c} ="<> get "Equation" h_c <> text ", \\label{eq:hc}\n\\end{equation} \nThe corresponding C code is given by:\n@<Function to Calculate hc@>=\ndouble calc_hc(double k_c, double h_p, double tau_c)\n{\n return (2*(k_c)*(h_p)) / ((2*(k_c))+(tau_c*(h_p)));\n}\n@ DD\\ref{L-hg} in the SRS gives the gap conductance ("<>get "Symbol" h_g<> text ") as:\n\\begin{equation} \nh_{g} =" <> get "Equation" h_g <> text "\\label{eq:hg} \n\\end{equation}\nThe corresponding C code is given by:\n@<Function to Calculate hg@>=\ndouble calc_hg(double k_c, double h_b, double tau_c)\n{\n return (2*(k_c)*(h_b)) / ((2*(k_c)) + (tau_c*(h_b)));\n}\n@"