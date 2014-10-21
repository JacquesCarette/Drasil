\documentclass[article]{cweb-hy}

\usepackage{xr}
\externaldocument[L-]{hghc_SRS}

\title{Literate Programmer's Manual for $h_g$ and $h_c$}

\author{Spencer Smith}

\begin{document}

\maketitle

@ First we define the overall structure of the library of functions.

@c
@<Header files@>@/
@<Functions@>@/

@ Although not necessary for this simple example, we will include the
math library, since future expansion will likely require it.

@<Header files@>=
#include <math.h>

@ This library will consist of a set of functions.

@<Functions@>=
@<Function to Calculate hg@>@/
@<Function to Calculate hc@>@/

@ DD\ref{L-hc} in the SRS gives the heat transfer coefficient ($h_c$) as: 

\begin{equation}
h_{c} =\frac{ 2k_{c}h_{b}}{2k_{c}+\tau_ch_{b}}, \label{eq:hc}
\end{equation} 

The corresponding C code is given by:

@<Function to Calculate hc@>=
double calc_hc(double k_c, double h_p, double tau_c)
{
 return (2*(k_c)*(h_p)) / ((2*(k_c))+(tau_c*(h_p)));
}

@ DD\ref{L-hg} in the SRS gives the gap conductance ($h_g$) as:

\begin{equation} 
h_{g} =\frac{2k_{c}h_{p}}{2k_{c}+\tau_c h_{p}} \label{eq:hg} 
\end{equation}

The corresponding C code is given by:

@<Function to Calculate hg@>=
double calc_hg(double k_c, double h_b, double tau_c)
{
 return (2*(k_c)*(h_b)) / ((2*(k_c)) + (tau_c*(h_b)));
}

@
\end{document}