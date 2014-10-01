\chapter{LP Manual}
@
\section{Overview}


Given relative fuel power ($q'_{\mathrm{NFRAC}}$) as input, the
function {|fuel_temp_|}
calculates the following:
\begin{enumerate}
\item Average fuel temperature ($T_1$),
\item Average Clad temperature ($T_2$), 
\item Centerline temperature ($\tclad$), 
\item Surface temperature ($T_S$), and
\item Stored fuel energy $(\Delta H(T_{\text{abs}}))$.
\item Integrated fuel power $(f_p)$.
\item Power to the coolant $(q'_{\text{out}})$.
\item Metal water reaction heat $(q'_{\text{MWR}})$.
\end{enumerate}


This function uses 
\begin{itemize}
\item the material properties of the clad and fuelpellet

\item lumped parameter methods and 

\item initial conditions
\end{itemize}
It solves the ODE’s from the SRS given by IM\ref{L-rcat}, IM\ref{L-rcct} and 
IM\ref{L-rcclt} with the initial conditions defined in IM\ref{L-InitConds},
 which are summarized below:
\begin{align}
C_1 \frac{dT_{1}}{dt} &= q'_N-\frac{T_{1}-T_2}{R_1} \label{eq:T_1}\\
C_2 \frac{dT_{2}}{dt} &=\frac{T_1-T_2}{R_1}+ q'_{\mathrm{MWR}}-\frac{T_2-T_B}{R_2} 
\label{eq:T_2}\\
C_{\text{CL}} \frac{d \tclad}{dt} &= q'_N-\frac{T_{\mathrm{CL}}-T_1}{R_{\text{CL}}}
\label{eq:T_3}
\end{align}
%
where 
\begin{itemize}
\item $T_B$ is the coolant temperature
\item $q'_{N}$ is the linear element power  ($\text{{kW}/{m}}$)
\item 
$q'_{\mathrm{MWR}}$ is metal-water reaction heat  ($\text{{kW}/{m}}$) 
\item $C_1$ is the thermal capacitance of the 
fuel ($\text{{kWs}/{(m\degree C)}}$)
\item $C_2$ is the thermal capacitance of the clad  ($\text{{kWs}/{(m\degree C)}}$)
\item $C_{\text{CL}}$ is the thermal capacitance at the centerline 
($\text{{kWs}/{(m\degree C)}}$)
\item $R_1$ is the effective thermal resistance between
 $T_1$ and $T_2$ $(\text{{m\degree C}/{kW}})$
\item $R_2$ is the effective thermal resistance between coolant and half of the 
clad $(\text{{m\degree C}/kW})$
\item $R_{\text{CL}}$ is the thermal resistance between $T_{\text{CL}}$ and $T_1$ 
$(\text{{m\degree C}/{kW}})$
\item $t$ is time
\end{itemize}

\section{Numerical Algorithm}
~\newline
Equations~\ref{eq:T_1}--\ref{eq:T_3} are of the form: 
\begin{equation}
\frac{dx}{dt}=a(x)x+b(x)u(x,t) \label{eq:DE},
\end{equation}
where $x$ is the variable under consideration. Taking the Laplace
transform of Equation~\ref{eq:DE}, 
\begin{equation}
x(s)=\frac{x_0}{s-a}+\frac{bu}{s(s-a)} \label{eq:LT}
 \end{equation}
with $x_0$ being $x(t_0)$.
To obtain the closed form solution, $a(x)$, $b(x)$ and $u(x,t)$ are assumed 
to be constant in the interval [$t$, $t+\Delta t$].
The solution to the above ODE is found by taking the inverse Laplace transform of 
Equation~\ref{eq:LT}
\begin{equation}
x(t)=x_oe^{-at}+bu\int^t_0e^{-at}dt
\end{equation}
 Denoting an approximation of $x(t_k)$ at $t_k$ by $x_{k}$ and denoting
 $\Delta t = t_{k+1}-t_k$, $k\ge 0$, we have:
\begin{equation}
x_{k+1}=x_ke^{-a_k\Delta t} + (1-e^{-a_k\Delta t})\frac{-b(x_k)u(x_k,t_k)}{a(x_k)}
 \label{eq:ODE}
\end{equation}
The following table summarizes the values of $a(x)$, $b(x)$ and $u(x,t)$ for Equations
 \ref{eq:T_1}, \ref{eq:T_2} and \ref{eq:T_3}:
\noindent
\begin{table}[H]
 \centering
 \begin{tabular}{c c c c c} \hline
Equation& $x$& $a(x)$& $b(x)$& $u(x,t)$ \\ 
 \hline
\ref{eq:T_1}& $T_1$&$-\frac{1}{R_1 C_1 }$&$\frac{T_2+q'_{N}R_1}{ R_1 C_1 }$&$1$ \\ 
  \hline
 \ref{eq:T_2}&$T_2$&$-\frac{(R_1+ R_2) }{R_1 R_2 C_2 }$&$\frac{T_1 R_2+q'_{\mathrm{MWR}}R_1 R_2+
T_B R_1}{R_1 R_2 C_2 }$&$1$\\
  \hline
\ref{eq:T_3}&$T_{\text{CL}}$&$-\frac{1}{R_3 C_{\text{CL}}}$&$\frac{T_1+q'_N R_3}
{R_3C_{\text{CL}}}
$&$1$\\
  \hline \end{tabular} 
\caption{Table of functions for ODEs representing different instance models}
 \end{table} \label{Tab:1}

\section{Algorithm}

{${\bf fuel\_temp\_}(\Delta t, q_{\text{NFRAC},k},q'_{N_{\text{max}}},r_f, f, \rho_1, 
 \rho_2, h_{ib}, h_p, \tau_g, \tau_c, T_{b}, p_{\text{dry}}, h_{\text{dry}},
\text{time}, init\_flag,
MW\_flag,n,\\ h_{b,k},q'_{N,k}, k_{c,k}, k_{\text{AV},k}, q'_{\text{MWR},k},
f_{p,k}, T_{1,k}, T_{2,k}, T_{\text{CL},k}, T_{S,k}, h_{c,k}, h_{g,k}, C_{1,k}, C_{2,k},
 C_{\text{CL},k}, c_{p,1,k}, c_{p,2,k}, c_{p,3,k}, \Delta H(T_{\text{abs},k}),\\
 \delta_{\text{ox},k}, R_{\text{ox},k}, q'_{\text{out},k}, q'_{\text{MWRI},k})$
\begin{enumerate}
\item Initialization section ($*init\_flag==1$):
\begin{itemize}
\item Input: $\Delta t$, $q_{\text{NFRAC},0}$, $q'_{N_{\text{max}}}$,$r_f$, $f$, 
 $\rho_1$, $\rho_2$,  $h_{ib}$, $h_p$, $\tau_g$, $\tau_c$, $T_{b}$, |init_flag|.

\item At $t_0$ compute $y_{0}$

 \item Output: $y_{0}$,

\end{itemize}
where $y_{0}= \{ h_{b},q'_{N}, k_{c}, k_{\text{AV}},q'_{\text{MWR}},
f_{p},T_{1}, T_{2}, T_{\text{CL}}, T_{S}, C_{1}, C_{2}, C_{\text{CL}},
  c_{p,1}, c_{p,2}, c_{p,3}, h_{c}, h_{g},\\ \Delta H(T_{\text{abs}}),
 \delta_{\text{ox}}, R_{\text{ox}}, q'_{\text{out}}, q'_{\text{MWRI}}\}$. \\ 
All elements of the set $y_0$ are evaluated at the
$0^{\text{th}}$ time step.

\item Dynamic section ($*init\_flag==0$):
@ At $t_{k+1}$, $k\ge 0$, 
\begin{itemize}

\item Input:$\Delta t$, $q_{\text{NFRAC},k+1}$, $q'_{N_{\text{max}}}$,$r_f$, $f$, 
 $\rho_1$, $\rho_2$,  $h_{ib}$, $h_p$, $\tau_g$, $\tau_c$, $T_{b}$, $p_{\text{dry}}$,
 $h_{\text{dry}}$, $\text{time}$, |init_flag|, |MW_flag|, $n$, $y_k$.
 \item compute $y_{k+1}$, update $n$ when necessary.
 \item Output: $y_{k+1}$, $n$, 
\end{itemize}
where $y_{k+1}= \{h_b, q'_{N}, k_{c}, k_{\text{AV}},q'_{\text{MWR}}, 
f_{p},T_{1}, T_{2}, T_{\text{CL}}, T_{S}, C_{1}, C_{2}, C_{\text{CL}},
 c_{p,1}, c_{p,2}, c_{p,3}, h_{c}, h_{g}, \\ \Delta H(T_{\text{abs}}),
 \delta_{\text{ox}}, R_{\text{ox}}, q'_{\text{out}},q'_{\text{MWRI}}\}$.\\
All elements of the set $y_{k+1}$ are evaluated at the $k+1^{\text{th}}$ time step.
\end{enumerate}
~\newline

\section{Overall function}

@<fuel temp function@>=
void calpro_(float *t, int *i, int *iflag, float *prpval, int * icnt);
void dryout_(float *htout, float *time);
void fuel_temp_(const float *delta, float *q_NFRAC, float *q_Nmax, float *r_f, float *f, 
float *rho_1, float *rho_2, float *h_ib, float *h_p, float *tau_g, float *tau_c,float *t_b,
float *p_dry, float *h_dry, float *time, short int *init_flag,int *MW_flag, 
int *n, float *h_b, float *q_N, float *k_c,
 float *k_AV, float *q_MWR,float *f_p, float *t_1, float *t_2, float *t_CL,float *t_S,
 float *h_c, float *h_g, float *c_1, float *c_2, float *c_CL,
 float *c_p1, float *c_p2, float *c_p3,  float *deltaHT_abs, 
 float *delta_ox,float *rate_ox, float *q_out, float *q_MWRI) 

 {
if (*init_flag)
  {
    @<initialization section@>
}
else
        {

        @<dynamic section@>
       }
}

@
The following sections show the connections between the theory and the numerical algorithm 
to the implementation.
~\newpage
\section{Naming Conventions}
$\bf{Input}$ $\bf{Parameters}$\\
{ \small
@c
void fuel_temp_(const float *delta, float *q_NFRAC, float *q_Nmax, float *r_f, float *f, 
float *rho_1, float *rho_2, float *h_ib, float *h_p, float *tau_g, float *tau_c,float *t_b,
float *p_dry, float *h_dry, float *time, short int *init_flag,int *MW_flag, 
int *n, float *h_b, float *q_N, float *k_c,
 float *k_AV, float *q_MWR,float *f_p, float *t_1, float *t_2, float *t_CL,float *t_S,
 float *h_c, float *h_g, float *c_1, float *c_2, float *c_CL,
 float *c_p1, float *c_p2, float *c_p3,  float *deltaHT_abs, 
 float *delta_ox,float *rate_ox, float *q_out, float *q_MWRI) 


@
}
@ On input, 

\begin{tabular}{ll}
parameter & stores        \\ \hline
|*delta|   & $\Delta t$    \\
|*q_NFRAC| & $q'_{\mathrm{NFRAC}}$  \\
|*q_Nmax| & $q'_{N_{\text{max}}}$ \\
|*r_f| & $r_{f}$  \\
|*f| & $f$  \\
|*rho_1| & $\rho_{1}$  \\
|*rho_2| & $\rho_{2}$  \\
|*h_ib| & $h_{\text{ib}}$\\
|*h_p| & $h_{p}$\\
|*tau_g| & $\tau_{g}$  \\
|*tau_c| & $\tau_{c}$  \\
|*t_b| & $T_{B}$  \\
|*p_dry| & $p_{\text{dry}}$  \\
|*h_dry| & $h_{\text{dry}}$  \\
|*time|& time \\
|*init_flag| & 0 or 1\\
|*MW_flag| & 0 or 1\\
|*n| & 1 or 2\\
|*h_b| & $h_{\text{ib}}$ or $h_{\text{dry}}$\\
|*q_N| & $q'_{N,k}$,   $k\ge 0$, if |!*init_flag|\\ 
|*k_c| & $k_{c,k}$,   $k\ge 0$, if |!*init_flag|\\  
|*k_AV| & $k_{\text{AV,k}}$,   $k\ge 0$, if |!*init_flag|\\
|*q_MWR| & $q'_{\mathrm{MWR},k}$,   $k\ge 0$, if |!*init_flag|\\
|*f_p|  & $P_{\text{F,SUM},k}$ $k\ge 0$, if |!*init_flag|\\  
|*t_1| & $T_{1,k}$,   $k\ge 0$, if |!*init_flag|\\   
|*t_2| & $T_{2,k}$,   $k\ge 0$, if |!*init_flag|\\   

\end{tabular}
~\newpage
\begin{tabular}{ll}
parameter & stores        \\ \hline
\\~\newline
|*t_CL| & $T_{\mathrm{CL},k}$,   $k\ge 0$, if |!*init_flag|\\   
|*t_S| & $T_{S,k}$,   $k\ge 0$, if |!*init_flag|\\     
|*h_c| & $h_{c,k}$,   $k\ge 0$, if |!*init_flag|\\ 
 |*h_g| & $h_{g,k}$,   $k\ge 0$, if |!*init_flag|\\
|*c_1| & $C_{1,k}$,   $k\ge 0$, if |!*init_flag|\\ 
|*c_2| & $C_{2,k}$,   $k\ge 0$, if |!*init_flag|\\  
|*c_CL| & $C_{\mathrm{CL},k}$,   $k\ge 0$, if |!*init_flag|\\  

|*c_p1| & $c_{p,1,k}$, $k\ge 0$, if |!*init_flag|\\  
|*c_p2| & $c_{p,2,k}$, $k\ge 0$, if |!*init_flag|\\  
|*c_p3| & $c_{p,3,k}$, $k\ge 0$, if |!*init_flag|\\ 
|*deltaHT_abs| & $\Delta H(T_{\text{abs},k})$, $k\ge 0$, if |!*init_flag|\\  
|*delta_ox| & $\delta_{\mathrm{ox},k}$,   $k\ge 0$, if |!*init_flag|  \\

|*rate_ox| &$R_{\text{ox},k}$ $k\ge 0$, if |!*init_flag|\\ 
|*q_out| & $q'_{\text{out},k}$ $k\ge 0$, if |!*init_flag|\\
|*q_MWRI| & $q'_{\mathrm{MWRI},k}$,   $k\ge 0$, if |!*init_flag|\\   
 
  
\end{tabular}
\bigskip
~\newline
For |*init_flag=1|, that is, when time step $k=0$, all the input variables with 
subscript $k$ can have any value, as they are not used in any calculations during
the initialization.\\

~\newpage
$\bf{Output}$ $\bf{Parameters}$ $\bf{from}$ $\bf{the}$ $\bf{Initialization}$ $\bf{section}$
\\
{ \small
@c
void fuel_temp_(const float *delta, float *q_NFRAC, float *q_Nmax, float *r_f, float *f, 
float *rho_1, float *rho_2, float *h_ib, float *h_p, float *tau_g, float *tau_c,float *t_b,
float *p_dry, float *h_dry, float *time, short int *init_flag,int *MW_flag, 
int *n, float *h_b, float *q_N, float *k_c,
 float *k_AV, float *q_MWR,float *f_p, float *t_1, float *t_2, float *t_CL,float *t_S,
 float *h_c, float *h_g, float *c_1, float *c_2, float *c_CL,
 float *c_p1, float *c_p2, float *c_p3,  float *deltaHT_abs, 
 float *delta_ox,float *rate_ox, float *q_out, float *q_MWRI) 
@
}

@ On output,

@ if |*init_flag == 1|, 

\begin{tabular}{ll}
parameter   & stores        \\ \hline
|*n| &1\\
|*h_b| &$h_{ib}$\\
|*q_N| & $q'_{N,0}$\\ 
|*k_c| & $k_{c,0}$\\ 
|*k_AV| & $k_{\text{AV},0}$\\
|*q_MWR| & $q'_{\mathrm{MWR},0}$\\
|*f_p|  & $P_{\text{F,SUM},0}$\\
|*t_1| & $T_{1,0}$\\    
|*t_2| & $T_{2,0}$\\ 
|*t_CL| & $T_{\text{CL},0}$\\     
|*t_S| & $T_{S,0}$\\  
|*h_c| & $h_{c,0}$\\ 
|*h_g| & $h_{g,0}$\\  
|*c_1| & $C_{1,0}$\\  
|*c_2| & $C_{2,0}$\\  
|*c_CL| & $C_{\mathrm{CL},0}$\\ 
|*c_p1| & $c_{p,1,0}$  \\
|*c_p2| & $c_{p,2,0}$  \\
|*c_p3| & $c_{p,3,0}$  \\  
|*deltaHT_abs| & $\Delta H(T_{\text{abs},0})$\\ 
|*delta_ox| & $\delta_{\mathrm{ox},0}$ \\
|*rate_ox| &$R_{\text{ox},0}$\\ 
|*q_out| & $q'_{\text{out},0}$\\ 
|*q_MWRI| & $q'_{\mathrm{MWRI},0}$\\
\end{tabular}

\bigskip
~\newpage
$\bf{Output}$ $\bf{Parameters}$ $\bf{from}$ $\bf{the}$ $\bf{Dynamic}$ $\bf{section}$
\\
{ \small
@c

@
}
@ On output,
@  If |!*init_flag|,

\begin{tabular}{ll}
parameter   & stores        \\ \hline
|*n| &1 or 2\\
|*h_b| &$h_{\text{ib}}$ or $h_{\text{dry}}$\\
|*q_N| & $q'_{N,k+1}$\\ 
|*k_c| & $k_{c,k+1}$\\ 
|*k_AV| & $k_{\mathrm{AV},k+1}$\\
|*q_MWR| & $q'_{\mathrm{MWR},k+1}$\\
|*f_p|  & $P_{\text{F,SUM},k+1}$\\
|*t_1| & $T_{1,k+1}$ \\    
|*t_2| & $T_{2,k+1}$\\   
|*t_CL| & $T_{\text{CL},k+1}$\\    
|*t_S| & $T_{S,k+1}$\\     
|*h_c| & $h_{c,k+1}$\\ 
|*h_g| & $h_{g,k+1}$\\    
|*c_1| & $C_{1,k+1}$\\    
|*c_2| & $C_{2,k+1}$\\     
|*c_CL| & $C_{\mathrm{CL},k+1}$\\ 
|*c_p1| & $c_{p,1,k+1}$  \\
|*c_p2| & $c_{p,2,k+1}$  \\
|*c_p3| & $c_{p,3,k+1}$  \\ 
|*deltaHT_abs| & $\Delta H(T_{\text{abs},k+1})$\\   
|*delta_ox| & $\delta_{\mathrm{ox},k+1}$\\ 
|*rate_ox| &$R_{\text{ox},k+1}$  \\
|*q_out| & $q'_{\text{out},k+1}$  \\
|*q_MWRI| & $q'_{\mathrm{MWRI},k+1}$\\
\end{tabular}\\
~\newline
NOTE: The |fuel_temp_| function calls two fuelpin15.f functions- `|calpro_|' which
calculates material properties and `|dryout_|' which outputs a message when 
dryout occurs. The interfaces for these functions are not specified in 
this document, but the relevant terms that they define are explained as they arise
in the documentation.
~\newpage
$\bf{Local}$ $\bf{Variables}$ $\bf{for}$ $\bf{the}$ $\bf{Effective}$ $\bf{thermal}$
$\bf{resistance}$ $\bf{in}$ $\bf{the}$ $\bf{Initialization}$ $\bf{section}$\\
{ \small
@c

@
}
\begin{tabular}{ll}
parameter   & stores        \\ \hline 
|r_1| & $R_{1,0}$\\
|r_2| & $R_{2,0}$\\
|r_3| & $R_{3,0}$\\
|r_fuel| & $R_{\text{FUEL},0}$\\    
\end{tabular}
~\newline
@
$\bf{Local}$ $\bf{Variables}$ $\bf{for}$ $\bf{the}$ $\bf{Effective}$ $\bf{thermal}$
$\bf{resistance}$ $\bf{in}$ $\bf{the}$ $\bf{Dynamic}$ $\bf{section}$\\
{ \small
@c

@
}
\begin{tabular}{ll}
parameter   & stores        \\ \hline
|r_1| & $R_{1,k+1}$\\    
|r_2| & $R_{2,k+1}$\\
|r_3| & $R_{3,k+1}$\\
|r_CL| & $R_{\mathrm{CL},k+1}$\\   
\end{tabular}\\
~\newpage


@*1 Initialization section.
In this section, we determine initial values (subscript $k=0$) for:
\[
h_b,q'_N,k_c,k_{\text{AV}},q'_{\mathrm{MWR}}, f_p,
 T_1, T_2, T_{\mathrm{CL}}, T_S, h_c, h_g, C_1,
C_2,C_{\text{CL}},c_{p,1},c_{p,2},c_{p,3},\Delta H(T_{\text{abs}}),
\delta_{\text{ox}},R_{\text{ox}}, q'_{\text{out}},q'_{\mathrm{MWRI}}  
\]
@<initialization section@>=
*n=1; /* n is used to keep track of the dryout output message in the dynamic
 section */
float pi=3.1416;
@<Calculation of $q'_{N}$@>;
@<initialization of average clad temperature $T_{2,0}$@>;
@<Calculation of $k_c$@>;
@<Calculation of heat transfer coefficient ($h_{c}$)
 and the gap conductance ($h_{g}$) @>;
@<initialization of surface temperature ($T_{S,0}$)@>;
@<convergence routine to determine $k_{\text{AV},0}$ and $T_{\text{CL},0}$ @>;
@<Calculation of $R_{1}$@>;
@<initialization of average fuel temperature $T_{1,0}$@>;
@<declaration of constants for stored energy@>;
@<$\Delta H (T_{\text{abs}})$@>;
@<Calpro function for $C_{1}$ and $c_{p,1}$ @>;
@<Calculation of $C_{1}$ and $c_{p,1}$ @>;
icnt=10;
@|/* icnt is given as an argument to the calpro() function 
for calculating the specific heats and the integrals of polynomials*/ 
@<Calpro function for $C_{2}$ and $c_{p,2}$ @>;
@<Calculation of $C_{2}$ and $c_{p,2}$ @>;
@<Calpro function for $C_{\text{CL}}$ and $c_{p,3}$ @>;
@<Calculation of $C_{\text{CL}}$ and $c_{p,3}$ @>;
@<initialization of constants for $R_{\text{ox}}$ @>;
@<Calculation of $R_{\text{ox}}$ @>;
@<Calculation of $q'_{\mathrm{MWR}}$@>; 
@<initialization of $q'_{\mathrm{MWRI}}$@>;
@<Calculation of $\delta_{\text{ox}}$ @>;
@<Calculation of $q'_{\text{out}}$ @>;
@<initialization of $f_{p,0}$ @>;
@
@*2 Computing ${q'_N}$, ${T_2}$ and ${k_c}$.\label{qt2k}
The input relative fuel power ($q'_{\mathrm{NFRAC}}$) is changed to
linear element power ($q'_{N}$) by multiplying it with the initial
 linear element rating ($q'_{\mathrm{N_{max}}}$) as given by DD\ref{L-LEP} of 
the SRS. 
\begin{equation}
q'_N = q'_{\text{NFRAC}}q'_{N_{\text{max}}};
\end{equation}
This $q'_{N}$ is used to
 determine the relevant temperatures for the fuelpin.
We evaluate linear element power as
@<Calculation of $q'_{N}$@>=
  *q_N = *q_NFRAC*(*q_Nmax);

@
 Now, we evaluate $T_2$ in steady state by first setting the time derivative term 
 of Equation~\ref{eq:T_1} to zero as follows,
\begin{equation}
\frac{T_1-T_2}{R_1}=q'_N \label{eq:tf}
\end{equation}
Next we set the time derivative term of Equation~\ref{eq:T_2} to zero and neglect the
metal water heating term to get,
\begin{equation}
\frac{T_1-T_2}{R_1}=\frac{T_2-T_B}{R_2} \label{eq:tc}
\end{equation}
Substituting Equation~\ref{eq:tf} in Equation~\ref{eq:tc} and rearranging the equation,
 we get the steady state case as:
\begin{equation}
T_{2}=T_{B}+q'_{N} R_{2} \label{eq:T2},
\end{equation}
\noindent where $R_2$ is given by DD\ref{L-R2} of the SRS as,
 \begin{equation}\\
R_2= \frac{1}{2\pi r_c h_c} \label{eq:R_2}
\end{equation}
From DD\ref{L-hc} of the SRS, we have the equation for $h_c$ as,
\begin{equation}
h_c =\frac{2k_ch_b}{2k_c+\tau_c h_b} \label{eq: hc}
\end{equation}
Substituting Equation~\ref{eq: hc} into Equation~\ref{eq:R_2}, we get,
\begin{align}
R_2&=\frac{1}{2\pi r_c\Bigl(\frac{2k_ch_b}{2k_c+\tau_c h_b}\Bigr)}\\
&=\frac{1}{2\pi r_c}\Bigl(\frac{2k_c+\tau_c h_b}{2k_ch_b}\Bigr) \label{r2}
\end{align}
The above equation cannot be evaluated directly in steady state, because $R_2$ is
 dependent on $T_2$ through the clad conductivity ($k_c$) as given by DD\ref{L-kc} 
of SRS. That is, 
\begin{equation}
k_{c} = aT_{2} + b \label{eq:kc},
\end{equation}
\\where $a$ and $b$ are given constants obtained by a least squares fit to tabulated data.
According to the Assumption A\ref{L-A_it2} of the SRS, since $T_2$ is less than $1000^oC$ 
in the initial state, the values of $a$ and $b$ are given by the Table TB\ref{L-k_c} of 
the SRS as,
 \begin{align}
a&=1.43\times 10^{-5}\\
b&=1.17\times 10^{-2} 
\end{align}
So, taking the expression for $k_c$ from Equation~\ref{eq:kc}, substituting
it into Equation~\ref{r2} gives
\begin{equation}
R_{2}=\frac{2(aT_2+b)+\tau_c h_b}{4\pi r_ch_b(aT_2+b)}, \label{eq:r2} 
\end{equation}
On further simplification, Equation~\ref{eq:r2} becomes,
\begin{equation}
R_{2}=\frac{2aT_2+2b+\tau_c h_b}{4\pi r_ch_baT_2+4\pi r_ch_bb}, \label{eq:r2o} 
\end{equation}
where $r_c$ is the outer clad radius and is obtained by the sum of fuel radius $(r_f)$,
gap thickness $(\tau_g)$ and clad thickness ($\tau_c$).
\begin{equation}
r_c = r_f + \tau_g + \tau_c \label{rc}
\end{equation}
Substituting Equation~\ref{eq:r2o} into Equation~\ref{eq:T2} and rearranging 
gives an  equation quadratic in $T_{2}$:
\begin{equation}
4 \pi r_c h_baT_{2}^2+\bigl(4\pi r_ch_b b -4\pi r_ch_b aT_{B}-2aq'_N\bigr)T_2
-(4\pi r_ch_bT_{B}b+2q'_Nb+q'_{N}h_b\tau_c)=0 \label{eq:13}
\end{equation}
The above equation has to be solved to find the 
positive root which gives $T_2$ in steady state. Simultaneously the value $k_c$ 
from Equation~\ref{eq:kc} is  also calculated.\\
@<initialization of average clad temperature $T_{2,0}$@>=
//declaration of constants 
float a=1.43E-05;
float b=1.17E-02;
 @|
// computation of clad radius \ref{rc}
float r_c = *r_f + *tau_g + *tau_c; 
@|// initializing coolant film conductance
*h_b=*h_ib;
@|
// computation of T2 in steady state     
 float C10 = 2.0*pi * r_c*(*h_b);
      float C11 =  2.0* C10 *a;
      float C12 = C10 * (2.0*b -(2.0*a * (*t_b)))-(*q_N*2.0*a);
     float C13 = -C10 * (*t_b) *2.0*b - *q_N *(2.0*b+((*h_b)*(*tau_c)));
// solving quadratic equation
 *t_2 = (-C12 + sqrt(C12*C12 - 4.0 * C11 * C13)) / (2.0 * C11);
//computation of initial clad conductivity \ref{eq:kc}
@
@<Calculation of $k_c$@>=      
 *k_c =  a*(*t_2)+b;
@ 

@*2 Computing ${h_c}$, ${h_g}$ and ${T_S}$ .\label{hcgts}
     
\noindent Using this clad conductivity ($k_c$), we compute the heat transfer coefficient 
($h_c$) and the gap conductance ($h_g$) as DD\ref{L-hc} and DD\ref{L-hg} of the SRS,
 respectively. That is,
 \begin{align}
h_{c} =\frac{ 2k_{c}h_{b}}{2k_{c}+\tau_ch_{b}}, \label{eq:hc}
\end{align}
\begin{align}
h_{g} =\frac{2k_{c}h_{p}}{2k_{c}+\tau_c h_{p}} \label{eq:hg}
\end{align}
@<Calculation of heat transfer coefficient ($h_{c}$)
 and the gap conductance ($h_{g}$) @>=
// calculation of heat transfer coefficient
 *h_c=(2*(*k_c)*(*h_b)) / ((2*(*k_c))+(*tau_c*(*h_b)));

// calculation of gap conductance
 *h_g=(2*(*k_c)*(*h_p)) / ((2*(*k_c))+(*tau_c*(*h_p)));
@
~\newline
At each time step, the surface temperature ($T_{S}$) is calculated based on 
the clad and average fuel temperatures as given by DD\ref{L-$T_S$} of the SRS as:
\begin{equation}
T_S = T_2 + \frac{T_1 - T_2}{R_1}R_3, \label{eq:ts}
\end{equation} 
where $R_3$ is calculated as given by DD\ref{L-r3} of the SRS.
\begin{equation}
R_3=\frac{1}{2\pi r_f h_{g}}
\end{equation}
The surface temperature in steady state ($T_{S,0}$) is evaluated using
 $T_{2,0}$ and by setting Equation~\ref{eq:T_1} to zero as shown in 
Equation~\ref{eq:tf}.
Substituting Equation~\ref{eq:tf} in Equation~\ref{eq:ts} gives the steady state 
case of $T_S$ as:
\begin{equation}
T_{S,0} = T_{2,0}+q'_{N,0}R_{3,0},
\end{equation}
where $R_{3,0}$ is calculated based on  $h_{g,0}$.
@<initialization of surface temperature ($T_{S,0}$)@>=
@| // calculation of $R_3$
 float r_3= 1/(2*pi*(*r_f)*(*h_g));  
@| // calculation of  $T_{S,0}$
*t_S= *t_2+(*q_N* r_3); 
@

@*2 Computing $T_{\text{CL}}$ and  $k_{\text{AV}}$ .\label{tclkav}
Given this $T_S$ and $q'_N$, the centerline temperature
 ($T_{\mathrm{CL}}$) is calculated as given by Equation~\ref{L-eq:tcl} of the SRS. 
That is, in steady state,
 \begin{equation}
 T_{\mathrm{CL}}=T_S+R_{\text{FUEL}}q'_N, \label{eq:TCL}
\end{equation}
where $R_{\text{FUEL}}$ is given by DD\ref{L-DD_Rfuel} of the SRS as,
\begin{equation}
R_{\text{FUEL}}=\frac{f}{4\pi k_{\mathrm{AV}}}, \label{eq:rfuel}
\end{equation}
\noindent where $f$ is the flux depression factor (constant obtained from the input file)
 and $k_{\mathrm{AV}}$ is the
 average fuel conductivity.

@<computation of $T_{\mathrm{CL}}$@>=
float r_fuel;
r_fuel=*f/(4.0*pi*(*k_AV));
 *t_CL= *t_S+(r_fuel*(*q_N));

 @ ~\newline
 The above computation requires the average thermal conductivity ($k_{\mathrm{AV}}$), 
but this value is not initially known.. Since $k_{\mathrm{AV}}$ is a
temperature-dependent, non-linear variable, an iterative procedure
converging on mutually consistent values for $k_{\mathrm{AV}}$ and
$T_{\mathrm{CL}}$ is needed.	
 An initial estimate of
$k_{\mathrm{AV}}$ ($k_{\mathrm{AV,est}}$) fixes the $T_{\mathrm{CL,est}}$. To update
$k_{\mathrm{AV}}$, we need an estimate of linear element power
($q'_{\mathrm{N,est}}$) which is computed from the current
$T_{\mathrm{CL,est}}$. Rewriting Equation~\ref{L-eq:54} of the SRS in terms of $q'_N$ by 
using DD\ref{L-LinearElmPower} of the SRS and taking flux depression factor
into consideration gives,
\begin{align}
\int^{T_S}_{T_{\mathrm{CL}}}dT&= \frac{-fq'_N}{2\pi r_f^2}\int^{r_f}_{0}\frac{r}{k}dr
\end{align}
Integrating the RHS we have,
\begin{align}
\int^{T_S}_{T_{\mathrm{CL}}}dT&=\frac{-fq'_N}{4\pi k} \label{eq:qkest}
\end{align}
 Rearranging Equation~\ref{eq:qkest} and integrating the LHS of the equation from 
 $T_S$ to $T_{\mathrm{CL,est}}$ generates the estimate of linear element power 
($q'_{\mathrm{N,est}}$) as,
\begin{align}
q'_{\mathrm{N,est}}&=4\pi \displaystyle{\int^{T_{\mathrm{CL,est}}}_{T_S} \frac 
{kdT}{ f}}\label{eq:18} 
\end{align} 
where $k$ is a first order polynomial function of temperature and is 
given by DD{L-pkav} of the SRS as,
\begin{equation} 
k = x_1 T + x_0
\end{equation}
Let
\begin{equation} 
K(T)=\int kdT,
\end{equation}
Hence,
\begin{align} 
q'_{\mathrm{N,est}}&=\frac{4\pi}{f}\displaystyle{[K(T)]}\scriptstyle{^{T_{\mathrm{CL,est}}}
_{T_S}},\label{eq:19}\\
&=\frac{4\pi}{f}\bigl(K(T_{\mathrm{CL,est}})-K(T_S)\bigr)\label{27} 
\end{align}
@<estimation of $q'_N$@>=
 float q_NEST;
q_NEST = ((4.0*pi) * (t_e - t_a))/(*f);
/* $t_e$ and $t_a$ are $K(T_{\mathrm{CL,est}})$ and $K(T_S)$ respectively which are
evaluated by calpro function */
 @ 
Substituting Equation~\ref{eq:rfuel} in Equation~\ref{eq:TCL} and rearranging gives,
\begin{equation}
k_{\text{AV}} = \frac{f q'_{\mathrm{N}}}{4\pi(T_{\text{CL}}-T_S)}\label{eq:26}
\end{equation}
The estimate of the element power from Equation~\ref{27} is compared to 
the actual value
 and used to update $k_{\mathrm{AV}}$. The relationship between $k_{\text{AV}}$ and
and $q'_N$ is given by the first order Taylor series expansion of $k_{\text{AV}}$ with
respect to $q'_N$ as,
 \begin{equation}
k_{\mathrm{AV,i+1}}= k_{\mathrm{AV,i}}+\frac{dk_{\text{AV}}}{dq'_{\mathrm{N}}}
\Delta q'_{\mathrm{N}} \label{tay} 
\end{equation}
Differentiating Equation~\ref{eq:26} with respect to $q'_{\mathrm{N}}$ gives,
\begin{equation}
\frac{dk_{\text{AV}}}{dq'_{\mathrm{N}}}= \frac{f}{4\pi(T_{\text{CL}}-T_S)}\label{eq:28} 
\end{equation}
The change in $q'_N$ ($\Delta q'_{{N}}$) is the difference between the estimated and
 actual values.
\begin{equation}
\Delta q'_{\mathrm{N}}=q'_{\mathrm{N,est,i}}-q'_N \label{deltaq}
\end{equation}
Substituting Equation~\ref{eq:28} and Equation~\ref{deltaq} in Equation~\ref{tay},
\begin{equation}
k_{\mathrm{AV,i+1}}= k_{\mathrm{AV,i}}+\frac{(fq'_{\mathrm{N,est,i}}-
fq'_N)}{4\pi(T_{\mathrm{CL}}-T_S)}
\end{equation}
@<update $k_{\mathrm{AV}}$@>=

*k_AV=*k_AV+(((*f*q_NEST-(*f*(*q_N))))/(4.0*pi*(*t_CL-*t_S)));
@
We compute the relative error (normalized difference between the  actual and estimated
 values) of $q'_N$ as a condition for convergence.
@<relative error computation@>=
 re=(*q_N-q_NEST)/(*q_N);
@
~\newline
Now we can put the above together to 
evaluate $k_{\text{AV},0}$ and $T_{\text{CL},0}$ using the described convergence routine.
@<convergence routine to determine $k_{\text{AV},0}$ and $T_{\text{CL},0}$ @>=
float t_a, t_e; 
int i,iflag;
int icnt;/* icnt is given as an argument to the calpro() function 
for calculating the specific heats and the integrals of polynomials*/ 
i=1;
iflag=1;
icnt=0;
float ts=*t_S;
calpro_(&ts,&i,&iflag,&t_a,&icnt);/* function calpro evaluates ${t_a}$ which is the
 integral 
of polynomial for $k_{\text{AV}}$ at $T_S$ ($ K(T_S))$ */
 int idnt=4;
@| // initial estimate of $k_{AV}$
*k_AV=0.00255;

@|  //  initial estimate of relative error for convergence
 float re;
do
{
@<computation of $T_{\mathrm{CL}}$@>;
/* function calpro evaluates the integral of polynomial for $k$ ($t_e$)
 at $T_{\text{CL}}$  */
float tcl=*t_CL;
 calpro_(&tcl,&i,&iflag,&t_e,&idnt);/* function calpro evaluates ${t_e}$ which is the 
integral of polynomial for $k_{\text{AV}}$ at $T_{\text{CL}}$ ($ K(T_{\text{CL}}))$ */
@<estimation of $q'_N$@>;
@<relative error computation@>;
if(fabsf(re)<=0.0004)
break;
@<update $k_{\mathrm{AV}}$@>;

}while(1);
@

@*2 Computing $T_1$ .\label{ct1}
With $k_{\mathrm{AV}}$ determined from the above routine, we can determine the 
average fuel temperature ($T_1$) by setting the time derivative term of 
Equation~\ref{eq:T_1} to zero. That is, in steady state,
\begin{equation}
T_1=T_2+q'_N R_1,
\end{equation}
\noindent where $R_1$ is the effective thermal resistance between
 $T_1$ and $T_2$. The value of $R_1$ is given by DD\ref{L-R1} of the SRS as:  
\begin{equation}
R_1 = \frac{f}{8\pi k_{\text{AV}}} + \frac{1}{2\pi r_f h_g} \label{R1}
\end{equation}
@<Calculation of $R_{1}$@>=
float r_1=(*f / (8*pi*(*k_AV)))+(1/(2*pi*(*r_f)*(*h_g)));
@
@<initialization of average fuel temperature $T_{1,0}$@>=

*t_1=*t_2+(*q_N * r_1);

@*2 Computing $\Delta H (T_{\text{abs}})$. \label{delht1}
 Now we compute the stored fuel energy, which depends on the average fuel temperature.
 It is the
change in fuel enthalpy from standard room temperature $(T_{std}=298 \text{K})$ to the
absolute value of the average 
fuel temperature $T_1$ and is given by DD\ref{L-FuelStoredEnergy} of the SRS as: 
\begin{align} 
\begin{split}	
\Delta H (T_{\text{abs}}) = K_0
\left(K_1 \theta \Bigl(\bigl(e^{\theta/T_{\text{abs}}}-1\bigr)^{-1} - 
\bigl(e^{\theta/T_{std}}-1\bigr)^{-1}\Bigr ) +K_2 (T_{\text{abs}}^2-T_{std}^2) +
 K_3 e^{-E_D/(R_DT_{\text{abs}})}\right),
\end{split}
\end{align}
where
 $K_0,K_1,K_2,K_3,\theta,E_D,R_D$ are constants whose values
 are given by the TB\ref{L-fse} of SRS as:\\ 
~\newline
\begin{tabular}{lll}
Constant & Value&Units \\
\hline
$K_0$  &15.496 &- \\

$K_1$ &19.145&$\mathrm{{cal}/{mole K}}$  \\

$K_2$ & $7.84733\times 10^{-4} $ & $\mathrm{{cal}/({mole K^2}})$\\

$K_3$ & $5.64373\times 10^6$ &$\mathrm{{cal}/{mole}}$\\

$\theta$ &535.285 &$\mathrm{K}$ \\
$E_D$ &$37.6946 \times 10^3$  \\
$R_D$ &1.987  \\
\end{tabular}

~\newline
@<declaration of constants for stored energy@>=
// declaration of constants
     float K0 = 15.49E-03;
     float K1 = 19.145;
     float K2 = 7.84733E-04;
     float K3 = 5.64373E06;
     float THETA = 535.285;
     float E_D = 37.6946E03;
     float R_D = 1.987;
     float t_std=298;

@
~\newline
Evaluation of the stored energy 
@<$\Delta H (T_{\text{abs}})$@>=
float t_abs;
t_abs= *t_1+273.0;
*deltaHT_abs = K0 * (K1 * THETA * ((1/(exp(THETA/t_abs)-1))
             -(1/(exp(THETA/t_std)-1)))+K2*(t_abs*t_abs-t_std*t_std)
             +K3*exp(-E_D/(R_D*t_abs)));

@
@*2 Computing $C_1$, $c_{p,1}$. \label{c1cp1} 
We initialize the thermal capacitances $C_1$, $C_2$, $C_{\mathrm{CL}}$ which will be used
 later in determining the transient temperatures in the dynamic section.
~\newline
$C_1$ is the thermal capacitance of the 
fuel ($\frac{\mathrm{kW}\text{s}}{\text{m}^o\text{C}}$) and is given by
 DD\ref{L-ThermCapTerms} of the SRS as,
\begin{equation}
C_{1} =\pi r_f^2 \rho_1 c_{p,1}, \label{eq:C_1}
\end{equation}
\noindent where
\\$\rho_1$ is the fuel density ($\mathrm{\frac{kJ}{kg^oC}}$)\\
$r_f$ is the fuel radius ($\text{m}$)\\
 $c_{p,1}$ is the specific heat corresponding to the fuel average
 temperature ($\mathrm{\frac{kJ}{kg^oC}}$)\\
$c_{p,1}$ is represented as a second order 
polynomial function of temperature and is 
given by DD\ref{L-pcps} of the SRS as,
\begin{equation} 
c_{p,1} =y_2 T^2 + y_1 T + y_0 \label{pcp}
\end{equation}
The average value of $c_{p,1}$ is explicitly obtained by finding the average $c_{p,1}$ by
integrating Equation~\ref{pcp} from $T_S$ to $T_{\mathrm{CL,est}}$ and dividing by 
$T_{\text{CL}}-T_S$. That is,
\begin{equation}
c_{p,1_{\text{AV}}} = \frac{1}{T_{\text{CL}}-T_S}\int_{T_S}^{T_\text{CL}}c_{p,1}dT 
\label{eq:cp1}
\end{equation}
Let
\begin{equation} 
C_p(T)=\int c_{p,1}dT,
\end{equation}
Hence,
\begin{align} 
c_{p,1_{\text{AV}}}&=\frac{1}{ T_{\mathrm{CL}}-T_S}\displaystyle{[C_p(T)]}\scriptstyle
{^{T_{\mathrm{CL}}}
_{T_S}},\label{eq:cp}\\
&=\frac{\bigl(C_p(T_{\mathrm{CL}})-C_p(T_S)\bigr)}{ T_{\mathrm{CL}}-T_S}
\end{align}
@<Calpro function for $C_{1}$ and $c_{p,1}$ @>=
float t_c,t_d;
@|// function calpro evaluates  $C_p(T_S)$ 
int j=2;
int jflag=3;
ts=*t_S;
float tcl=*t_CL;
calpro_(&ts,&j,&jflag,&t_c,&idnt);
@|/*function calpro evaluates $C_p(T_{\mathrm{CL}})$ */
  calpro_(&tcl,&j,&jflag,&t_d,&idnt);
@
@<Calculation of $C_{1}$ and $c_{p,1}$ @>=
@| // calculation of specific heat of the fuel 
      *c_p1 = (t_d-t_c) / (*t_CL - *t_S);
@| // calculation of  C1
 *c_1=pi *(*r_f)*(*r_f)*(*rho_1)*(*c_p1);
@
@*2 Computing $C_2$, ${c_{p,2}}$ .\label{c2cp2}
$C_2$ is the thermal capacitance of the clad ($\mathrm{\frac{kW sec}{m^oC}}$) 
and is given by DD\ref{L-ThermCapTerms} of SRS as,
\begin{equation} 
C_2 =2 \pi r_c \tau_c \rho_2 c_{p,2}, \label{eq:C_2}
\end{equation}
where 
$r_c$ is the outer clad radius ($\text{m}$)\\
$\tau_c$ is the clad thickness ($\text{m}$)\\
$c_{p,2}$ is the specific heat corresponding to the  clad  temperature
  ($\mathrm{\frac{kJ}{kg^oC}}$)\\
$\rho_2$ is the  clad density ($\mathrm{\frac{kJ}{kg^oC}}$)\\
We evaluate capacitance $C_2$ for $T_2$ as:
@<Calpro function for $C_{2}$ and $c_{p,2}$ @>=
int k=3;
int kflag=2;
@|
/* function calpro evaluates the specific heat of the clad ($c_{p,2}$) at $T_2$*/
float t2=*t_2;
float cp2;
  calpro_(&t2,&k,&kflag,&cp2,&idnt);
@
@<Calculation of $C_{2}$ and $c_{p,2}$ @>=
*c_p2=cp2;
@| // calculation of  C2
*c_2 =2*pi *r_c *(*tau_c)*(*rho_2)*(*c_p2);
@
@*2 Computing  ${C_{\text{CL}}}$, ${c_{p,3}}$ .\label{c3cp3}
$C_{\text{CL}}$ is the thermal capacitance at the centerline
 ($\mathrm{\frac{kW s}{m^oC}}$) and is given by DD\ref{L-ThermCapTerms}
 of SRS as,
\begin{equation}
C_{\text{CL}} =  \pi r_f ^2 c_{p,3}\rho_ 1, \label{C_3}
\end{equation}
where
$r_f$ is the fuel radius($\text{m}$)\\
$c_{p,3}$ is the specific heat corresponding to 
the fuel centreline temperature ($\mathrm{\frac{kJ}{kg^oC}}$).\\
$\rho_1$ is the fuel density ($\mathrm{\frac{kJ}{kg^oC}}$).\\
~\newline
We evaluate capacitance $C_{\text{CL}}$ for $T_{\mathrm{CL}}$ as:
@<Calpro function for $C_{\text{CL}}$ and $c_{p,3}$ @>=
@| /* function calpro evaluates the specific heat at the centerline ($c_{p,3}$) 
at $T_{\text{CL}}$*/
int l=2;
int lflag=2;
tcl=*t_CL;
float cp3;
 calpro_(&tcl,&l,&lflag,&cp3,&idnt);
@
@<Calculation of $C_{\text{CL}}$ and $c_{p,3}$ @>=
*c_p3=cp3;
@| // calculation of $C_{\text{CL}}$
*c_CL =pi *(*r_f)*(*r_f) *(*rho_1)*(*c_p3);
@
@*2 Computing ${\delta_{ox}}$, $R_{\text{ox}}$ and $q'_{\text{MWR}}$.\label{delroxqmwr}
The zircaloy clad material oxidizes exothermically when	exposed	to high temperature
steam, resulting in additional heat input ($q'_{\text{MWR}}$) to the clad. The rate of
 oxidization ($R_{\text{ox}}$) depends on the average clad temperature ($T_2$) and the
 thickness of the reacted zircaloy ($\delta_{ox}$) and is given by 
DD\ref{L-MetalWatReact} of the SRS as,
\begin{equation}
{R_{\text{ox}}}= \frac{A}{1.56  \delta_{ox}} e^{\frac{-B}{R(T_2+273)}},
\end{equation} 
where the values of constants $A$, $B/R$ are given by Table TB\ref{L-mwr} of the SRS.
According to the Assumption A\ref{L-A_it2} of the SRS, since $T_2$ is less than $1000^oC$
 in the initial state, the values of $A$ and $B/R$ are given as,
 \begin{align}
A&=6.48\times 10^{-8}\\
B/R&=13586.0 \label{a}
\end{align}
The thickness of the reacted zircaloy $(\delta_{\text{ox}})$ is initialized to
 $1.0\times 10^{-6}$.
@<initialization of constants for $R_{\text{ox}}$ @>=
// initialization of $\delta_{\text{ox},0}$ 
*delta_ox=1.0E-06;
@|// initialization of constants $A$ and $B/R$
float A= 6.48E-08;
float BbyR=13586.0;
@
@<Calculation of $R_{\text{ox}}$ @>=
*rate_ox=(A / (1.56*(*delta_ox))) * exp(-(BbyR) / (*t_2 + 273.0));
@
Now using this $R_{\text{ox}}$, the metal water reaction heat ($q'_{\mathrm{MWR}}$)
 can be calculated as given by DD\ref{L-MetalWatReact} of the SRS.
\begin{equation}
q'_{\mathrm{MWR}}= {R_{\text{ox}}} 2\pi r_c  \rho_2 q_r,
\end{equation}
where $q_r$ is the heat of reaction and its value (6500.0) is given by Table 
TB\ref{L-mwr} of the SRS.
The integrated metal water heat ($q'_{\mathrm{MWRI}}$) is initialized to zero.
@<Calculation of $q'_{\mathrm{MWR}}$@>=
float q_r=6500.0;
*q_MWR = *rate_ox*2*pi*r_c*(*rho_2)*q_r;
@
@<initialization of $q'_{\mathrm{MWRI}}$@>=
*q_MWRI=0.0;
@
As the reaction takes place, the clad material is oxidized and
the thickness of the reacted zircaloy clad material is found by using Euler's method
for solving an ODE.
\begin{equation}
\delta_{\text{ox},i+1}=\delta_{\text{ox},i}+\frac{d\delta_{\text{ox}}}{dt}\Delta t \label{deltaox}
\end{equation}
Since the derivative of oxidized material with respect to time is rate of oxidization,
 that is,
\begin{equation}
\frac{d\delta_{\text{ox}}}{dt}=R_{\text{ox}} \label{delrox}
\end{equation}
Substituting (\ref{delrox}) in (\ref{deltaox}),
 \begin{equation}
\delta_{\text{ox},i+1}=\delta_{\text{ox},i}+R_{\text{ox}}\Delta t \label{delta_ox}
\end{equation}
@<Calculation of $\delta_{\text{ox}}$ @>=
*delta_ox = *delta_ox + *rate_ox * (*delta);
@
@*2 Computing $q'_{\text{out}}$ and initializing $f_{p}$ .\label{qoutfp}
The output heat from the reaction is sent into the coolant. This heat to the coolant 
which is given by DD\ref{L-qout} of the SRS is normalized by $q'_{N_{\text{max}}}$ for
 easier understanding and comparission purposes. In other words the normalization
is done since this is a standard form for presenting this information. Hence, the
heat out is given as,
\begin{equation}
q'_{\text{out}}=\frac{1}{q'_{N_{\text{max}}}}\Bigl(\frac{T_2-T_B}{R_2}\Bigr),
\end{equation}
where $R_2$ is the effective resistance between coolant film and the clad
 and is given by DD\ref{L-R2} of the SRS as,
\begin{equation}
R_2=\frac{1}{2\pi r_c h_c}
\end{equation}
@<Calculation of $q'_{\text{out}}$ @>=
float r_2=1/(2*pi*r_c*(*h_c));
*q_out=(*t_2-*t_b)/(r_2*(*q_Nmax));
@
The Integrated fuel power ($f_p$) as given by DD\ref{L-IntegFuelPow}
 of the SRS, is a summation of the fuel powers at each time step.
At $t_0$, no reaction takes place and hence no fuel power is generated. So, initially
the integrated fuel power is set to zero. 
@<initialization of $f_{p,0}$ @>=
*f_p=0.0;
@
@*1 Dynamic section.
In this section, we determine transient values (subscript $k>0$) for
\[
q'_N,k_c,k_{\text{AV}},q'_{\mathrm{MWR}}, f_p,
 T_1, T_2, T_{\mathrm{CL}}, T_S, h_c, h_g, C_1,
C_2,C_{\text{CL}},c_{p,1},c_{p,2},c_{p,3},\Delta H(T_{\text{abs}}),
\delta_{\text{ox}},R_{\text{ox}}, q'_{\text{out}},q'_{\mathrm{MWRI}}  
\]
@<dynamic section@>=
float pi=3.1416;
int icnt=10;/* icnt is given as an argument to the calpro() function 
for calculating the specific heats and the integrals of polynomials*/
@<Check for dryout @>;
@<Computing $q'_{N,k+1}$@>;
@<Computing $k_{c,k+1}$@>;
@<Computing $h_{c,k+1}$ and $h_{g,k+1}$@>;
@<Computing $R_{1,k+1}$ and $R_{2,k+1}$@>;
@<Computing exponential term $e^{\frac{-\Delta t}{R_{1,k+1} C_{1,k}}}$ for $T_1$@>;
@<Computing exponential term $e^{\frac{-\Delta t (R_{1,k+1} +
 R_{2,k+1})}{R_{1,k+1} R_{2,k+1} C_{2,k}}}$ for $T_2$@>;
@<Computing exponential term $e^{\frac{-\Delta t}{R_{\text{CL},k+1} C_{\text{CL},k}}}$
 for $T_{\text{CL}}$@>;
@<Computing $T_{2,k+1}$@>;
@<Computing $T_{1,k+1}$@>;
@<Computing $T_{\text{CL},k+1}$@>;
@<Computing $T_{S,k+1}$@>;
@<Computing $\Delta H (T_{\text{abs},k+1})$@>;
@<Computing $P_{\text{F,SUM},k+1}$@>;
@<Computing $C_{1,k+1} =\pi r_f^2 \rho_1 c_{p,1,k+1}$ @>;

@<Computing $C_{2,k+1} =2 \pi r_c \tau_c \rho_2 c_{p,2,k+1}$ @>;
@<Computing $C_{\text{CL},k+1} =  \pi r_f ^2 \rho_ 1 c_{p,3,k+1}$ @>;
@<Computing $k_{\mathrm{AV},k+1}$ @>;
if(*MW_flag==1)
{
@<Computing $q'_{\text{out},k+1}$ @>;
@<Computing $R_{\text{ox},k+1}$ @>;
@<Computing $q'_{\mathrm{MWR},k+1}$ @>;
@<Computing $\delta_{\mathrm{ox},k+1}$ @>;
@<Computing $q'_{\mathrm{MWRI},k+1}$ @>;
}
@
@*2 Checking for Dryout.
We check for dryout using the condition given in DD\ref{L-dryout} of the SRS.
 If the dryout occurs, we output a message notifying the time and heat out 
at which it occured and assign the heat transfer coefficient between the fuel
surface and the coolant at dryout ($h_{\text{dry}}$) to the coolant film conductance
 ($h_b$).
@<Check for dryout @>=
//check for dryout 
 if(*q_out>=*p_dry&&*n==1)
@|
{
float qout,tym;
qout=*q_out;
tym=*time;
@|/* calling fuelpin15.f subroutine `|dryout_|' to write out fuel sheath dryout time and
 $q'_{\text{out}}$ */
dryout_(&qout,&tym);
}
 if(*q_out>=*p_dry)
@| 
{
*n=2;
@| // assigning dryout heat tranfer coefficient to the coolant conductance
*h_b = *h_dry;
}
@
@*2 Computing ${q'_{N,k+1}}$ and ${k_{c,k+1}}$.
The transient linear element power and clad conductivity are determined
 in the same way as done in \ref{qt2k}. At time $t_{k+1}$, the $q'_N$ is calculated 
based on relative fuel power ($q'_{\mathrm{NFRAC}}$) at $t_{k+1}$ and is given by 
DD\ref{L-LEP} of the SRS as,
\begin{equation}
q'_{N,k+1} = q'_{\text{NFRAC},k+1}q'_{N_{\text{max}}};
\end{equation}
We use the same chunk which calculates $q'_N$ in the initialization section to compute
${q'_{N,k+1}}$, as the piece of code is same for both steady state and the transient
state calculations.
@<Computing $q'_{N,k+1}$@>=
@<Calculation of $q'_{N}$@>;
@
The value of clad conductivity ($k_c$) at time $t_{k+1}$ depends on the average clad 
temperature ($T_2$) at $t_{\text{k}}$ and is given by DD\ref{L-kc} of the SRS as,
\begin{equation}
k_{c,k+1} = aT_{2,k} + b \label{eq:kck},
\end{equation}
where $a$ and $b$ are constants obtained by a least squares fit to tabulated data and are 
given by Table TB\ref{L-k_c} of the SRS, where Table TB\ref{L-k_c} used different values 
for $a$ and $b$ if the temperature is greater than $1000^oC$.
We evaluate value of $k_c$ at $t_{k+1}$ as,
@<Computing $k_{c,k+1}$@>=
float a,b;
if( *t_2>1000.0)
@|{
 a=2.727E-05;
 b=-1.2727E-03;
}
else
@|{
 a=1.43E-05;
 b=1.17E-02;

}
  *k_c =  a* (*t_2)+b;
@ 
@*2 Computing ${h_{c,k+1}}$ and ${h_{g,k+1}}$.
 Now using the $k_{c,k+1}$, we compute the heat transfer
 coefficient ($h_c$) and the gap conductance ($h_g$) at $t_{k+1}$ in
the same way we did in \ref{hcgts} as,
\begin{align}
h_{c,k+1} =\frac{ 2k_{c,k+1}h_{b}}{2k_{c,k+1}+\tau_ch_{b}}. \label{eq:hck}
\end{align}
\begin{align}
h_{g,k+1} =\frac{2k_{c,k+1}h_{p}}{2k_{c,k+1}+\tau_c h_{p}}. \label{eq:hgk}
\end{align}
Hence, reusing the chunk that has calculated $h_c$ and $h_g$ in the initialization
section, $h_{c,k+1}$ and $h_{g,k+1}$ are computed as,  
@<Computing $h_{c,k+1}$ and $h_{g,k+1}$@>=
@<Calculation of heat transfer coefficient ($h_{c}$)
 and the gap conductance ($h_{g}$) @>;
@
@*2 Computing ${R_{1,k+1}}$ and ${R_{2,k+1}}$.
 $R_1$ at $t_{k+1}$ is computed in the same way we did in 
\ref{ct1} by taking the value 
of $h_{g}$ at $t_{k+1}$ and $k_{\text{AV}}$ at $t_k$ as,  
\begin{align}
R_{1,k+1} = \frac{f}{8\pi k_{\text{AV},k}} + \frac{1}{2\pi r_f h_{g,k+1}}
\end{align}
So, for computing $R_{1,k+1}$, we reuse the same piece of code that computes
$R_1$ at steady state. 
~\newline
$R_2$ at $t_{k+1}$ is computed taking the value 
of $h_{c,k+1}$ and is given by DD\ref{L-R2} of SRS as,
\begin{align}
R_{2,k+1} = \frac{1}{2\pi r_c h_{c,k+1}}.
\end{align}
@<Computing $R_{1,k+1}$ and $R_{2,k+1}$@>=
@<Calculation of $R_{1}$@>;
// computation of clad radius
float r_c = *r_f + (*tau_g) + (*tau_c); 
float r_2=1.0/(2.0*pi*r_c*(*h_c));
@
@*2 Computing ${T_{2,k+1}}$.
We solve Equation~\ref{eq:T_2} for ${T_{2,k+1}}$. The value of $T_2$ at time $t_{k+1}$ is 
computed using $R_{1,k+1}$,
 $R_{2,k+1}$ and the values of $C_2$, $T_1$, $T_2$, $q'_{\mathrm{MWR}}$ at $t_{k}$.
By taking $C_2$ of Equation~\ref{eq:T_2} to the RHS and rearranging, it simplifies to,
\begin{equation}
\frac{dT_{2}}{dt} =-\frac{(R_1+ R_2) }{R_1 R_2 C_2 }T_2+\frac{T_1 R_2+
q'_{\mathrm{MWR}}R_1 R_2+T_B R_1}{R_1 R_2 C_2 } \label{eq:T2k}
\end{equation}
Comparing (\ref{eq:T2k}) with (\ref{eq:DE}) using the Table~\ref{Tab:1}, the solution to
 Equation~\ref{eq:T_2} is given as,
\begin{equation}
T_{2,k+1}=T_{2,k}e^{\frac{-\Delta t (R_{1,k+1} + R_{2,k+1})}{R_{1,k+1} R_{2,k+1} C_{2,k}}} +
 \Bigl(1-e^{\frac{-\Delta t (R_{1,k+1} + R_{2,k+1})}{R_{1,k+1} R_{2,k+1} C_{2,k}}}\Bigr)
 \frac{T_{1,k} R_{2,k+1}+q'_{\mathrm{MWR},k}R_{1,k+1} R_{2,k+1}+T_{B} R_{1,k+1}}
 {(R_{1,k+1}+ R_{2,k+1}) }
\end{equation}
@<Computing exponential term $e^{\frac{-\Delta t (R_{1,k+1} +
 R_{2,k+1})}{R_{1,k+1} R_{2,k+1} C_{2,k}}}$ for $T_2$@>=
float g = exp((-(*delta) * (r_1 + r_2)) / (r_1 * r_2 * (*c_2)));
@
@<Computing $T_{2,k+1}$@>=
*t_2= *t_2* g + ((1.0 - g) * (((*t_1 * r_2) + (*q_MWR * r_1 * r_2) + ((*t_b)*r_1)) / 
(r_1 + r_2)));
@
@*2 Computing ${T_{1,k+1}}$.
We solve Equation~\ref{eq:T_1} for ${T_{1,k+1}}$. The value of $T_1$ at time $t_{k+1}$ 
is computed using
 $R_{1,k+1}$, $T_{2,k+1}$, $q'_{N,k+1}$ and the values of  $C_1$, $T_1$ at $t_{{k}}$.
By taking $C_1$ of Equation~\ref{eq:T_1} to the RHS and rearranging, it simplifies to,
\begin{equation}
\frac{dT_{1}}{dt} =-\frac{1}{R_1 C_1 }T_1+\frac{T_2+q'_{N}R_1}{ R_1 C_1 } \label{eq:T1}
\end{equation}

Comparing Equation~\ref{eq:T1} with Equation~\ref{eq:DE}, using  Table~\ref{Tab:1}, 
the solution to Equation~\ref{eq:T_1} is given as,
\begin{equation}
T_{1,k+1}=T_{1,k}e^{\frac{-\Delta t}{R_{1,k+1} C_{1,k}}} +
 \Bigl(1-e^{\frac{-\Delta t}{R_{1,k+1} C_{1,k}}}\Bigr) (R_{1,k+1} q'_{N,k+1}+T_{2,k+1})
\end{equation}
@<Computing exponential term $e^{\frac{-\Delta t}{R_{1,k+1} C_{1,k}}}$ for $T_1$@>=
float j = exp(-(*delta) / (r_1 * (*c_1)));
@
@<Computing $T_{1,k+1}$@>=
*t_1 = j * (*t_1) + ((1.0 - j) * (r_1 * (*q_N) + (*t_2)));
@
@*2 Computing ${T_{\text{CL},k+1}}$.
Now we solve Equation~\ref{eq:T_3} for ${T_{\text{CL},k+1}}$. The value of $T_{\text{CL}}$ 
at time $t_{k+1}$ is computed 
using $T_{1,k+1}$, $q'_{N,k+1}$ and the values of  $C_{\text{CL}}$, $T_{\text{CL}}$ and
 $k_{\text{AV}}$ at $t_{k}$. 
By taking $C_{\text{CL}}$ of Equation~\ref{eq:T_3} to the RHS and rearranging, 
it simplifies to,
\begin{align}
\frac{dT_{\text{CL}}}{dt} =-\frac{1}{R_{\text{CL}} C_{\text{CL}}}T_{\text{CL}}+
\frac{T_1+q'_N R_{\text{CL}}}{R_{\text{CL}}C_{\text{CL}}}, \label{eq:T4}
\end{align} 
where $R_{\text{CL}}$ is the one half of the fuel resistance ($\rm{ R_{FUEL}}$) and is given 
by DD\ref{L-rcl} of the SRS as,
\begin{equation} 
R_{\text{CL},k+1} = \frac {f}{8 \pi k_{\mathrm{AV},k}}
\end{equation}
Comparing Equation~\ref{eq:T4} with Equation~\ref{eq:DE} and using the Table~\ref{Tab:1}, 
the solution to Equation~\ref{eq:T_3} is given as,
\begin{equation}
T_{\text{CL},k+1}=T_{\text{CL},k}e^{\frac{-\Delta t}{R_{\text{CL},k+1} C_{\text{CL},k}}} +
 \Bigl(1-e^{\frac{-\Delta t}{R_{\text{CL},k+1} C_{\text{CL},k}}}\Bigr) (R_{\text{CL},k+1} 
 q'_{N,k+1} + T_{1,k+1})
\end{equation}
@<Computing exponential term $e^{\frac{-\Delta t}{R_{\text{CL},k+1} C_{\text{CL},k}}}$
 for $T_{\text{CL}}$@>=

 float r_CL=*f/(8.0*pi*(*k_AV));// calculation of $R_{\text{CL},k+1}$
float m = exp(-(*delta) / (r_CL * (*c_CL)));// calculation of exponential term
@
@<Computing $T_{\text{CL},k+1}$@>=
*t_CL = m * (*t_CL) + ((1.0 - m) * (r_CL * (*q_N) + (*t_1)));
@
@*2 Computing ${T_{S,k+1}}$.
The value of $T_{S}$ at time $t_{k+1}$ is calculated based on 
$T_{1,k+1}$, $T_{2,k+1}$ and is given by DD\ref{L-$T_S$} of the SRS as,
\begin{equation}
T_{S,k+1} = T_{2,k+1} + \frac{T_{1,k+1} - T_{2,k+1}}{R_{1,k+1}}R_{3,k+1},
\end{equation} 
where $R_{3,k+1}$ is calculated as given by DD\ref{L-r3} of the SRS as:
\begin{equation}
R_{3,k+1}=\frac{1}{2\pi r_f h_{g,k+1}}
\end{equation}
@<Computing $T_{S,k+1}$@>=
 float r_3= 1/(2*pi*(*r_f)*(*h_g));// calculation of gap resistance
*t_S =*t_2 + ((*t_1 - *t_2)/(r_1)*(r_3));
@
@*2 Computing \bf{$\Delta H (T_{\text{abs}})$ and $P_{F,SUM}$}.
 Now we compute the transient stored fuel energy in the same way we did
 in the initialization section (\ref{delht1}). The stored fuel energy at time $t_{k+1}$ 
depends on the value of absolute value of $T_1$ at $t_{k+1}$ and is given by
 DD\ref{L-FuelStoredEnergy} of the SRS as: 
\begin{align} 
\begin{split}	
\Delta H (T_{\text{abs},k+1}) = K_0
\left(K_1 \theta \Bigl(\bigl(e^{\theta/T_{\text{abs},k+1}}-1\bigr)^{-1} - 
\bigl(e^{\theta/T_{std}}-1\bigr)^{-1}\Bigr ) +K_2 (T_{\text{abs},k+1}^2-T_{std}^2) +
 K_3 e^{-E_D/(R_DT_{\text{abs},k+1})}\right),
\end{split}
\end{align}
where the values of the constants are given in the initialization section.
Reusing the chunks that initialize the constants and compute $\Delta H (T_{\text{abs}})$ 
in the initialization section, we can compute  $\Delta H (T_{\text{abs},k+1})$ as,
@<Computing $\Delta H (T_{\text{abs},k+1})$@>=
@<declaration of constants for stored energy@>;
@<$\Delta H (T_{\text{abs}})$@>;
@
~\newline
The integrated fuel power ($P_{\text{F,SUM}}$) at each time step $t_{k+1}$,
is based on the relative fuel power $(q'_{\text{NFRAC},k+1})$ and is given 
by the numerical approximation of the integral version shown in 
DD\ref{L-IntegFuelPow} of the SRS as:
\begin{equation}
P_{\text{F,SUM},k+1}=\sum_{0}^{i=k+1} q'_{\text{NFRAC},i} \Delta t_i,
\end{equation}
where $q_{\text{NFRAC},i}$ is the relative fuel power at $t_i$.
@<Computing $P_{\text{F,SUM},k+1}$@>=
*f_p= *f_p + (*q_NFRAC * (*delta));
@
@*2 Computing ${C_1, C_2, C_3, c_{p,1}, c_{p,2}, c_{p,3}}$.
We evaluate the thermal capacitances and the specific heats in the same way we did 
in the initialization sections~\ref{c1cp1},
\ref{c2cp2} and
\ref{c3cp3}. So we reuse the chunks that have implemented the capacitances $C_1$,
 $C_2$, $C_3$ and their respective specific heats in the initialization section to
 compute the thermal capacitances and the specific heats at $t_{k+1}$.
~\newline
At time $t_{k+1}$, the average fuel specific heat ($c_{p,1}$) is computed based on
 $T_{\text{CL},k+1}$ and $T_{S,k+1}$. Taking this $c_{p,1,k+1}$, we evaluate $C_{1,k+1}$
 as given in Equation~\ref{eq:C_1}.
@<Computing $C_{1,k+1} =\pi r_f^2 \rho_1 c_{p,1,k+1}$ @>=
float t_c,t_d;
int i, iflag;
i=2;
iflag=3;
float ts=*t_S;
float tcl=*t_CL;
@|// function calpro evaluates  $C_p(T_{S,k+1})$ 
calpro_(&ts,&i,&iflag,&t_c,&icnt);
@|/*function calpro evaluates $C_p(T_{\mathrm{CL},k+1})$ */
   calpro_(&tcl,&i,&iflag,&t_d,&icnt);
@<Calculation of $C_{1}$ and $c_{p,1}$ @>;
@
~\newline
At time $t_{k+1}$, the specific heat of the clad ($c_{p,2}$) is computed based on $T_{2,k+1}$.
 Taking this $c_{p,2,k+1}$, we evaluate $C_{2,k+1}$  as given in Equation~\ref{eq:C_2}.
@<Computing $C_{2,k+1} =2 \pi r_c \tau_c \rho_2 c_{p,2,k+1}$ @>=
// calculation of specific heat of the clad ( $c_{p,2,k+1}$) at $T_2$ by calpro
i=3;
iflag=2;
float t2=*t_2;
float cp2;
   calpro_(&t2,&i,&iflag,&cp2,&icnt);
@<Calculation of $C_{2}$ and $c_{p,2}$ @>;
@
~\newline
At time $t_{k+1}$, the specific heat at the centerline ($c_{p,3}$) is computed based 
on $T_{\mathrm{CL},k+1}$. Taking this $c_{p,3,k+1}$, we evaluate $C_{\mathrm{CL},k+1}$ 
 as given in Equation~\ref{C_3}.
@<Computing $C_{\text{CL},k+1} =  \pi r_f ^2 \rho_ 1 c_{p,3,k+1}$ @>=
@| //calculation of specific heat $c_{p,3,k+1}$ at $T_{\text{CL}}$ by calpro
i=2;
iflag=2;
tcl=*t_CL;
float cp3;
 calpro_(&tcl,&i,&iflag,&cp3,&icnt);
@<Calculation of $C_{\text{CL}}$ and $c_{p,3}$ @>;
@
@*2 Computing $k_{AV}$.
 Since $k_{\mathrm{AV}}$ is represented as first order 
polynomial function of temperature, at time $t_{k+1}$, the average fuel conductivity is explicitly
 obtained by integrating that expression from $T_S$ to $T_{\mathrm{CL}}$. That is,
 \begin{align}
k_{\text{AV}} &= \int_{T_S}^{T_\text{CL}}\frac{kdT}{(T_{\text{CL}}-T_S)}\\
&=\frac{1}{(T_{\text{CL}}-T_S)}\displaystyle{[K(T)]}\scriptstyle{^{T_{\mathrm{CL}}}_{T_S}},
\end{align}  
where
\begin{equation} 
K(T)=\int kdT,
\end{equation}
Hence,
\begin{equation}
k_{\text{AV}}=\frac{K(T_{\mathrm{CL}})-K(T_S)}{(T_{\text{CL}}-T_S)}
\end{equation}
@<Computing $k_{\mathrm{AV},k+1}$ @>=
float t_a;
/*evaluation of $K(T)$  at $T_{S}$ by calpro*/
i=1;
iflag=1;
ts=*t_S;
 calpro_(&ts, &i, &iflag,&t_a,&icnt);
float t_e;
/*evaluation of $K(T)$  at
$T_{\text{CL}}$ by calpro*/
tcl=*t_CL;
 calpro_(&tcl, &i, &iflag,&t_e,&icnt);
@| // calculation of average fuel conductivity 
     *k_AV = (t_e- t_a) / (*t_CL - *t_S);
@
@*2 Computing $q'_{\text{out}}$.
We calculate the heat out ($q'_{\text{out}}$) in the same way as
done in the initialization section (\ref{qoutfp}). The heat out at time $t_{k+1}$ 
depends on the
value of $T_{2}$ and $h_c$ at $t_{k+1}$ and is given by DD\ref{L-qout} of the SRS as:
\begin{equation}
q'_{\text{out},k+1}=\frac{1}{R_{2,k+1}}\Bigl(\frac{T_{2,k+1}-T_B}
{ q'_{N_{\text{max}}}}\Bigr),
\end{equation}
where $R_{2,k+1}$ is the effective resistance between the clad and the coolant film
and is given by DD\ref{L-R2} of the SRS as,
\begin{equation}
R_{2,k+1}=\frac{1}{2\pi r_c h_{c,k+1}}
\end{equation}
Reusing the chunk calculating $q'_{\text{out}}$ from the initializing section,
we can calculate  $q'_{\text{out},k+1}$
@<Computing $q'_{\text{out},k+1}$ @>=
@<Calculation of $q'_{\text{out}}$ @>;
@

@*2 Computing \bf{rate of oxidation}.
 We calculate the rate of oxidation  ($R_{\text{ox}}$) in the same way as
we did in the initialization section (\ref{delroxqmwr}). The $R_{\text{ox}}$
 at time $t_{k+1}$ depends on the
value of $T_{2}$ at $t_{k+1}$ and $\delta_{\text{ox}}$ at $t_k$ and is given by
 DD\ref{L-MetalWatReact} of the SRS,
\begin{equation}
{R_{\text{ox},k+1}}= \frac{A}{1.56  \delta_{\text{ox},k}} e^{\frac{-B}{R
(T_{2,k+1}+273)}},
\end{equation} 
where the values of constants $A$, $B/R$ are given by Table TB\ref{L-mwr}
 of the SRS. Table TB\ref{L-mwr} uses different values for $A$ and $B/R$ if
 the temperature is greater than $1580^oC$.
We evaluate value of $R_{\text{ox}}$ at $t_{k+1}$ using the same chunk which calculates
 $R_{\text{ox}}$ during the initialization section. However, before the chunk is called,
 the assignment of values to the variables $A$ and $BbyR$ is done based on the value of
 $T_2$ as given by  Table TB\ref{L-mwr} of the SRS.
@<Computing $R_{\text{ox},k+1}$ @>=
float A,BbyR;
if(*t_2<=1580.0)
{
 A= 6.48E-08;
 BbyR=13586.0;
}
else
{
 A= 1.0E-06;
 BbyR=16014.0;
}
@<Calculation of $R_{\text{ox}}$ @>;
if(*t_2>=1850.0)
{
*rate_ox=(A / (1.56*(*delta_ox))) * exp(-(BbyR) / (1850.0 + 273.0));
}
@
@*2 Computing \bf{metal water reaction heat ($q_{\mathrm{MWR}}$)}.
We calculate the metal water reaction heat in the same way as
we did in the initialization section (\ref{delroxqmwr}). The $q_{\mathrm{MWR}}$
 at time $t_{k+1}$ 
depends on the value of $R_{\text{ox}}$ at $t_{k+1}$ and is given by
 DD\ref{L-MetalWatReact} of SRS as:
\begin{equation}
q'_{\mathrm{MWR},k+1}= R_{\text{ox},k+1} 2\pi r_c  \rho_2 q_r,
\end{equation}
So, for evaluating $q'_{\mathrm{MWR},k+1}$, we reuse the chunk that calculates
$q'_{\mathrm{MWR}}$ in the steady state.
Taking assumption A\ref{L-A_tauc} of the SRS into consideration,
 when all the clad material gets oxidized, that is, when the thickness of the
 reacted zircaloy ($\delta_{\text{ox}}$) becomes equal to or greater than the clad 
thickness ($\tau_c$), then there will not be any more metal water reaction taking place 
and hence no more $q'_{\mathrm{MWR}}$ will be generated. That is,
\begin{equation}
\delta_{\text{ox}} \geq \tau_c \Rightarrow q'_{\text{MWR}}=0
\end{equation}
@<Computing $q'_{\mathrm{MWR},k+1}$ @>=
@<Calculation of $q'_{\mathrm{MWR}}$ @>;
if(*delta_ox>=*tau_c)
{
*q_MWR=0.0;
}
@
@*2 Computing \bf{oxidation layer thickness}.
We calculate the oxidation layer thickness in the same way as
we did in the initialization section (\ref{delroxqmwr}). The $\delta_{\mathrm{ox}}$ 
at time $t_{k+1}$ 
depends on the value of $R_{\text{ox}}$ at $t_{k+1}$ and is given as:
\begin{equation}
\delta_{\text{ox},k+1}=\delta_{\text{ox},k}+R_{\text{ox},k+1}\Delta t 
\end{equation}
So, for evaluating $\delta_{\text{ox},k+1}$, we reuse the chunk that calculates
$\delta_{\text{ox}}$ in the steady state.
But once the $\delta_{\text{ox}}$ becomes equal to or greater than the clad 
thickness ($\tau_c$), as there will not be anymore metal water reaction taking 
place, the rate of oxidation of the clad becomes zero. That is,
\begin{equation}
\delta_{\text{ox}} \geq \tau_c \Rightarrow R_{\text{ox}}=0
\end{equation}
@<Computing $\delta_{\mathrm{ox},k+1}$ @>=
if(*delta_ox>=*tau_c)
{
*rate_ox=0.0;
}
@<Calculation of $\delta_{\text{ox}}$ @>;
@
@*2 Computing \bf{Integrated metal water reaction heat ($q'_{\mathrm{MWRI}}$)}.
The integrated metal water reaction heat is a summation of $q'_{\mathrm{MWR}}$ 
normalized by $q'_{N_{\text{max}}}$ at each time step.  
At time $t_{k+1}$, the $q'_{\mathrm{MWRI}}$ is based on  $q'_{\mathrm{MWR},k+1}$ 
and is given by the numerical approximation of the integral form given in
DD\ref{L-qmwri} of the SRS as,
\begin{equation}
q'_{\text{MWRI},k+1}=\frac{1}{q'_{N_{\text{max}}}}\sum_{i=0}^{k+1} q'_{\text{MWR},i}
 \Delta t_i
\end{equation}
@<Computing $q'_{\mathrm{MWRI},k+1}$ @>=
*q_MWRI=*q_MWRI+((*q_MWR/(*q_Nmax))*(*delta));
@

@ We store the program into the C file 
@(fuel_temp.c@>=
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
@<fuel temp function@>;
@


