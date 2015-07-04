{-# OPTIONS -Wall #-} 
module Example2 where
import ASTInternal
import Chunk
import SI_Units

si_units :: Chunks
si_units = [metre, kilogram, second, centigrade, joule, watt]

surface_areas, shc, dimensions, heat_energies, heat_generations, 
	heat_transfers, latent_heat_rel, masses, vectors, heat_flux,
	times, temperatures, volumes, densities, others :: Chunks

all_ex2 :: Chunks
all_ex2 = surface_areas ++ shc ++ dimensions ++ heat_energies ++ 
	heat_generations ++ heat_transfers ++ latent_heat_rel ++
	masses ++ vectors ++ heat_flux ++ times ++ temperatures ++
	volumes ++ densities ++ others
--------------- --------------- --------------- ---------------
{--------------- Begin Surface Areas ---------------}
--------------- --------------- --------------- ---------------
surface_areas = [a_C,a_in,a_P,a_out]
a_C :: Chunk
a_C = newChunk $
  [(Symbol, S "A" :-: S "C"), --Formatted symbol for documentation
   (VarName,S "a_C"),        --VarName if the symbol represents a variable
                                --Equation if the symbol can be calculated
   (Description,S "coil surface area") --Description
  ]
----------------------------------------------------------------------
a_in :: Chunk
a_in = newChunk $
  [(Symbol, S "A" :-: S "in"),
   (VarName,S "a_in"),
   (Description,S "surface area over which heat is transferred in")
  ]
----------------------------------------------------------------------  
a_P :: Chunk
a_P = newChunk $
  [(Symbol, S "A" :-: S "P"),
   (VarName,S "a_P"),
   (Description,S "phase change material surface area")
  ]
----------------------------------------------------------------------
a_out :: Chunk
a_out = newChunk $
  [(Symbol, S "A" :-: S "out"),
   (VarName,S "a_out"),
   (Description,S "surface area over which heat is transferred out")
  ]
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Specific Heat Capacities ---------------}  
--------------- --------------- --------------- ---------------
shc = [c, c__L, c_P_L, c__S, c_P_S,c_V,c_W]
c :: Chunk
c = newChunk $
  [(Symbol, S "C"),
   (VarName,S "c"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity")
  ]
-- c_dep :: Dependency
-- c_dep = get_dep h_c_eq

-- c_eq :: Expr           
-- c_eq = ((Int 2):*(C k_c):*(C h_b)) :/ ((Int 2):*(C k_c)
  -- :+((C tau_c):*(C h_b)))

----------------------------------------------------------------------

c__L :: Chunk
c__L = newChunk $
  [(Symbol, S "C" :^: S "L"),
   (VarName,S "c__L"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of a liquid")
  ]

----------------------------------------------------------------------

c_P_L :: Chunk
c_P_L = newChunk $
  [(Symbol, S "C" :-: S "P" :^: S "L"),
   (VarName,S "c_P_L"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of PCM during latent heating phase")
  ]

----------------------------------------------------------------------

c__S :: Chunk
c__S = newChunk $
  [(Symbol, S "C" :^: S "S"),
   (VarName,S "c__S"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of a solid")
  ]

----------------------------------------------------------------------

c_P_S :: Chunk
c_P_S = newChunk $
  [(Symbol, S "C" :-: S "P" :^: S "S"),
   (VarName,S "c_P_S"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of PCM during sensible heating phase")
  ]

----------------------------------------------------------------------

c_V :: Chunk
c_V = newChunk $
  [(Symbol, S "C" :-: S "V"),
   (VarName,S "c_V"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of a vapour")
  ]

----------------------------------------------------------------------

c_W :: Chunk
c_W = newChunk $
  [(Symbol, S "C" :-: S "W"),
   (VarName,S "c_W"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of water")
  ]
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Dimensions ---------------}
--------------- --------------- --------------- ---------------
dimensions = [d,l]
d :: Chunk
d = newChunk $
  [(Symbol, S "D"),
   (VarName,S "d"),
   -- (Equation, E h_c_eq),
   (Description, S "diameter of tank")
  ]
  
----------------------------------------------------------------------

l :: Chunk
l = newChunk $
  [(Symbol, S "L"),
   (VarName,S "l"),
   -- (Equation, E h_c_eq),
   (Description, S "length of tank")
  ]

--------------- --------------- --------------- ---------------
{--------------- Begin Heat Energies ---------------}
--------------- --------------- --------------- ---------------
heat_energies = [e,e_P,e_Pmelt_init,e_W]
e :: Chunk
e = newChunk $
  [(Symbol,S "E"),
   (VarName,S "e"),
-- (Equation, E h_c_eq),
   (Description, S "sensible heat energy")
  ]
  
----------------------------------------------------------------------

e_P :: Chunk
e_P = newChunk $
  [(Symbol,S "E" :-: S "P"),
   (VarName,S "e_P"),
-- (Equation, E h_c_eq),
   (Description, S "heat energy in the PCM")
  ]
  
----------------------------------------------------------------------

e_Pmelt_init :: Chunk
e_Pmelt_init = newChunk $
  [(Symbol,S "E" :-: S "Pmelt" :^: S "init"),
   (VarName,S "e_Pmelt_init"),
-- (Equation, E h_c_eq),
   (Description, S "heat energy in the PCM at the instant when melting begins")
  ]
  
----------------------------------------------------------------------

e_W :: Chunk
e_W = newChunk $
  [(Symbol,S "E" :-: S "W"),
   (VarName,S "e_W"),
-- (Equation, E h_c_eq),
   (Description, S "heat energy in the water")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Heat Generations ---------------}
--------------- --------------- --------------- ---------------
heat_generations = [g]
g :: Chunk
g = newChunk $
  [(Symbol,S "g"),
   (VarName,S "g"),
-- (Equation, E h_c_eq),
   (Description, S "volumetric heat generation per unit volume")
  ]

--------------- --------------- --------------- ---------------
{--------------- Begin Heat Transfers ---------------}
--------------- --------------- --------------- ---------------
heat_transfers = [h,h_C,h_P]

h :: Chunk
h = newChunk $
  [(Symbol,S "h"),
   (VarName,S "h"),
-- (Equation, E h_c_eq),
   (Description, S "convective heat transfer coefficient")
  ]

----------------------------------------------------------------------

h_C :: Chunk
h_C = newChunk $
  [(Symbol,S "h" :-: S "C"),
   (VarName,S "h_C"),
-- (Equation, E h_c_eq),
   (Description, S "convective heat transfer coefficient between coil and water")
  ]
  
----------------------------------------------------------------------

h_P :: Chunk
h_P = newChunk $
  [(Symbol,S "h" :-: S "P"),
   (VarName,S "h_P"),
-- (Equation, E h_c_eq),
   (Description, S "convective heat transfer coefficient between water and PCM")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Latent Heat-related ---------------}
--------------- --------------- --------------- ---------------
latent_heat_rel = [h_f, qQ, qQ_P]

h_f :: Chunk
h_f = newChunk $
  [(Symbol,S "H":-: S "f"),
   (VarName, S "h_f"),
-- (Equation, E h_c_eq),
   (Description, S "specific latent heat of fusion")
  ]

----------------------------------------------------------------------
  
--uppercase Q is a problem; there is already a "q" chunk
qQ :: Chunk
qQ = newChunk $
  [(Symbol, S "Q"),
   (VarName,S "qQ"),
   -- (Equation, E h_c_eq),
   (Description, S "latent heat energy")
  ]

qQ_P :: Chunk
qQ_P = newChunk $
  [(Symbol, S "Q" :-: S "P"),
   (VarName,S "qQ_P"),
   -- (Equation, E h_c_eq),
   (Description, S "latent heat energy added to PCM")
  ]
--------------- --------------- --------------- ---------------
{--------------- Begin Masses ---------------}
--------------- --------------- --------------- ---------------

masses = [m,m_P,m_W]
  
m :: Chunk
m = newChunk $
  [(Symbol, S "m"),
   (VarName,S "m"),
   -- (Equation, E h_c_eq),
   (Description, S "mass")
  ]
  
----------------------------------------------------------------------

m_P :: Chunk
m_P = newChunk $
  [(Symbol, S "m" :-: S "P"),
   (VarName,S "m_P"),
   -- (Equation, E h_c_eq),
   (Description, S "mass of phase change material")
  ]
  
----------------------------------------------------------------------

m_W :: Chunk
m_W = newChunk $
  [(Symbol, S "m" :-: S "W"),
   (VarName,S "m_W"),
   -- (Equation, E h_c_eq),
   (Description, S "mass of water")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Vectors ---------------}
--------------- --------------- --------------- ---------------

vectors = [nhat, qvect]

nhat :: Chunk
nhat = newChunk $
  [(Symbol, F Vector (F Hat (S "n"))),
   (VarName,S "n"),
   -- (Equation, E h_c_eq),
   (Description, S "unit outward normal vector for a surface")
  ]

----------------------------------------------------------------------
  
qvect :: Chunk
qvect = newChunk $
  [(Symbol, F Vector (S "q")),
   (VarName,S "qvect"),
   -- (Equation, E h_c_eq),
   (Description, S "thermal flux vector")
  ]
--------------- --------------- --------------- ---------------
{--------------- Begin Heat Flux ---------------}
--------------- --------------- --------------- ---------------

heat_flux = [q,q_C,q_in,q_P,q_out]

q :: Chunk
q = newChunk $
  [(Symbol, S "q"),
   (VarName,S "q"),
   -- (Equation, E h_c_eq),
   (Description, S "heat flux")
  ]

----------------------------------------------------------------------

q_C :: Chunk
q_C = newChunk $
  [(Symbol, S "q" :-: S "C"),
   (VarName,S "q_C"),
   -- (Equation, E h_c_eq),
   (Description, S "heat flux from coil")
  ]
  
----------------------------------------------------------------------

q_in :: Chunk
q_in = newChunk $
  [(Symbol, S "q" :-: S "in"),
   (VarName,S "q_in"),
   -- (Equation, E h_c_eq),
   (Description, S "heat flux in")
  ]
  
----------------------------------------------------------------------

q_P :: Chunk
q_P = newChunk $
  [(Symbol, S "q" :-: S "P"),
   (VarName,S "q_P"),
   -- (Equation, E h_c_eq),
   (Description, S "heat flux into phase change material")
  ]
  
----------------------------------------------------------------------

q_out :: Chunk
q_out = newChunk $
  [(Symbol, S "q" :-: S "out"),
   (VarName,S "q_out"),
   -- (Equation, E h_c_eq),
   (Description, S "heat flux out")
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin Times ---------------}
--------------- --------------- --------------- ---------------

times = [t,t_melt, t_final, t_melt_init, t_melt_final]

t :: Chunk
t = newChunk $
  [(Symbol, S "t"),
   (VarName,S "t"),
   -- (Equation, E h_c_eq),
   (Description, S "time")
  ]
  
----------------------------------------------------------------------

t_melt :: Chunk
t_melt = newChunk $
  [(Symbol, S "t" :-: S "melt"),
   (VarName,S "t_melt"),
   -- (Equation, E h_c_eq),
   (Description, S "time when melting of the PCM begins")
  ]
  
----------------------------------------------------------------------

t_final :: Chunk
t_final = newChunk $
  [(Symbol, S "t" :-: S "final"),
   (VarName,S "t_final"),
   -- (Equation, E h_c_eq),
   (Description, S "final time")
  ]
  
----------------------------------------------------------------------

t_melt_init :: Chunk
t_melt_init = newChunk $
  [(Symbol, S "t" :-: S "melt" :^: S "init"),
   (VarName,S "t_melt_init"),
   -- (Equation, E h_c_eq),
   (Description, S "time at which melting of the PCM begins")
  ]
  
----------------------------------------------------------------------

t_melt_final :: Chunk
t_melt_final = newChunk $
  [(Symbol, S "t" :-: S "melt" :^: S "final"),
   (VarName,S "t_melt_final"),
   -- (Equation, E h_c_eq),
   (Description, S "time at which melting of the PCM ends")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Temperatures ---------------}
--------------- --------------- --------------- ---------------

temperatures = [tT, tT_boil, tT_C, tT_env, tT_init, tT_melt, tT_melt_P, tT_W, tT_P, tT_Delta]

tT :: Chunk
tT = newChunk $
  [(Symbol, S "T"),
   (VarName,S "tT"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature")
  ]
  
----------------------------------------------------------------------

tT_boil :: Chunk
tT_boil = newChunk $
  [(Symbol, S "T" :-: S "boil"),
   (VarName,S "tT_boil"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature at boiling point")
  ]
  
----------------------------------------------------------------------

tT_C :: Chunk
tT_C = newChunk $
  [(Symbol, S "T" :-: S "C"),
   (VarName,S "tT_C"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature of coil")
  ]
  
----------------------------------------------------------------------

tT_env :: Chunk
tT_env = newChunk $
  [(Symbol, S "T" :-: S "env"),
   (VarName,S "tT_env"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature of environment")
  ]
  
----------------------------------------------------------------------

tT_init :: Chunk
tT_init = newChunk $
  [(Symbol, S "T" :-: S "init"),
   (VarName,S "tT_init"),
   -- (Equation, E h_c_eq),
   (Description, S "initial temperature")
  ]
  
----------------------------------------------------------------------

tT_melt :: Chunk
tT_melt = newChunk $
  [(Symbol, S "T" :-: S "melt"),
   (VarName,S "tT_melt"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature at melting point")
  ]
  
----------------------------------------------------------------------

tT_melt_P :: Chunk
tT_melt_P = newChunk $
  [(Symbol, S "T" :-: S "melt" :^: S "P"),
   (VarName,S "tT_melt_P"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature at melting point for PCM")
  ]
  
----------------------------------------------------------------------

tT_W :: Chunk
tT_W = newChunk $
  [(Symbol, S "T" :-: S "W"),
   (VarName,S "tT_W"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature of water")
  ]
  
----------------------------------------------------------------------

tT_P :: Chunk
tT_P = newChunk $
  [(Symbol, S "T" :-: S "P"),
   (VarName,S "tT_P"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature of phase change material")
  ]
  
----------------------------------------------------------------------

tT_Delta :: Chunk
tT_Delta = newChunk $
  [(Symbol, U Delta_U :+: S "T"),
   (VarName,S "tT_Delta"),
   -- (Equation, E h_c_eq),
   (Description, S "temperature difference")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Volumes ---------------}
--------------- --------------- --------------- ---------------

volumes = [v,v_P,v_tank,v_W]

v :: Chunk
v = newChunk $
  [(Symbol, S "V"),
   (VarName,S "v"),
   -- (Equation, E h_c_eq),
   (Description, S "volume")
  ]
  
----------------------------------------------------------------------

v_P :: Chunk
v_P = newChunk $
  [(Symbol, S "V" :-: S "P"),
   (VarName,S "v_P"),
   -- (Equation, E h_c_eq),
   (Description, S "volume of PCM")
  ]
  
----------------------------------------------------------------------

v_tank :: Chunk
v_tank = newChunk $
  [(Symbol, S "V" :-: S "tank"),
   (VarName,S "v_tank"),
   -- (Equation, E h_c_eq),
   (Description, S "volume of the cylindrical tank")
  ]
  
----------------------------------------------------------------------

v_W :: Chunk
v_W = newChunk $
  [(Symbol, S "V" :-: S "W"),
   (VarName,S "v_W"),
   -- (Equation, E h_c_eq),
   (Description, S "volume of water")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Densities ---------------}
--------------- --------------- --------------- ---------------

densities = [rho,rho_P,rho_W]

rho :: Chunk
rho = newChunk $
  [(Symbol, U Rho_L),
   (VarName,S "rho"),
   -- (Equation, E h_c_eq),
   (Description, S "density, mass per unit volume")
  ]
  
----------------------------------------------------------------------

rho_P :: Chunk
rho_P = newChunk $
  [(Symbol, U Rho_L :-: S "P"),
   (VarName,S "rho_P"),
   -- (Equation, E h_c_eq),
   (Description, S "density of PCM")
  ]
  
----------------------------------------------------------------------

rho_W :: Chunk
rho_W = newChunk $
  [(Symbol, U Rho_L :-: S "W"),
   (VarName,S "rho_W"),
   -- (Equation, E h_c_eq),
   (Description, S "density of water")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Other ---------------}
--------------- --------------- --------------- ---------------

others = [tau, phi]

tau :: Chunk
tau = newChunk $
  [(Symbol, U Tau_L),
   (VarName,S "tau"),
   -- (Equation, E h_c_eq),
   (Description, S "dummy variable for integration over time")
  ]
  
----------------------------------------------------------------------

phi :: Chunk
phi = newChunk $
  [(Symbol, U Phi_L),
   (VarName,S "phi"),
   -- (Equation, E h_c_eq),
   (Description, S "melt fraction")
  ]
  
----------------------------------------------------------------------



------------------------------------------------------------------------------------------
---------------DELETE EVERYTHING AFTER THIS LINE AS THE EXAMPLE EXPANDS-------------------
------------------------------------------------------------------------------------------


--------------- --------------- --------------- ---------------
{--------------- Begin tau_c ---------------}
--------------- --------------- --------------- ---------------
tau_c :: Chunk
tau_c = newChunk $
  [(Symbol,U Tau_L :-: S "c"), --Formatted symbol for documentation
   (VarName,S "tau_c"),        --VarName if the symbol represents a variable
                                --Equation if the symbol can be calculated
   (Description,S "clad thickness") --Description
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin h_c ---------------}  
--------------- --------------- --------------- ---------------
h_c :: Chunk
h_c = newChunk $
  [(Symbol, S "h" :-: S "c"),
   (Equation, E h_c_eq),
   (Description, S 
    "convective heat transfer coefficient between clad and coolant"),
   (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)")
  ]

h_c_dep :: Dependency
h_c_dep = get_dep h_c_eq

h_c_eq :: Expr           
h_c_eq = ((Int 2):*(C k_c):*(C h_b)) :/ ((Int 2):*(C k_c)
  :+((C tau_c):*(C h_b)))


--------------- --------------- --------------- ---------------
{--------------- Begin h_g ---------------}
--------------- --------------- --------------- ---------------
h_g :: Chunk
h_g = newChunk $
  [(Symbol, S "h" :-: S "g"),
   (Equation, E h_g_eq),
   (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   (Description, S
    "effective heat transfer coefficient between clad and fuel surface")
  ]
  
h_g_dep :: Dependency
h_g_dep = get_dep h_g_eq

h_g_eq :: Expr           
h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

--------------- --------------- --------------- ---------------
{--------------- Begin h_b ---------------}
--------------- --------------- --------------- ---------------

h_b :: Chunk
h_b = newChunk $
  [(Symbol,S "h" :-: S "b"),
   (VarName,S "h_b"),
   (Description, S "initial coolant film conductance")
  ]
--------------- --------------- --------------- ---------------
{--------------- Begin h_p ---------------}
--------------- --------------- --------------- ---------------

h_p :: Chunk
h_p = newChunk $
  [(Symbol, S "h":-: S "p"),
   (VarName, S "h_p"),
   (Description, S "initial gap film conductance")
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin k_c ---------------}
--------------- --------------- --------------- ---------------

k_c :: Chunk
k_c = newChunk $
  [(Symbol,S "k":-: S "c"),
   (VarName, S "k_c"),
   (Description, S "clad conductivity")
  ]
