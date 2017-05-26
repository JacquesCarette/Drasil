#Commented lines pending removal
class Parameters:

    def __init__(self):
        self.L = 0.0  # Length of tank
        self.diam = 0.0  # Diameter of tank
#        self.Vp = 0.0  # Volume of PCM
#        self.Ap = 0.0  # Surface area of PCM
#        self.rho_p = 0.0  # Density of PCM
#        self.Tmelt = 0.0  # Melting temperature of PCM
#        self.C_ps = 0.0  # Specific heat capacity of solid PCM
#        self.C_pl = 0.0  # Specific heat capacity of liquid PCM
#        self.Hf = 0.0  # Heat of fusion for PCM
        self.Ac = 0.0  # Area of coil
        self.Tc = 0.0  # Temperature of coil
        self.rho_w = 0.0  # Density of water
        self.C_w = 0.0  # Specific heat capacity of water
        self.hc = 0.0  # Heat transfer coefficient between water and coil
#        self.hp = 0.0  # Heat transfer coefficient between PCM and water
        self.Tinit = 0.0  # Initial temperature of water and PCM
        self.tstep = 0.0  # Time step for simulation
        self.tfinal = 0.0  # Time at which to stop simulation
        self.AbsTol = 0.0  # Absolute tolerance
        self.RelTol = 0.0  # Relative tolerance
        self.ConsTol = 0.0  # Relative tolerance for conservation of energy
        self.Vt = 0.0  # Total volume of tank, including PCM and water
        self.Mw = 0.0  # Mass of water
        self.tau_w = 0.0  # ODE parameter for water
#        self.eta = 0.0  # ODE parameter
#        self.Mp = 0.0  # Mass of PCM
#        self.tau_ps = 0.0  # ODE parameter for solid PCM
#        self.tau_pl = 0.0  # ODE parameter for liquid PCM
#        self.Epmelt_init = 0.0  # Heat energy in PCM at the instant when melting begins
#        self.Ep_melt3 = 0.0  # Heat energy change in PCM from melting start to completion
        self.Mw_noPCM = 0.0  # Mass of water if no PCM is present
        self.tau_w_noPCM = 0.0  # ODE parameter for water if no PCM is present
