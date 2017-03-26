%% Input Format Module
% This module takes a filename and then uses it to load
% the parameter values into the parameters object (params)
%
% Authors: Thulasi Jegatheesan, Spencer Smith, Ned Nedialkov, and Brooks
% MacLachlan
%
% Date Last Revised: May 17, 2016
%
% Uses: params (Input Parameters Module)
%
% State Variables: none
%
% Environment Variable: the file associated with filename
%
% Assumptions: Once initialized the parameter values are constant
% for the life of the program.
%
% Considerations: The meaning of each parameter is given as a comment in the code below
%
function [ params ] = load_params(filename)
    
    fid = fopen(filename);
    [param] = textscan(fid, '%f', 'Delimiter', '\n', 'CommentStyle', '#');
    param = param{:};
    fclose(fid);

    %parameters from input
    params.L       = param(1); % Length of tank
    params.diam    = param(2); % Diameter of tank
    params.Vp      = param(3); % Volume of PCM
    params.Ap      = param(4); % Surface area of PCM
    params.rho_p   = param(5); % Density of PCM
    params.Tmelt   = param(6); % Melt temperature of PCM
    params.C_ps    = param(7); % Specific heat capacity of solid PCM
    params.C_pl    = param(8); % Specific heat capacity of liquid PCM
    params.Hf      = param(9); % Heat of fusion for PCM
    params.Ac      = param(10); % Area of coil
    params.Tc      = param(11); % Temperature of coil
    params.rho_w   = param(12); % Density of water
    params.C_w     = param(13); % Specific heat capacity of water
    params.hc      = param(14); % Heat transfer coefficient between water and coil
    params.hp      = param(15); % Heat transfer coeeficient between PCM and water
    params.Tinit   = param(16); % Initial temperature of water and PCM
    params.tstep   = param(17); % Time step for simulation
    params.tfinal  = param(18); % Time at which to stop simulation
    params.AbsTol  = param(19); % Absolute tolerance
    params.RelTol  = param(20); % Relative tolerance

    %calculated parameters
    params.Vt      = pi*(params.diam/2)*(params.diam/2)*params.L;       % Total volume of tank, including pcm and water
    params.Mw      = params.rho_w * (params.Vt - params.Vp);            % Mass of water
    params.tau_w   = (params.Mw * params.C_w)/(params.hc * params.Ac);  % ODE parameter for water
    params.eta     = (params.hp * params.Ap)/(params.hc * params.Ac);   % ODE parameter
    params.Mp      = params.rho_p * params.Vp;                          % Mass of pcm
    params.tau_ps  = (params.Mp * params.C_ps)/(params.hp * params.Ap); % ODE parameter for solid pcm
    params.tau_pl  = (params.Mp * params.C_pl)/(params.hp * params.Ap); % ODE parameter for liquid pcm
    
    params.Epmelt_init   = params.C_ps * params.Mp * (params.Tmelt - params.Tinit);
    params.Ep_melt3 = params.Hf * params.Mp;

    params.Mw_noPCM      = params.rho_w * (params.Vt);
    params.tau_w_noPCM   = (params.Mw_noPCM * params.C_w)/(params.hc * params.Ac);

end
