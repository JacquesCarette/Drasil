%% Input Format Module
% This module takes a filename and then uses it to load
% the parameter values into the parameters object (params)
%
% Authors: Thulasi Jegatheesan, Spencer Smith and Ned Nedialkov
%
% Date Last Revised: Feb 18, 2016
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
                               %params.Vp      = param(3); % Volume of PCM
                               %params.Ap      = param(4); % Surface area of PCM
                               %params.rho_p   = param(5); % Density of PCM
                               %params.Tmelt   = param(6); % Melt temperature of PCM
                               %params.C_ps    = param(7); % Specific heat capacity of solid PCM
                               %params.C_pl    = param(8); % Specific heat capacity of liquid PCM
                               %params.Hf      = param(9); % Heat of fusion for PCM
    params.Ac      = param(3); % Area of coil
    params.Tc      = param(4); % Temperature of coil
    params.rho_w   = param(5); % Density of water
    params.C_w     = param(6); % Specific heat capacity of water
    params.hc      = param(7); % Heat transfer coefficient between water and coil
                                %params.hp      = param(15); % Heat transfer coefficient between PCM and water
    params.Tinit   = param(8); % Initial temperature of water
    params.tstep   = param(9); % Time step for simulation
    params.tfinal  = param(10); % Time at which to stop simulation
% $$$     params.AbsTol  = param(11); % Absolute tolerance
% $$$     params.RelTol  = param(12); % Relative tolerance

    %calculated parameters
    params.Vt      = pi*(params.diam/2)*(params.diam/2)*params.L;       % Total volume of tank, including pcm and water
    params.Mw      = params.rho_w * (params.Vt);            % Mass of water
    params.tau_w   = (params.Mw * params.C_w)/(params.hc * params.Ac);

%Check that inputs are valid - this really should be in a separate function
if params.L <= 0
    error('input:L', 'Tank length must be > 0');
elseif params.diam <= 0
    error('input:diam', 'Tank diameter must be > 0');
elseif params.Tc <= params.Tinit
  error('input:TcTinit', 'Tc must be > Tinit');
elseif params.Tc >= 100 || params.Tc <= 0
    error('input:Tc', 'Tc must be > 0 and < 100');
elseif params.Ac <= 0
    error('input:Ac', 'Ac must be > 0');
elseif params.rho_w <= 0
    error('input:rho_w','rho_w must be > 0');
elseif params.C_w <= 0
    error('input:C_w','C_w must be > 0');
elseif params.hc <= 0
    error('input:hc', 'hc must be > 0');
elseif params.Tinit <= 0 || params.Tinit >= 100
    error('input:Tinit', 'Tinit must be > 0 and < 100');
elseif params.tfinal <= 0
    error('input:tfinal', 'tfinal must be > 0');
end

%Software Constraints
if params.L < 0.1 || params.L > 50
    warning('inputwarn:L', 'It is recommended that 0.1 <= L <= 50');
elseif params.diam/params.L < 0.002 || params.diam/params.L > 200
    warning('inputwarn:diam', 'It is recommended that 0.002 <= D/L <= 200');
elseif params.Ac > pi*(params.diam/2)^2
      warning('inputwarn:Ac', 'It is recommended that Ac <= pi*(D/2)^2');
elseif params.rho_w <= 950 || params.rho_w > 1000
      warning('inputwarn:rho_w', 'It is recommended that 950 < rho_w <= 1000');
elseif params.C_w <= 4170 || params.C_w >= 4210
      warning('inputwarn:C_w', 'It is recommended that 4170 < C_w < 4210');
elseif params.tfinal <= 0 || params.tfinal >= 86400
      warning('input:tfinal', 'It is recommended that 0 < tfinal < 86400');
end
