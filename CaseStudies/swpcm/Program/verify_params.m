%% Input Verification Module

% This module verifies that the input parameters comply with  

% physical and software constraints by throwing errors and

% warnings, respectively, if any parameter does not.

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

% Environment Variables: none


%
function verify_params(params)



%Check that inputs are valid

if params.L <= 0

    error('input:L', 'Tank length must be > 0');

elseif params.diam <= 0

    error('input:diam', 'Tank diameter must be > 0');

elseif params.Vp <= 0

    error('input:Vp', 'PCM volume must be > 0');

elseif params.Vp >= params.Vt

    error('input:VpVt', 'PCM volume must be < tank volume'); 

elseif params.Ap <= 0

    error('input:Ap', 'PCM area must be > 0');

elseif params.rho_p <= 0

    error('input:rho_p', 'rho_p must be > 0');

elseif params.Tmelt <= 0 || params.Tmelt >= params.Tc

    error('input:Tmelt', 'Tmelt must be > 0 and < Tc');

elseif params.Tc <= params.Tinit

    error('input:TcTinit', 'Tc must be > Tinit');

elseif params.Tc >= 100 || params.Tc <= 0

    error('input:Tc', 'Tc must be > 0 and < 100');

elseif params.C_ps <= 0

    error('input:C_ps', 'C_ps must be > 0');

elseif params.C_pl <= 0

    error('input:C_pl', 'C_pl must be > 0');

elseif params.Hf <= 0

    error('input:Hf', 'Hf must be > 0');

elseif params.Ac <= 0

    error('input:Ac', 'Ac must be > 0');

elseif params.rho_w <= 0

    error('input:rho_w','rho_w must be > 0');

elseif params.C_w <= 0
    error('input:C_w','C_w must be > 0');

elseif params.hc <= 0

    error('input:hc', 'hc must be > 0');

elseif params.hp <= 0

    error('input:hp', 'hp must be > 0');
elseif params.Tinit <= 0 || params.Tinit >= 100

    error('input:Tinit', 'Tinit must be > 0 and < 100');

elseif params.tfinal <= 0
    error('input:tfinal', 'tfinal must be > 0');

elseif params.Tinit > params.Tmelt
    error('input:TinitTmelt', 'Tinit must be < Tmelt');

end

%Software Constraints

if params.L < 0.1 || params.L > 50

    warning('inputwarn:L', 'It is recommended that 0.1 <= L <= 50');

elseif params.diam/params.L < 0.002 || params.diam/params.L > 200

    warning('inputwarn:diam', 'It is recommended that 0.002 <= D/L <= 200');

elseif params.Vp < (10^-6)*params.Vt

    warning('inputwarn:VpVt', 'It is recommended that Vp be >= 0.0001% of Vt');

elseif params.Vp > params.Ap || params.Ap > (2/0.001)*params.Vp

    warning('inputwarn:VpAp', 'It is recommended that Vp <= Ap <= (2/0.001)*Vp');

elseif params.rho_p <= 500 || params.rho_p >= 20000

    warning('inputwarn:rho_p', 'It is recommended that 500 < rho_p < 20000');

elseif params.C_ps <= 100 || params.C_ps >= 4000

    warning('inputwarn:C_ps', 'It is recommended that 100 < C_ps < 4000');

elseif params.C_pl <= 100 || params.C_pl >= 5000

     warning('inputwarn:C_pl', 'It is recommended that 100 < C_pl < 5000');

%elseif params.Hf <= ADD WHEN DECIDED

      %warning

elseif params.Ac > pi*(params.diam/2)^2

      warning('inputwarn:Ac', 'It is recommended that Ac <= pi*(D/2)^2');

elseif params.rho_w <= 950 || params.rho_w > 1000
      warning('inputwarn:rho_w', 'It is recommended that 950 < rho_w <= 1000');

elseif params.C_w <= 4170 || params.C_w >= 4210

      warning('inputwarn:C_w', 'It is recommended that 4170 < C_w < 4210');

elseif params.tfinal <= 0 || params.tfinal >= 86400

      warning('input:tfinal', 'It is recommended that 0 < tfinal < 86400');

end

end