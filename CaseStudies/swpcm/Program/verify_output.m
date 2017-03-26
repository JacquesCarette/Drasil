%% Output Verification Module

% This module verifies that the input parameters lie within  
% physical and software constraints by throwing errors and

% warnings, respectively, if any parameter does not.

%

% Authors: Thulasi Jegatheesan, Spencer Smith, Ned Nedialkov, and Brooks

% MacLachlan

%

% Date Last Revised: May 27, 2016

%

% Uses: params (Input Parameters Module)

%

% State Variables: none

%

% Environment Variables: none

%

function verify_output(params,t,T,Ew,Ep)



for i = 1:(length(t)-1)

    delta_t(i) = t(i+1) - t(i);

    i = i + 1;

end



for i = 1:length(delta_t)

    coil_E(i) = params.hc * params.Ac * delta_t(i) * (params.Tc - T(i+1,1) + params.Tc - T(i,1)) / 2;

    pcm_E(i) = params.hp * params.Ap * delta_t(i) * (T(i+1,1) - T(i+1,2) + T(i,1) - T(i,2)) / 2;

end



water_E = coil_E - pcm_E;



tot_water_E = sum(water_E);



water_error = abs(tot_water_E - Ew(end))/Ew(end)*100;



tot_pcm_E = sum(pcm_E);



pcm_error = abs(tot_pcm_E - Ep(end))/Ep(end)*100;



if(water_error > 0.001)

    warning('output:Ew', 'There is greater than 0.00001 relative error between the Ew output and the expected output based on the law of conservation of energy.')
end



if(pcm_error > 0.001)

    warning('output:Ep', 'There is greater than 0.00001 relative error between the Ep output and the expected output based on the law of conservation of energy.')

end
end