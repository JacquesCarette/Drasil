%% Event Module, for phi = 1
% This module stops integration of the Temperature ODEs Module, 
% when Tp = Tmelt, once melt fraction, phi, is 1
%
% Authors: Thulasi Jegatheesan, Spencer Smith and Ned Nedialkov
%
% Date Last Revised: May 27, 2015
%
% Uses: params (Input Parameters Module)
%
% State Variables: none
%
% Environment Variable: none
%
function [value,isterminal,direction] = event2(t,T,params)

    phi         = T(3)/(params.Hf * params.Mp);
    value       = phi-1;
    isterminal  = 1;
    direction   = 0;