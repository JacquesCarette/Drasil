%% Event Module, for Tp = Tmelt
% This module stops integration of the Temperature ODEs Module, 
% when Tp < Tmelt, once Tp = Tmelt
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
function [value,isterminal,direction] = event1(t,T,params)

    value       = params.Tmelt - T(2);
    isterminal  = 1;
    direction   = 0;
    
    