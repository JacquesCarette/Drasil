%% Energy Module, when Tp < Tmelt
% This module uses the input parameters in params and the array T1 which is calculated 
% by temp1.m to specify the equations that govern the change in energy of the water and PCM
%
% Authors: Thulasi Jegatheesan, Spencer Smith and Ned Nedialkov
%
% Data Last Revised: May 20, 2015
%
% Uses: params (Input Parameters Module)
%
% State Variables: none
%
% Environment Variables: none
%
% Governing Equations
%
% $$E_W(t) = C_W m_w (T_W(t) - T_{init})$$
%
% $$E_P(t) = C^S_P m_p (T_P(t) - T_{init})$$
%
%
function [Ew1, Ep1] = energy1(T1, params)

    %change in energy in water when T < Tmelt
    Ew1     = params.C_w * params.Mw * (T1(:,1) - params.Tinit);
        
    
    %change in energy in PCM when T < Tmelt 
    Ep1     = params.C_ps * params.Mp * (T1(:,2) - params.Tinit);