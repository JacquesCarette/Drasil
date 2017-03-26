%% Energy Module, when Tp > Tmelt
% This module uses the input parameters in params and the array T3 which is calculated 
% by temp3.m to specify the equations that govern the change in energy of the water and PCM
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
% $$E_P(t) = E^{init}_{Pmelt} + H_f m_P + C^L_P m_P (T_P(t) - T^P_{melt})$$
%
%
function [Ew3, Ep3] = energy3(T3, params)

    %change in energy in water when T > Tmelt
    Ew3     = params.C_w * params.Mw * (T3(:,1) - params.Tinit);
        
    
    %change in energy in Pcm when T > Tmelt        
    
    Ep3     = params.Epmelt_init + params.Ep_melt3 + (params.C_pl * params.Mp * (T3(:,2) - params.Tmelt));
