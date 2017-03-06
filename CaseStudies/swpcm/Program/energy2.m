%% Energy Module, when Tp = Tmelt
% This module uses the input parameters in params and the array T2 which is calculated 
% by temp2.m to specify the equations that govern the change in energy of the water and PCM
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
% $$E_P(t) = E^{init}_{Pmelt} + Q_P(t)$$
%
%
function [Ew2, Ep2] = energy2(T2, params)
    
    %change in energy in water when T = Tmelt
    Ew2     = params.C_w * params.Mw * (T2(:,1) - params.Tinit);
        
    
    %change in energy in Pcm when T = Tmelt        
    Qp      = T2(:,3);
    
    Ep2     = params.Epmelt_init + Qp;
