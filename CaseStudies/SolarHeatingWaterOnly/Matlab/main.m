%% Control Module
% This module coordinates the running of SWHS - Water only (no PCM)
%
% Authors: Thulasi Jegatheesan, Spencer Smith and Ned Nedialkov
%
% Date Last Revised: Feb 17, 2016
%
% Uses: params (Input Parameters Module), Output Format Modules
%
% State Variables: none
%

function [] = main(filename)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[ params ] = load_params(filename);

%calculate temperature change in water
%options = odeset('AbsTol', params.AbsTol, 'RelTol', params.RelTol);
[t, T] = ode45(@(t,T)temperature1(t,T,params), [0 params.tfinal], params.Tinit)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Plot Temperature, save to image files
plots(t, T)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Output file to same directory as input filename
[path, name] = fileparts(filename);
outfile = fullfile(path, [name,'.out']);
output(outfile, t, T, params);

plot(t, exp(t))

%True solution
%Ttrue = params.Tc + (params.Tinit - params.Tc)*exp(-1/params.tau_w * t)
%or
%Ttrue = params.Tinit + (params.Tc - params.Tinit)*(1-exp(-1/params.tau_w * t))