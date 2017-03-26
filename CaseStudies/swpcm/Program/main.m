%% Control Module
% This module coordinates the running of SWHS
%
% Authors: Thulasi Jegatheesan, Spencer Smith, Ned Nedialkov, and Brooks
% MacLachlan
%
% Date Last Revised: May 17, 2016
%
% Uses: Input Format Module, params (Input Parameters Module), Input Verification
% Module, Output Format Modules, Temperature ODEs Modules, and Energy Equations 
% Modules, Output Verification Module 
%
% State Variables: none
%
%
function [] = main(filename)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[ params ] = load_params(filename);

verify_params(params);

%calculate temperature and change in energy of water and PCM when T<Tmelt
options = odeset('Events', @(t,T)event1(t,T,params), 'AbsTol', params.AbsTol, 'RelTol', params.RelTol);
[t1,T1] = ode45(@(t,T)temperature1(t,T,params), [0 params.tfinal], [params.Tinit; params.Tinit], options);
[Ew1,Ep1] = energy1(T1,params);
meltstart = t1(end);
if T1(end,2) < params.Tmelt 
    fprintf('PCM has not started melting\n');
else
    fprintf('PCM has started to melt at time %f\n', meltstart);
end

%Calculate temperature and change in energy of water and PCM when T=Tmelt
if (meltstart <  params.tfinal)
    options = odeset('Events', @(t,T)event2(t,T, params), 'AbsTol', params.AbsTol, 'RelTol', params.RelTol);
    [t2,T2,~,~,IE] = ode45(@(t,T)temperature2(t,T,params), [t1(end) params.tfinal], [T1(end,1) T1(end,2) 0.0], options);
    meltfinish = t2(end);
    [Ew2,Ep2] = energy2(T2,params);
    phi     =   T2(end,3)/(params.Hf * params.Mp);
    if(isempty(IE))
        meltfrac = phi*100;
        fprintf('%f%% of the PCM has melted at time %f\n', meltfrac, meltfinish);
    else
        fprintf('PCM has finished melting at time %f\n', meltfinish);
    end
else
    t2 = []; T2 = [];
    Ew2 = []; Ep2 = [];
    meltfinish = params.tfinal;
end

%Calculate temperature and change in energy of water and PCM when T>Tmelt
if (meltfinish < params.tfinal)
    options = odeset('AbsTol', params.AbsTol, 'RelTol', params.RelTol);
    [t3,T3] = ode45(@(t,T)temperature3(t,T,params), [t2(end) params.tfinal], [T2(end,1) T2(end,2)], options);
    [Ew3,Ep3] = energy3(T3,params);
else
    t3 = []; T3 = [];
    Ew3 = []; Ep3 = [];
end

Ew = [Ew1; Ew2; Ew3];
Ep = [Ep1; Ep2; Ep3];
Etot = Ew + Ep;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Plot Temperature and Energy (New and Original)
t = [t1; t2; t3];
if size(T2) == 0;
    T = T1;
else    
    T = [T1; [T2(:,1), T2(:,2)]; T3];
end
plots(t,T,Ew,Ep)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Verify that energy outputs follow the law of conservation of energy
verify_output(params, t, T, Ew, Ep);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Output file to directory
[path,name] = fileparts(filename);
outfile = fullfile(path, [name,'.out']);
output(outfile, t, T, Ew, Ep, Etot, params);
