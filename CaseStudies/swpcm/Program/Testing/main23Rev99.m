function [] = main(filename)

%clear all;
%close all;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[ params ] = load_params(filename);

if params.Tc < params.Tinit
    error('temperature:TcTinit', 'Tc must be >= Tinit');
end

%calculate temperature of water and PCM when T<Tmelt
options = odeset('Events', @(t,T)event1(t,T,params), 'AbsTol', params.AbsTol, 'RelTol', params.RelTol);
[t1,T1] = ode23(@(t,T)temperature1(t,T,params), [0 params.tfinal], [params.Tinit; params.Tinit], options);
meltstart = t1(end);
fprintf('PCM has started to melt at time %f\n', meltstart);

%Calculate temperature of water and PCM when T=Tmelt
options = odeset('Events', @(t,T)event2(t,T, params), 'AbsTol', params.AbsTol, 'RelTol', params.RelTol);
[t2,T2] = ode23(@(t,T)temperature2(t,T,params), [t1(end) params.tfinal], [T1(end,1) T1(end,2) 0.0], options);
meltfinish = t2(end);
fprintf('PCM has finished melting at time %f\n', meltfinish);

%Calculate temperature of water and PCM when T>Tmelt
options = odeset('AbsTol', params.AbsTol, 'RelTol', params.RelTol);
[t3,T3] = ode23(@(t,T)temperature3(t,T,params), [t2(end) params.tfinal], [T2(end,1) T2(end,2)], options);

%Calculate change in energy
[Ew1,Ep1] = energy1(T1,params);
[Ew2,Ep2] = energy2(T2,params);
[Ew3,Ep3] = energy3(T3,params);

Ew = [Ew1; Ew2; Ew3];
Ep = [Ep1; Ep2; Ep3];
Etot = Ew + Ep;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Plot Temperature and Energy (New and Original)
t = [t1; t2; t3];
T = [T1; [T2(:,1), T2(:,2)]; T3];
plots(t,T,Ew,Ep)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Output file to directory
[~,name] = fileparts(filename);
outfile = fullfile([name,'.out']);
output(outfile, t, T, Ew, Ep, Etot, params);
