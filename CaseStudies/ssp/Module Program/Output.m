function Output(cslip, params_layers, params_piez,...
    params_soln, params_load, fname, sepind, params_search)

% Slope Stability Analysis Program
% Output.m
%
% 20 August 2015
% 
%  - For a description of the module interface refer to the MIS.
% (../Documentation Files/MIS_SSP.pdf)
%
%  - For a description of the module secrets and services refer to the MG.
% (../Documentation Files/MG_SSP.pdf)
%
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% REANALYZE CRITICAL SURFACE USING MORG-PRICE AND RFEM (FOR PLOTTING)
% -------------------------------------------------------------------------

nlayer = length(params_layers); % strat extraction
strat = cell(nlayer,1);
for i = 1:nlayer
    strat{i} = params_layers(i).strat;
end

piez = params_piez.piez; % piez extraction

ftype = params_soln.ftype; % soln method extraction
evnslc = params_soln.evnslc;

Xetr = params_search.Xetr; %search range extraction
Xext = params_search.Xext;
Ylim = params_search.Ylim;


% create surface for analysis
nslice=36;
[evalslip] = Slicer(evnslc, cslip, nslice);

% get surface coords of each slice
ysrf = zeros(1,nslice+1);
nply = size(strat{1},2);
for i = 1:nslice+1
    
    x = evalslip(1,i);
    
    for iply = 1:nply-1

        x1 = strat{1}(1,iply);    y1 = strat{1}(2,iply);
        x2 = strat{1}(1,iply+1);  y2 = strat{1}(2,iply+1);

        if x >= x1 && x < x2    % check if coords of base lie in the domain of this line seg
            ysrf(i) = y1 + (x - x1)*(y2 - y1)/(x2 - x1);    % compute y coord on surface
            break;
        end
    end
    
end

% slice coords (for plotting displacements)
xslc = zeros(1,5*nslice);
yslc = zeros(1,5*nslice);
xslc(1:5:5*nslice) = evalslip(1,1:nslice);
yslc(1:5:5*nslice) = evalslip(2,1:nslice);
xslc(2:5:5*nslice) = evalslip(1,2:nslice+1);
yslc(2:5:5*nslice) = evalslip(2,2:nslice+1);
xslc(3:5:5*nslice) = evalslip(1,2:nslice+1);
yslc(3:5:5*nslice) = ysrf(2:nslice+1);
xslc(4:5:5*nslice) = evalslip(1,1:nslice);
yslc(4:5:5*nslice) = ysrf(1:nslice);
xslc(5:5:5*nslice) = evalslip(1,1:nslice);
yslc(5:5:5*nslice) = evalslip(2,1:nslice);


% analyze critical surface with Morgenstern-Price method
[F_mp, ~, Nint_mp, Tint_mp] = ...
    MorgPriceSolver(evalslip, params_layers,params_piez,...
    params_soln, params_load);

% analyze critical surface with RFEM
[F_rfem, Floc_rfem, dx_rfem, dy_rfem, Nint_rfem, Tint_rfem] = ...
    RFEMSolver(evalslip, params_layers, params_piez,...
    params_soln, params_load);

% compute magnification factor such that max displacement is 15% of slope
% height
hslp = max(strat{1}(2,:)) - min(strat{1}(2,:));
maxd = max(sqrt(dx_rfem.^2 + dy_rfem.^2));
mag = 0.15 * hslp / maxd;

% determine slice displacements
for i = 1:nslice
    
    iii = 5*i; ii = iii-4;
    
    xslc(ii:iii) = xslc(ii:iii) + mag*dx_rfem(i);
    yslc(ii:iii) = yslc(ii:iii) + mag*dy_rfem(i);
    
end


% -------------------------------------------------------------------------
% PLOT RESULTS
% -------------------------------------------------------------------------

figure;

% plot stratigraphy and critical slip surface
hgeom = subplot(311);
nlayer = size(strat,1);
if size(piez,1),    plot(piez(1,:),piez(2,:),'--b');
else plot(strat{1}(1,1),strat{1}(2,1));
end
hold on;
for i = 1:nlayer
    h = plot(strat{i}(1,:),strat{i}(2,:),'-k');
    if i==1,	set(h,'linewidth',2);   end
end
h = plot(evalslip(1,:),evalslip(2,:),'--r');
set(h,'linewidth',2)
xlabel('x','fontweight','bold');
ylabel('y','fontweight','bold');
title(sprintf('Critical slip surface, F_{MP} = %5.4f, F_{RFEM} = %5.4f, magnification = %g', ...
                F_mp, F_rfem, mag), 'fontweight', 'bold');

% plot displaced slices (from RFEM)
for i = 1:nslice
    
    iii = 5*i; ii = iii-4;
    plot(xslc(ii:iii), yslc(ii:iii), '-r');
    
end

minx = min(strat{1}(1,:));
maxx = max(strat{1}(1,:));
miny = min([min(evalslip(2,:)) min(strat{nlayer}(2,:))]);
maxy = max(strat{1}(2,:));

set(hgeom, 'xlim', [minx maxx]);
set(hgeom, 'ylim', [miny-0.2*hslp maxy+0.2*hslp]);
hold off;

% plot local factors of safety
hloc = subplot(312);
xn = 0.5*(evalslip(1,1:nslice) + evalslip(1,2:nslice+1));
plot(xn,F_mp*ones(1,nslice), '--r');
hold on;
plot(xn,F_rfem*ones(1,nslice), '--k');
plot(xn,Floc_rfem, '-ok');
hold off;

set(hloc, 'xlim', [minx maxx]);
xlabel('x','fontweight','bold');
ylabel('F_s','fontweight','bold');
title('Factor of safety distribution','fontweight','bold');
legend('F_{MP}','F_{RFEM}','F_{loc,RFEM}');

% plot interslice forces
hfrc = subplot(313);
plot(evalslip(1,2:nslice),Nint_mp,'-k');
hold on;
plot(evalslip(1,2:nslice),Tint_mp,'-r');
plot(evalslip(1,2:nslice),Nint_rfem,'--k');
plot(evalslip(1,2:nslice),Tint_rfem,'--r');
hold off;

set(hfrc, 'xlim', [minx maxx]);
xlabel('x','fontweight','bold');
ylabel('N_{int}, T_{int}','fontweight','bold');
title('Interslice forces','fontweight','bold');
legend('N_{MP}','T_{MP}','N_{RFEM}','T_{RFEM}');



% -------------------------------------------------------------------------
% GENERATE OUTPUT FILE
% -------------------------------------------------------------------------

foutname = fname;

% strip file extension
if ~isempty(sepind),    foutname = foutname(1:sepind-1);    end

% check for existing output file and set output filename
if exist([foutname '.OUT'],'file')
    foutname = [foutname '.OUT'];
else
    foutname = [foutname '.out'];
end

fout = fopen(foutname,'a');

fprintf(fout, '\r\n\r\n');
fprintf(fout, '-----------------------------------------\r\n');
fprintf(fout, 'Slope Stability Analysis\r\n\r\n');
fprintf(fout, 'Input File:\t\t%s\r\n', fname);
fprintf(fout, 'Date:\t\t\t%s\r\n', datestr(now));
fprintf(fout, '-----------------------------------------\r\n');

fprintf(fout, '\r\n\r\n');
fprintf(fout, 'Slip Surface Search Ranges\r\n');
fprintf(fout, '\r\n');
fprintf(fout, '   Xenter      Xexit     Yrange\r\n');
fprintf(fout, '%9.4f  %9.4f  %9.4f\r\n', [Xetr; Xext; Ylim]);

fprintf(fout, '\r\n\r\n');
if ftype,   ftype = 'constant (Spencer)';
else        ftype = 'half-sine (Morgenstern-Price)';
end
fprintf(fout, 'Interslice force relation:\t%s\r\n', ftype);

fprintf(fout, '\r\n\r\n');
fprintf(fout, 'Critical Surface\r\n');
fprintf(fout, '\r\n');
fprintf(fout, '        x          y\r\n');
fprintf(fout, '%9.4f  %9.4f\r\n', evalslip);

fprintf(fout, '\r\n\r\n');
fprintf(fout, 'Global Factors of Safety\r\n');
fprintf(fout, '\r\n');
fprintf(fout, 'F_mp =\t\t%5.4f\r\n', F_mp);
fprintf(fout, 'F_rfem =\t%5.4f\r\n', F_rfem);

fprintf(fout, '\r\n\r\n');
fprintf(fout, 'Local Factors of Safety (RFEM)\r\n');
fprintf(fout, '\r\n');
fprintf(fout, '        x          F\r\n');
fprintf(fout, '%9.4f  %9.4f\r\n', [xn; Floc_rfem]);

fprintf(fout, '\r\n\r\n');
fprintf(fout, 'Slice Displacements (RFEM)\r\n');
fprintf(fout, '\r\n');
fprintf(fout, '        x               dx               dy\r\n');
fprintf(fout, '%9.4f  %15.5e  %15.5e\r\n', [xn; dx_rfem; dy_rfem]);

fprintf(fout, '\r\n\r\n');
fprintf(fout, 'Interslice Forces\r\n');
fprintf(fout, '\r\n');
fprintf(fout, '        x         N_mp         T_mp       N_rfem       T_rfem\r\n');
fprintf(fout, '%9.4f  %11.3f  %11.3f  %11.3f  %11.3f\r\n', ...
                    [   evalslip(1,2:nslice);
                        Nint_mp;
                        Tint_mp;
                        Nint_rfem;
                        Tint_rfem]     );

fprintf(fout, '\r\n\r\n');

fclose(fout);
end