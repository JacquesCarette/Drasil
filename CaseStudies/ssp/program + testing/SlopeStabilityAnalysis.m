% SlopeStabilityAnalysis.m
% 6 January 2012
% 
% Contact:
% Brandon Karchewski
% c/o Department of Civil Engineering - JHE-301
% McMaster University
% 1280 Main St W
% Hamilton, Ontario, Canada
% L8S 4L7
% P: 905-525-9140 x24287
% F: 905-529-9688
% E: karcheba@mcmaster.ca
% 
% 
% ---------------------
% Description:
% ---------------------
% 
% This program analyzes a slope stability problem given a formatted data
% file containing a description of the stratigraphy, material properties,
% and piezometric surface for the slope. The critical slip surface is
% sought through a genetic algorithm (see GenAlgNonCirc) using the
% Morgenstern-Price method as the objective function (see MorgPrice). Once
% the critical surface is located, it is re-analyzed using the rigid finite
% element method (see RFEM) and plots are generated showing the
% displacement of slices, global and local factors of safety, and the
% distribution of interslice forces.
% 
% 
% ---------------------
% Notes:
% ---------------------
% 
% 1. The program requires the user to enter reasonable estimates for the
%       minimum and maximum abscissae for entry and exit of the slip
%       surface as well as the minimum and maximum ordinates for slip
%       surface coordinates. Consequently, it may be useful to have a
%       sketch or plot of the stratigraphy at hand when running the
%       program.
% 2. There are two options for the Morgenstern-Price portion of the
%       analysis with regard to the relationship between interslice normal
%       and shear forces. Typical Morgenstern-Price analysis uses a
%       half-sine function, however Spencer's method may be recovered as a
%       special case where a constant function is used. The desired option
%       may be selected by setting the appropriate switch when the program
%       is run.
% 3. The analysis assumes that no surface forces are applied (besides those
%       that may be due to hydrostatic pressure).
% 4. The analysis assumes that the shape of the failure surface is concave
%       with respect to the slope surface. That is, the change in base
%       angle across the slip surface is monotonic.
% 5. Displacements are magnified for plotting such that the maximum
%       displacement is 15% of the vertical distance from the toe to the
%       crest of the slope. The magnification factor is shown on the plot.
% 6. Both the input data file and the output plots are unitless. This
%       allows flexibility to work in any unit system that is desired, but
%       it is the user's responsibility to use a consistent set of base
%       units to ensure correct interpretation of the results.
%
% -------------------------------------------------------------------------


clear
clc


% -------------------------------------------------------------------------
% GET INPUT DATA (GEOMETRY, STRATIGRAPHY, MATL PROPS, ETC.) FROM USER
% -------------------------------------------------------------------------

% get name of data file
fname = ...
    input('Enter the name of the slope data file (including extension):\n', 's');

% find separator (for creating output file later)
sepind = find(fname=='.',1,'last');

nfile = length(fname);
if ~isempty(sepind) && (    strcmp(fname(sepind+1:nfile),'out') ...
                            || strcmp(fname(sepind+1:nfile),'OUT')  )
	error('Cannot use extension ''.out'' for input file.');
end

% read in slope geometry, stratigraphy, and piezometric surface
data = dlmread(fname);

i = 1;
nlayer = data(i,1);         % number of stratigraphic layers
ltor = data(i,2);           % is slope movement left-to-right?
strat = cell(nlayer,1);     % array for strat data
phi = zeros(nlayer,1);      % effective angle of friction
coh = zeros(nlayer,1);      % effective cohesion
gam = zeros(nlayer,1);      % dry unit weight
gams = zeros(nlayer,1);     % sat unit weight
E = zeros(nlayer,1);        % elastic modulus
nu = zeros(nlayer,1);       % Poisson's ratio

i = i+1;
for ilayer=1:nlayer     % loop through layers
    
    npts = data(i,1);           % number of points in strat geom
    phi(ilayer) = data(i,2);
    coh(ilayer) = data(i,3);
    gam(ilayer) = data(i,4);
    gams(ilayer) = data(i,5);
    E(ilayer) = data(i,6);
    nu(ilayer) = data(i,7);
    
    i = i+1;
    
    strat{ilayer} = data(i:i+npts-1,1:2)';   % read strat geom
    
    i = i+npts;
    
end
phi = phi*(pi/180);

nwpts = data(i,1);  % number of points in piez data (0 if dry)
if nwpts > 0
    
    i = i+1;
    gamw = data(i,1);   % unit weight of water
    
    i = i+1;
    piez = data(i:i+nwpts-1,1:2)';   % piez surface data
    
else
    
    piez = [];
    gamw = 0;
    
end

Q = [];
omega = [];
Kc = 0;



% -------------------------------------------------------------------------
% ANALYZE SLOPE USING GENETIC ALGORITHM
% -------------------------------------------------------------------------

%**************************************************************************
% NOTE: 
% Based on user feedback, it would be better if this portion were included
% in the data file as well, making it easier to run repeated analyses.
% -BK
%**************************************************************************


% set up genetic algorithm search
x1 = input('\nEnter minimum x-coord for slip surface entry: ');
x2 = input('Enter maximum x-coord for slip surface entry: ');
Xetr = [min(x1,x2),max(x1,x2)];

x1 = input('\nEnter minimum x-coord for slip surface exit: ');
x2 = input('Enter maximum x-coord for slip surface exit: ');
Xext = [min(x1,x2),max(x1,x2)];

y1 = input('\nEnter minimum y-coord for slip surface: ');
y2 = input('Enter maximum y-coord for slip surface: ');
Ylim = [min(y1,y2),max(y1,y2)];

cncvu = 1;
obtu = 1;
evnslc = 1;

ftype = input(['\nSelect interslice shear force distribution,\n'...
                '1 => f(x) = 1 (Spencer''s method)\n'...
                '0 => f(x) = half-sine (standard Morgenstern-Price): ']);
              
%**************************************************************************


% run genetic algorithm search
[F,cslip] = ...
    GenAlgNonCirc(@MorgPrice,strat,phi,coh,gam,gams,...
                    piez,gamw,Q,omega,Kc,ltor,...
                    Xetr,Xext,Ylim,...
                    cncvu,obtu,evnslc,ftype);



% -------------------------------------------------------------------------
% REANALYZE CRITICAL SURFACE USING MORG-PRICE AND RFEM (FOR PLOTTING)
% -------------------------------------------------------------------------

% create surface for analysis
nslice = 36;
nvtx = 13;
nsubslice = nslice / (nvtx-1);
if evnslc

    % initialize surface for evaluation
    evalslip = zeros(2,nslice+1);
    evalslip(:,1:nvtx) = cslip(:,:);

    % initialize slice counter
    islice = nvtx-1;

    % divide segments in half until desired number of slices reached
    while islice < nslice

        % find maximum segment width
        [tmp,imax] = max(diff(evalslip(1,1:islice+1)));

        % shift vertices after max width segment
        evalslip(:,imax+2:islice+2) = evalslip(:,imax+1:islice+1);

        % take average for new vertex
        evalslip(:,imax+1)=0.5*(evalslip(:,imax)+evalslip(:,imax+2));

        % increment slice counter
        islice = islice+1;

    end

else

    % initialize surface
    evalslip = zeros(2,nslice+1); %#ok<UNRCH>
    evalslip(:,1) = cslip(:,1);

    % divide each subsegment into an equal number of slices
    for j = 1:nvtx-1

        % beginning and end coords of subsegment
        xsl1 = cslip(1,j);     ysl1 = cslip(2,j);
        xsl2 = cslip(1,j+1);   ysl2 = cslip(2,j+1);

        % slope and y-intercept of subsegment
        msl = (ysl2-ysl1)/(xsl2-xsl1);
        b = (xsl2-xsl1)/nsubslice;

        % divide into slices
        for k = 1:nsubslice

            ii = (j-1)*nsubslice+k; iii = ii+1;

            evalslip(1,iii) = evalslip(1,ii)+b;
            evalslip(2,iii) = evalslip(2,ii)+msl*b;

        end
    end

end

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
[F_mp, lam_mp, Nint_mp, Tint_mp] = ...
    MorgPrice(evalslip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,ftype);

% analyze critical surface with RFEM
[F_rfem, Floc_rfem, dx_rfem, dy_rfem, Nint_rfem, Tint_rfem] = ...
    RFEM(evalslip,strat,phi,coh,gam,gams,piez,gamw,Q,omega,Kc,ltor,E,nu);

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