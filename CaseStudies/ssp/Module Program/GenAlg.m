function [cslip, F, slips, gens, Fgens, nof, rt, switch7, switch13 ] = ...
            GenAlg (funcF, params_layers,...
    params_piez, params_search, params_soln, params_load)
% Slope Stability Analysis Program
% Output.m
%
% 20 August 2015
% 
%  - For a description of the theories and equations the module is built on
%  refer to the SRS. Specifially IM6.
%  (../Documentation Files/SRS_SSP.pdf)
%
%  - For a description of the module interface refer to the MIS.
% (../Documentation Files/MIS_SSP.pdf)
%
%  - For a description of the module secrets and services refer to the MG.
% (../Documentation Files/MG_SSP.pdf)
%
% ---------------------
%
% The genetic algorithm implemented in this function is based on the
% technique proposed in:
% 
%   Li,Y.C., Chen,Y.M., Zhan,T.L.T., Ling,D.S., and Cleall,P.J. (2010). An
%       efficient approach for locating the critical slip surface in slope
%       stability analyses using a real-coded genetic algorithm. Canadian
%       Geotechnical Journal 47(7), 806-820.
% 
% As suggested in the above reference, the random initialization of the
% population is based on the entrance and exit angle scheme described in:
% 
%   Boutrup,E. and Lovell,C.W. (1980). Searching techniques in slope
%       stability analysis. Engineering Geology 16(1-2), 51-61.
% 
% The termination of the search process is based on a dynamic criterion
% whereby the search is stopped if the relative difference in minimum
% factor of safety is less than a specified value, eps_term, for a certain
% number of generations, Mni_term, after a minimum number of generations,
% Mterm, have been evaluated as proposed in:
% 
%   Cheng,Y.M., Li,L., Chi,S.C., and Wei,W.B. (2007). Particle swarm
%       optimization algorithm for the location of the critical
%       non-circular failure surface in two-dimensional slope stability
%       analysis. Computers and Geotechnics 34(2), 92-103.
% 
% The search procedure involves beginning with piecewise linear surfaces
% specified by 4 vertices, then proceeding to 7 vertices, and finally 13
% vertices.
%
% ---------------------
% Notes:
% ---------------------
% 
% 1. The stratigraphy parameter, strat, which contains the geometry of the
%       soil layers must span the problem domain for all layers. Partial
%       layers must be "pinched out" to zero thickness where they are not
%       present. Furthermore, strat{1} must define the upper surface of the
%       slope geometry with strat{>1} proceeding monotonically downward in
%       the soil profile. This simplifies certain operations that involve
%       determining intersections with bedding planes.
% 2. It is assumed that phily is in radians.
%
% -------------------------------------------------------------------------

nlayer = length(params_layers); % strat extraction
strat = cell(nlayer,1);
for i = 1:nlayer
    strat{i} = params_layers(i).strat;
end

phily = params_layers.phi; % soil property extraction

piez = params_piez.piez; % piez extraction

Xetr = params_search.Xetr; % search extraction
Xext = params_search.Xext;
Ylim = params_search.Ylim;

ltor = params_soln.ltor; % soln extraction
evnslc = params_soln.evnslc;

% initialize the run time clock
rt = cputime;
                            
% initialize the random number generator
if exist('rng') %#ok<EXIST>
    rng('shuffle');
end

% ensure that Xetr and Xext are in the correct order
if diff(Xetr) < 0
    tmp = Xetr(1);
    Xetr(1) = Xetr(2);
    Xetr(2) = tmp;
end
if diff(Xext) < 0
    tmp = Xext(1);
    Xext(1) = Xext(2);
    Xext(2) = tmp;
end


% APPLY MIRROR OPERATOR TO GEOMETRY IF SLOPE MOVEMENT IS LEFT-TO-RIGHT
%{
This reduces the number of conditional situations that may occur since it
can always be assumed that entry is near the toe and exit is near the
crest. This will be unapplied at the end of the search by mirroring the
critical surface, cslip.
%}
xmax = max(strat{1}(1,:));
xmin = min(strat{1}(1,:));
ymax = max(Ylim);
ymin = min(Ylim);
if ltor
    for i = 1:length(strat)
        strat{i}(1,:) = xmax - strat{i}(1,:) + xmin;
        strat{i} = fliplr(strat{i});
        params_layers(i).strat = strat{i};
    end
    if size(piez,1)
        piez(1,:) = xmax - piez(1,:) + xmin;
        piez = fliplr(piez);
        params_piez(1).piez = piez;
    end
    Xetr = fliplr(xmax - Xetr + xmin);
    Xext = fliplr(xmax - Xext + xmin);
end
params_soln.ltor=0;

% KEY PARAMETERS FOR THE GENETIC ALGORITHM
%{
Mslip       number of slip surfaces in the population, an even number
            [10~50]
Mpool       size of the tournament pool [2*Mslip]
rcrs        probability of crossover [0.5~0.9]
eta         golden ratio for crossover interpolation, [(sqrt(5)-1)/2]
rmut        probability of mutation [0.05~0.2]
Mterm       minimum number of generations before tracking stopping
            criterion [50~200]
igen        counter for number of generations
gens        array that tracks the generations
Fgens       array that tracks the minimum factor of safety by generation
Mni_term    number of generations that the relative difference in minimum
            factor of safety must remain below the terminal value
            [Mterm/(10~20)]
eps_term	terminal value for relative difference in minimum factor of
            safety for stopping the search [0.00002~0.0002]
iterm       counter for stopping criterion
Madd        number of generations after which to add key points to the
            surfaces even if the solution continues to improve [Mterm/3,
            2Mterm/3]
Mni_add     number of generations that the relative difference in minimum
            factor of safety must remain below the prescribed value to add
            key points even if Madd has not been reached [Mterm/(6~20)]
eps_add     terminal value for relative difference in minimum factor of
            safety for adding key points [0.00002~0.0002]
iadd        counter for adding key points
nvtx        number of key vertices that define a slip surface; initially
            this is 4, then 7, then 13
nslice      number of slices to divide surfaces into for evaluation; since
            surfaces will be defined by 4, 7, and 13 key points, the number
            of slices should be a multiple of 12 since it divides evenly
            into 3, 6, and 12 [12,24,36,...]
nsubslice   number of slices in each subregion between control vertices
npsrf       number of points in upper stratigraphic layer
dXetr       width of range for abscissa of entry point
dXext       width of range for abscissa of exit point
slips       container for slip surfaces, factors of safety, and weighting
            scheme, dimension(Mslip,3)
pool        container for parent and child slip surfaces,
            dimension(Mpool,3)
nslip       counter for number of surfaces added to population
nof         counter for number of objective function, funcF, evaluations
%}
Mslip = 20;
Mpool = 2*Mslip;
rcrs = 0.8;
eta = (sqrt(5)-1)/2;
rmut = 0.15;
Mterm = 100;
igen = 0;
gens = igen;
Fgens = [];
Mni_term = round(Mterm/10);
eps_term = 0.00005;
iterm = 0;
Madd = [round(Mterm/3) round(2*Mterm/3)];
Mni_add = round(Mterm/6);
eps_add = 0.00005;
iadd = 0;
nvtx = 4;
nslice = 36;
npsrf = size(strat{1},2);
slips = cell(Mslip,3);
pool = cell(Mpool,3);
nslip = 0;
nof = 0;

% ensure entry and exit ranges are in the correct order
if mean(Xetr) > mean(Xext)
    Xtmp = Xetr;
    Xetr = Xext;
    Xext = Xtmp;
end
dXetr = diff(Xetr);
dXext = diff(Xext);


% INITIALIZATION (after Boutrup and Lovell, 1980)
%{
The initialization of a random surface is as follows:

1. Select an entry and exit point on the surface of the slope. The
    entry point is always taken to be to the left of the exit point
    (this is checked by ensuring that mean(Xetr) < mean(Xext)).
    Furthermore, since the mirror operator is applied for slopes moving
    left-to-right, the entry point is always on the toe-side of the
    slope and the exit point is always on the crest-side.
2. Compute the y-coords of the entry and exit points (they must reside
    on the upper surface of the slope, as specified by strat{1}) and
    the angle of the line segments on which they lie with respect to
    the horizontal, beta.
3. Select an entry angle in a specified range from a minimum in the CCW
    direction to a maximum in the CW direction with a bias toward the
    CW direction.
4. Select an exit angle in a specified range from a minimum in the CCW
    direction to a maximum in the CCW direction with a bias toward the
    maximum.
5. Compute the intersection of the lines defined by entry and exit
    points and angles.
6. Select middle points along these line segments with a bias in the
    direction of the intersection point.
7. Check admissibility criteria for the surface using the KinAdm.m module.
8. If the surface passes the checks in step 7, add it to the
    population.
9. Repeat steps 1-8 until the size of the population reaches Mslip.
10. Divide each member of the initial population using the Slicer.m
    module.
11. Sort the population from lowest to highest factor of safety and
    compute the weighting factor for each using the SlipWeighter.m module.
%}
while nslip < Mslip     % (9) iterate until population is filled
    
    newslip = zeros(2,nvtx);
    
    % (1) Select x-coords of entry and exit points
    newslip(1,1) = Xetr(1) + rand*dXetr;
    newslip(1,nvtx) = Xext(1) + rand*dXext;
    
    % (2) Compute y-coords of entry and exit points
    foundEtr = 0;	foundExt = 0;
    for i = 1:npsrf-1
        
        xly1 = strat{1}(1,i); yly1 = strat{1}(2,i);
        xly2 = strat{1}(1,i+1); yly2 = strat{1}(2,i+1);
        
        if newslip(1,1) > xly1 && newslip(1,1) <= xly2
            foundEtr = 1;
            mEtr = (yly2-yly1)/(xly2-xly1);
            betaEtr = atan(mEtr);
            newslip(2,1) = yly1 + (newslip(1,1)-xly1)*mEtr;
        end
        
        if newslip(1,nvtx) > xly1 && newslip(1,nvtx) <= xly2
            foundExt = 1;
            mExt = (yly2-yly1)/(xly2-xly1);
            betaExt = atan(mExt);
            newslip(2,nvtx) = yly1 + (newslip(1,nvtx)-xly1)*mExt;
        end
        
        if foundEtr && foundExt,	break;	end
    end
    
    % (3) Select entry angle
    alphaEtrmax = betaEtr - 0.0872;	% beta1 - 5deg
    alphaEtrmin = -0.7854;          % -45deg
    alphaEtr = alphaEtrmin + (alphaEtrmax-alphaEtrmin)*rand^2;
    mEtr = tan(alphaEtr);
    bEtr = newslip(2,1) - mEtr*newslip(1,1);
    
    % (4) Select exit angle
    alphaExtmin = betaExt + 0.0872;  % beta4 + 5deg
    alphaExtmax = max(alphaExtmin, max(1.047, 0.7854+phily(1)/2));  % max(alphaExtmin, max(60, 45+phi/2))
    alphaExt = alphaExtmax - (alphaExtmax-alphaExtmin)*rand^2;
    mExt = tan(alphaExt);
    bExt = newslip(2,nvtx) - mExt*newslip(1,nvtx);
    
    % (5) Compute intersection of entry and exit lines
    xint = (bExt-bEtr)/(mEtr-mExt);
    yint = mEtr*xint + bEtr;
    
    % (6) Select points along entry and exit lines for interior points
    wt = rand^2;
    newslip(1,2) = wt*xint + (1-wt)*newslip(1,1);
    newslip(2,2) = wt*yint + (1-wt)*newslip(2,1);
    
    wt = rand^2;
    newslip(1,3) = wt*xint + (1-wt)*newslip(1,nvtx);
    newslip(2,3) = wt*yint + (1-wt)*newslip(2,nvtx);
    
    % (7) Evaluate admissibility criteria
    [pass, newslip] =...
   KinAdm(newslip, strat{1}, params_soln);
    
    % (8) If the surface is admissible, add it to the initial population
    if pass
        nslip = nslip+1;
        slips{nslip,1} = newslip;
    end
end


% (10) Evaluate factor of safety for each initial surface
for i = 1:Mslip
    
    evalslip = Slicer(evnslc, slips{i,1}, nslice);
    
    % evaluate factor of safety
    slips{i,2} = funcF(evalslip, params_layers, params_piez,...
        params_soln, params_load);
    nof = nof+1;
    
end

% (11) Sort the list of surfaces by factor of safety and apply the
% weighting scheme

slips = SlipWeighter(Mslip, slips);

F = slips{1,2}; % initialize minimum factor of safety
Fgens = [Fgens F];

%GENETIC ALGORITHM (after Li et. al., 2010)
%{
The search process consists of 5 main parts: crossover, mutation,
admissibility check, evaluation, and tournament selection. The details
are as follows:

1. Check if key points need to be added to the slip surfaces. The procedure
    begins with crude surfaces defined by 4 key vertices and proceeds to 7
    and 13 vertex surfaces. The addition of vertices is controlled both by
    improvement of the minimum factor of safety and an iteration counter.
2. Crossover
    (i) Select random pairs of parents based on the weighting scheme
            associated with the factor of safety sort and place them into
            the mating pool. Also, place copies into the bottom of the
            mating pool to initialize the children.
    (ii) For each pair of children, scroll through the list of
            parameters (the x,y coords of the vertices) and perform
            linear interpolation based on the golden ratio,
            eta=(sqrt(5)-1)/2, if a randomly generated number is less
            than rcrs.
3. Mutation
    Scroll through the list of parameters in the pool of children and
	randomly regenerate parameters within the dynamic bounds as suggested
	by Li et al (2010) if a random number is less than rmut.
4. Admissibility Check
	Ensure each slip surface in the pool of children passes the
	kinematic admissibility criterion using the KinAdm.m module. If any of 
    these fail, assign a factor of safety of 1000 to the surface so that it 
    will not be selected for the next generation.
5. Evaluation
	Compute the factor of safety for each of the slip surfaces in the
	pool of children (unless it has already been set in step 4), using the 
    MorgPriceSolver.m module.
6. Tournament Selection
	(i) Sort the tournament pool, which contains parents and children from
            the crossover and mutation stages, based on factor of safety
            and assign weights to each slip surface, using the 
            SlipWeighter.m module.
    (ii) Immediately place the minimum factor of safety from the tournament
            pool into the population for the next generation to prevent
            loss of this information.
	(iii) Randomly select 3 surfaces at a time and place the one with
            the lowest factor of safety into the population for the
            next generation. Assign a weight of 0 to the surface that
            was selected so that it is only selected once.
	(iv) Once the new population is filled, sort it and assign new
            weights, again using the SlipWeighter.m module for the next 
            round of crossover.
	(v) Compute relative difference in minimum factor of safety and
            increment counters as necessary.
7. Repeat steps 1-6 until the stopping criterion is satisfied.
%}
while 1
    
    % save previous minimum factor of safety to check relative difference
    Fold = F;
    
    % (1) Check if vertices need to be added
    % first criterion based on improvement of F
    if iadd > Mni_add
        if size(slips{1,1},2) < 7   % increase from 4 to 7 vertices
            iadd = 0;
            nvtx = 7;
            switch7 = igen;
            for i = 1:Mslip
                newslip = zeros(2,nvtx);
                newslip(:,1:2:nvtx) = slips{i,1};
                for j = 2:2:nvtx
                    newslip(:,j) = 0.5*(newslip(:,j-1)+newslip(:,j+1));
                end
                slips{i,1} = newslip;
            end
        elseif size(slips{1,1},2) < 13  % increase from 7 to 13 vertices
            iadd = 0;
            nvtx = 13;
            switch13 = igen;
            for i = 1:Mslip
                newslip = zeros(2,nvtx);
                newslip(:,1:2:nvtx) = slips{i,1};
                for j = 2:2:nvtx
                    newslip(:,j) = 0.5*(newslip(:,j-1)+newslip(:,j+1));
                end
                slips{i,1} = newslip;
            end
        end
    % second criterion based on passage of generations
    elseif igen == Madd(1) && size(slips{1,1},2) < 7
        iadd = 0;
        nvtx = 7;
        switch7 = igen;
        for i = 1:Mslip
            newslip = zeros(2,nvtx);
            newslip(:,1:2:nvtx) = slips{i,1};
            for j = 2:2:nvtx
                newslip(:,j) = 0.5*(newslip(:,j-1)+newslip(:,j+1));
            end
            slips{i,1} = newslip;
        end
    elseif igen == Madd(2) && size(slips{1,1},2) < 13
        iadd = 0;
        nvtx = 13;
        switch13 = igen;
        for i = 1:Mslip
            newslip = zeros(2,nvtx);
            newslip(:,1:2:nvtx) = slips{i,1};
            for j = 2:2:nvtx
                newslip(:,j) = 0.5*(newslip(:,j-1)+newslip(:,j+1));
            end
            slips{i,1} = newslip;
        end
    end
    
    
    % (2,i) Select random pairs of parents and place them into the mating
    % pool; also initialize children at bottom of pool
    prtsel = rand(Mslip,1);
    for i = 1:Mslip
        found = 0;
        for j = 1:Mslip
            if prtsel(i) <= slips{j,3}
                found = 1;
                pool{i,1} = slips{j,1};
                pool{i,2} = slips{j,2};
                pool{i+Mslip,1} = slips{j,1};
                break;
            end
        end
        if ~found,  break;  end
    end
    
    % Note: If all slip surfaces have the same factor of safety, the
    % weights will all evaluate to NaN. Consequently, the weighted random
    % selection scheme will fail. In this case, select surfaces at random.
    if ~found
        prtsel = randi(Mslip,Mslip,1);
        for i = 1:Mslip
            pool{i,1} = slips{prtsel(i),1};
            pool{i,2} = slips{prtsel(i),2};
            pool{i+Mslip,1} = slips{prtsel(i),1};
        end
    end
    
    
    % (2,ii) Apply crossover to pairs of children
    for i = Mslip+1:2:Mpool
        ii = i+1;
        for j = 1:nvtx
            if rand < rcrs
                akp = pool{i,1}(:,j);
                akq = pool{ii,1}(:,j);
                pool{i,1}(:,j) = eta*akp + (1-eta)*akq;
                pool{ii,1}(:,j) = (1-eta)*akp + eta*akq;
            end
        end
    end
    
    
    % (3) Apply mutation to parameters of children
    for i = Mslip+1:Mpool
        
        % entry vertex
        if rand < rmut
            
            % upper limit for x1
            xlimu = 0.5*(pool{i,1}(1,1) + pool{i,1}(1,2));
            
            % coords and slope of line V2,V3
            xsl1 = pool{i,1}(1,2);  ysl1 = pool{i,1}(2,2);
            xsl2 = pool{i,1}(1,3);  ysl2 = pool{i,1}(2,3);
            msl = (ysl2-ysl1)/(xsl2-xsl1);
            bsl = ysl1 - msl*xsl1;
            
            % lower limit for x1
            for j = 1:npsrf-1

                xly1 = strat{1}(1,j);   yly1 = strat{1}(2,j);
                xly2 = strat{1}(1,j+1); yly2 = strat{1}(2,j+1);
                mly = (yly2-yly1)/(xly2-xly1);
                bly = yly1 - mly*xly1;

                xliml = (bly-bsl)/(msl-mly);

                if xliml > xly1 && xliml <= xly2
                    break;
                elseif xliml < xmin
                    xliml = xmin;
                    break;
                end

            end
            
            % generate new value for x1
            wt = rand;
            pool{i,1}(1,1) = wt*xliml + (1-wt)*xlimu;
            
            % compute y1
            for j = 1:npsrf-1
                xly1 = strat{1}(1,j);   yly1 = strat{1}(2,j);
                xly2 = strat{1}(1,j+1); yly2 = strat{1}(2,j+1);
                
                if pool{i,1}(1,1) > xly1 && pool{i,1}(1,1) <= xly2
                    pool{i,1}(2,1) = yly1 + ...
                                (pool{i,1}(1,1)-xly1)*(yly2-yly1)/(xly2-xly1);
                    break;
                end
            end
            
        end
        
        
        % exit vertex
        if rand < rmut
            
            % upper limit for x1
            xliml = 0.5*(pool{i,1}(1,nvtx-1) + pool{i,1}(1,nvtx));
            
            % coords and slope of line Vnvtx-2,Vnvtx-1
            xsl1 = pool{i,1}(1,nvtx-2);  ysl1 = pool{i,1}(2,nvtx-2);
            xsl2 = pool{i,1}(1,nvtx-1);  ysl2 = pool{i,1}(2,nvtx-1);
            msl = (ysl2-ysl1)/(xsl2-xsl1);
            bsl = ysl1 - msl*xsl1;
            
            % lower limit for x1
            for j = npsrf:-1:2

                xly1 = strat{1}(1,j-1);	yly1 = strat{1}(2,j-1);
                xly2 = strat{1}(1,j);   yly2 = strat{1}(2,j);
                mly = (yly2-yly1)/(xly2-xly1);
                bly = yly1 - mly*xly1;

                xlimu = (bly-bsl)/(msl-mly);

                if xlimu > xly1 && xlimu <= xly2
                    break;
                elseif xlimu > xmax
                    xlimu = xmax;
                    break;
                end

            end
            
            % generate new value for xnvtx
            wt = rand;
            pool{i,1}(1,nvtx) = wt*xliml + (1-wt)*xlimu;
            
            % compute y1
            for j = npsrf:-1:2
                xly1 = strat{1}(1,j-1);	yly1 = strat{1}(2,j-1);
                xly2 = strat{1}(1,j);   yly2 = strat{1}(2,j);
                
                if pool{i,1}(1,nvtx) > xly1 && pool{i,1}(1,nvtx) <= xly2
                    pool{i,1}(2,nvtx) = yly1 + ...
                                (pool{i,1}(1,nvtx)-xly1)*(yly2-yly1)/(xly2-xly1);
                    break;
                end
            end
            
        end
        
        
        % non-end vertices
        for j = 2:nvtx-1
            
            if rand < rmut
                
                xprv = pool{i,1}(1,j-1);    yprv = pool{i,1}(2,j-1);
                xcur = pool{i,1}(1,j);
                xnxt = pool{i,1}(1,j+1);    ynxt = pool{i,1}(2,j+1);
                
                % lower and upper limits for x-coord
                xliml = 0.5*(xprv+xcur);
                xlimu = 0.5*(xcur+xnxt);
                
                % upper limit for y-coord
                ylimu = min(yprv +  (xcur-xprv)*(ynxt-yprv)/(xnxt-xprv),...
                                ymax);
                
                % lower limit for y-coord
                if j == 2
                    
                    xahd = pool{i,1}(1,j+2);    yahd = pool{i,1}(2,j+2);
                    
                    yliml = max(ynxt + (xcur-xnxt)*(yahd-ynxt)/(xahd-xnxt),...
                                    ymin);
                    
                elseif j == nvtx-1
                    
                    xbhd = pool{i,1}(1,j-2);    ybhd = pool{i,1}(2,j-2);
                    
                    yliml = max(yprv + (xcur-xprv)*(yprv-ybhd)/(xprv-xbhd),...
                                    ymin);
                    
                else
                    
                    xbhd = pool{i,1}(1,j-2);    ybhd = pool{i,1}(2,j-2);
                    xahd = pool{i,1}(1,j+2);    yahd = pool{i,1}(2,j+2);
                    
                    yliml = max([(yprv + (xcur-xprv)*(yprv-ybhd)/(xprv-xbhd))
                            (ynxt + (xcur-xnxt)*(yahd-ynxt)/(xahd-xnxt))
                            ymin]);
                        
                end
                
                % generate new x-coord
                wt = rand;
                pool{i,1}(1,j) = wt*xliml + (1-wt)*xlimu;
                
                % generate new y-coord
                wt = rand;
                pool{i,1}(2,j) = wt*yliml + (1-wt)*ylimu;
                
            end
            
        end
        
    end
    
    
    % (4) Apply kinematic admissibility constraints to children
    for i = Mslip+1:Mpool
        
        [pass, pool{i,1}]=...
   KinAdm(pool{i,1}, strat{1}, params_soln);
        
        % if kinematic admissibility failed, assign high factor of safety
        % so that this surface will not be selected in the tournament
        if ~pass
            pool{i,2} = 1000;
            
        % (5) If the surface is kinematically admissible, evaluate the
        % factor of safety
        else
            
            evalslip = Slicer(evnslc, pool{i,1}, []);    

            pool{i,2} = funcF(evalslip, params_layers, params_piez,...
                params_soln, params_load);
            nof = nof+1;
            
        end
        
    end
    
    
    % (6,i) Sort the tournament pool by factor of safety and apply the
    % weighting scheme
    
    pool = SlipWeighter(Mpool, pool);
    
    % (6,ii) Place the surface with the minimum factor of safety so far in
    % the population for the next generation.
    slips{1,1} = pool{1,1};
    slips{1,2} = pool{1,2};
    pool{1,3} = 0;
    
    % (6,iii) Randomly select 3 surfaces from the tournament pool at a time
    % and place the one with the lowest factor of safety into the next
    % generation.
    for i = 2:Mslip
        
        trnsel = rand(3,1);
        isel = Mpool+1;
        found = 0;
        for j = 1:3
            for k = 1:Mpool
                if trnsel(j) <= pool{k,3} && k < isel
                    found = 1;
                    isel = k;
                    break;
                end
            end
        end
        if ~found,  break;  end
        
        slips{i,1} = pool{isel,1};  % place into population
        slips{i,2} = pool{isel,2};
        pool{isel,3} = 0;           % prevent same surface being selected twice
        if isel == Mpool,   pool{Mpool-1,3} = 1;    end
        
    end
    
    % Note: If all slip surfaces have the same factor of safety, the
    % weights will all evaluate to NaN. Consequently, the weighted random
    % selection scheme will fail. In this case, simply take surfaces
    % 2:Mslip into the next generation.
    if ~found
        for i = 2:Mslip
            slips{i,1} = pool{i,1};
            slips{i,2} = pool{i,2};
        end
    end
    
    
    % (6,iv) Sort the new population and assign weights for the next round
    % of crossover
    
    slips = SlipWeighter(Mslip, slips);
    
    % (6,v) Compute improvement of F and increment counters
    F = slips{1,2};
    
    dFmin = abs(F - Fold);
    
    if dFmin < eps_term,	iterm = iterm+1;
    else    iterm = 0;
    end
    
    if dFmin < eps_add,     iadd = iadd+1;
    else    iadd = 0;
    end
    
    igen = igen+1;
    gens = [gens igen]; %#ok<AGROW>
    Fgens = [Fgens F];  %#ok<AGROW>
    
    % (7) Check stopping criterion
    if igen > Mterm && iterm > Mni_term,    break;  end
end


% APPLY MIRROR OPERATOR TO slips IF SLOPE MOVEMENT IS LEFT-TO-RIGHT
%{
% See note in mirror operator at beginning.
%}
if ltor
    for i = 1:size(slips,1)
        slips{i,1}(1,:) = xmax - slips{i,1}(1,:) + xmin;
        slips{i,1} = fliplr(slips{i,1});
    end
end

% ASSIGN CRITICAL SURFACE AND FACTOR OF SAFETY TO RETURN PARAMETERS
cslip = slips{1,1};
F = slips{1,2};

% compute run time
rt = cputime-rt;