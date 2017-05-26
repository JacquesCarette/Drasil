function [ F, cslip, slips, gens, Fgens, nof, rt ] = ...
            Original_GenAlgNonCirc(  funcF, strat, phily, cohly, gamly, gamsly, ...
                            piez, gamw, Q, omega, Kc, ltor, ...
                            Xetr, Xext, Ylim, ...
                            cncvu, obtu, evnslc, ...
                            varargin )
% GenAlgNonCirc.m
% 10 October 2011
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
% Revision Notes:
% ---------------------
%                             
% 8 December 2011
% +Added slice division algorithm corresponding to evnslc switch
% 
% 
% ---------------------
% Description:
% ---------------------
% 
% This program searches for the non-circular failure surface, cslip, having
% the lowest factor of safety, F, given the stratigraphy (strat), material
% properties (phily,cohly,gamly,gamsly,gamw), location of the water table
% (piez), and direction of soil movement (ltor). Feasible ranges for the
% abscissae of the entry and exit points must also be provided in Xetr and
% Xext, respectively. The factor of safety is evaluated based on the
% provided function, funcF, which should have the following form:
% 
%       function [F] = funcF(slip, strat, phily, cohly, gamly, gamsly,
%                               piez, gamw, Q, omega, Kc, ltor, ...)
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
% vertices. In each case, the surface is divided into 30 slices for
% evaluation.
% 
% 
% ---------------------
% Input Parameters:
% ---------------------
% 
% funcF     handle to function for evaluating the factor of safety
% strat     cell array with dimension [nlayer,1] with each cell containing
%           a matrix of the coordinates of a stratigraphic layer with
%           dimension [2,npts]; the layers must proceed from top to bottom
%           and every layer must span the domain (partial layering may be
%           captured by "pinching out" to zero thickness in regions where
%           the layer is not present)
% phily     vector containing effective friction angle of each layer;
%           [1,nlayer] or [nlayer,1]
% cohly     vector containing effective cohesion of each layer; [1,nlayer]
%           or [nlayer,1]
% gamly     vector containing dry unit weight of each layer; [1,nlayer] or
%           [nlayer,1]
% gamsly    vector containing saturated unit weight of each layer;
%           [1,nlayer] or [nlayer,1]
% piez      matrix of the coordinates of the piezometric surface (water
%           table) with dimension [2,nptspz]
% gamw      unit weight of water [1,1]
% ltor      switch for left-to-right slope movement,
%           1=left-to-right
%           0=right-to-left [1,1]
% Xetr      feasible range of x-coordinates for slip surface entry; ideally
%           these should be ordered left-to-right and to the left of the
%           exit range, but both of these conditions are checked and
%           corrected herein; [1,2]
% Xext      feasible range of x-coordinates for slip surface exit; [1,2]
% Ylim      feasible range for y-coordinates; [1,2]
% cncvu     switch for the concave up condition, 1=true, 0=false; generally
%           this should be set to true unless the stratigraphy is not
%           concave up since it prevents searching a large number of
%           kinematically unreasonable surfaces
% obtu      switch to check for sharp enclosed angles between adjacent
%           slices, 1=true, 0=false
% evnslc    switch for slice division algorithm, 1=true, 0=false;
%           true = slices created by iteratively dividing the largest
%                       subsegment in half until there are nslice slices
%           false = subsegments are divided into an equal number of
%                       slices, which is nslice/(nvtx-1)
% varargin	a container for additional parameters to the factor of safety
%           function; for example, the stress field for finite-element
%           based techniques or stiffness parameters for more sophisticated
%           limit equilibrium methods
% 
% 
% ---------------------
% Output Parameters:
% ---------------------
% 
% F         the minimum factor of safety; [1,1]
% cslip     a matrix containing the most critical 13-vertex slip surface of
%           dimension [2,13]
% slips     a cell array of the top 20 results of dimension [Mslip,2]; each
%           row contains a matrix of a 13-vertex surface with dimension
%           [2,13] in the first cell column and the factor of safety in the
%           second cell column
% gens      a vector of generation numbers; [1,ngen+1]
% Fgens     a vector containing the minimum factor of safety from each
%           generation; [1,ngen+1]
% nof       number of calls to the objective function; [1,1]
% rt        run time (in seconds); [1,1]
% 
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
% 3. The parameter varargin{:} may be used to pass additional parameters to
%       the objective function for the factor of safety, funcF.
% -------------------------------------------------------------------------


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
    end
    if size(piez,1)
        piez(1,:) = xmax - piez(1,:) + xmin;
        piez = fliplr(piez);
    end
    Xetr = fliplr(xmax - Xetr + xmin);
    Xext = fliplr(xmax - Xext + xmin);
end


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
nsubslice = nslice / (nvtx-1);
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
7. Check admissibility criteria for the surface, namely:
        (i)     x-ordinates must be monotonically increasing
        (ii)    end vertices must be on slope surface
        (iii)   non-end vertices must be within the domain (i.e. below
                the set of line segments in strat{1} and between the
                min and max abscissae of strat{1})
        (iv)    line segments of slip surface must not intersect the
                upper surface, strat{1}
        (v)     slopes of the line segments must be monotonically
                increasing (i.e. surface must be concave up)
        (vi)    enclosed angle between adjacent slice bases should not be
                too sharp (<110deg) to prevent unrealistic convergence of
                factor of safety
    Note that items (i) and (ii) may be ignored in the initialization
    process since they cannot be false due to the setup process, but
    they will be important later on in the crossover and mutation
    stages. Item (v) may also be relaxed by setting the concave up
    switch, cncvu, to 0 in cases where the stratigraphy may dominate.
8. If the surface passes the checks in step 7, add it to the
    population.
9. Repeat steps 1-8 until the size of the population reaches Mslip.
10. Divide each member of the initial population into nslice slices and
    evaluate the factor of safety.
11. Sort the population from lowest to highest factor of safety and
    compute the weighting factor for each (to be used in crossover).
%}
while nslip < Mslip     % (9) iterate until population is filled
    
    newslip = zeros(2,nvtx);
    
    % (1) Select x-coords of entry and exit points
    newslip(1,1) = Xetr(1) + rand*dXetr;
    newslip(1,nvtx) = Xext(1) + rand*dXext;
    
    % (2) Compute y-coords of entry and exit points
    foundEtr = 0;	foundExt = 0;
    for i = 1:npsrf-1
        
        xly1 = strat{1}(1,i);   yly1 = strat{1}(2,i);
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
    
    mMid = (newslip(2,3)-newslip(2,2))/(newslip(1,3)-newslip(1,2));
    
    % (7) Evaluate admissibility criteria
    pass = 1;
    
    % (7,iii) Non-end vertices must be in domain
    for j = 2:3
        
        x = newslip(1,j);   y = newslip(2,j);
        
        if x < xmin || x > xmax
            pass = 0;
            break;
        end
        
        for i = 1:npsrf-1
        
            xly1 = strat{1}(1,i);   yly1 = strat{1}(2,i);
            xly2 = strat{1}(1,i+1); yly2 = strat{1}(2,i+1);

            if x > xly1 && x <= xly2
                ys = yly1 + (x-xly1)*(yly2-yly1)/(xly2-xly1);
                break;
            end
        end
        
        if y > ys
            pass = 0;
            break;
        end
        
    end
    if ~pass,   continue;   end
    
    % (7,iv) Slip surface line segments must not intersect uppermost
    % stratigraphic layer
    for j = 1:3
        
        xsl1 = newslip(1,j);    ysl1 = newslip(2,j);
        xsl2 = newslip(1,j+1);  ysl2 = newslip(2,j+1);
        msl = (ysl2-ysl1)/(xsl2-xsl1);
        bsl = ysl1 - msl*xsl1;
        
        for i = 1:npsrf-1
            
            xly1 = strat{1}(1,i);   yly1 = strat{1}(2,i);
            xly2 = strat{1}(1,i+1); yly2 = strat{1}(2,i+1);
            mly = (yly2-yly1)/(xly2-xly1);
            bly = yly1 - mly*xly1;
            
            xint = (bly-bsl)/(msl-mly);
            
            if  xint-xsl1 > 1e-3 && ...
                xint-xsl2 < -1e-3 && ...
                xint-xly1 > 1e-3 && ...
                xint-xly2 < -1e-3
                pass = 0;
                break;
            end
            
        end
        if ~pass,	break;	end
        
    end
    if ~pass,   continue;   end
    
    % (7,v) Surface must be concave up (optional)
    if cncvu && ~(mMid >= mEtr && mExt >= mMid),	continue;	end
    
    % (7,vi) angles must be >110deg between slice bases (optional)
    if obtu

        xprv = newslip(1,1);    yprv = newslip(2,1);
        xcur = newslip(1,2);    ycur = newslip(2,2);
        xnxt = newslip(1,3);    ynxt = newslip(2,3);

        asq = (xcur-xprv)^2 + (ycur-yprv)^2;
        a = sqrt(asq);
        bsq = (xnxt-xcur)^2 + (ynxt-ycur)^2;
        b = sqrt(bsq);
        csq = (xnxt-xprv)^2 + (ynxt-yprv)^2;
        theta = acos((asq+bsq-csq)/(2*a*b));

        if theta < 1.9199	% limit is 110deg in radians
            continue;
        end
        
        xprv = xcur;            yprv = ycur;
        xcur = xnxt;            ycur = ynxt;
        xnxt = newslip(1,4);    ynxt = newslip(2,4);

        asq = (xcur-xprv)^2 + (ycur-yprv)^2;
        a = sqrt(asq);
        bsq = (xnxt-xcur)^2 + (ynxt-ycur)^2;
        b = sqrt(bsq);
        csq = (xnxt-xprv)^2 + (ynxt-yprv)^2;
        theta = acos((asq+bsq-csq)/(2*a*b));

        if theta < 1.9199	% limit is 110deg in radians
            continue;
        end

    end
    
    % (8) If the surface is admissible, add it to the initial population
    nslip = nslip+1;
    slips{nslip,1} = newslip;
end


% (10) Evaluate factor of safety for each initial surface
for i = 1:Mslip
    
    if evnslc
        
        % initialize surface for evaluation
        evalslip = zeros(2,nslice+1);
        evalslip(:,1:nvtx) = slips{i,1}(:,:);

        % initialize slice counter
        islice = nvtx-1;

        % divide segments in half until desired number of slices reached
        while islice < nslice

            % find maximum segment width
            [tmp,imax] = max(diff(evalslip(1,1:islice+1))); %#ok<ASGLU>

            % shift vertices after max width segment
            evalslip(:,imax+2:islice+2) = evalslip(:,imax+1:islice+1);

            % take average for new vertex
            evalslip(:,imax+1)=0.5*(evalslip(:,imax)+evalslip(:,imax+2));

            % increment slice counter
            islice = islice+1;

        end
        
    else
    
        % initialize surface
        evalslip = zeros(2,nslice+1);
        evalslip(:,1) = slips{i,1}(:,1);

        % divide each subsegment into an equal number of slices
        for j = 1:3

            % beginning and end coords of subsegment
            xsl1 = slips{i,1}(1,j);     ysl1 = slips{i,1}(2,j);
            xsl2 = slips{i,1}(1,j+1);   ysl2 = slips{i,1}(2,j+1);

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
    
    % evaluate factor of safety
    slips{i,2} = funcF(evalslip,strat,phily,cohly,gamly,gamsly,...
                        piez,gamw,Q,omega,Kc,0,varargin{:});
    nof = nof+1;
    
end


% (11) Sort the list of surfaces by factor of safety and apply the
% weighting scheme
wts = zeros(Mslip,1);
for i = 1:Mslip
    wts(i) = slips{i,2};
end
[wts,ix] = sort(wts);
wts = wts-wts(Mslip);
wtSum = sum(wts);
wts = wts./wtSum;
tmp = slips;
for i = 1:Mslip
    
    slips{i,1} = tmp{ix(i),1};
    slips{i,2} = tmp{ix(i),2};
    
    if i > 1, slips{i,3} = slips{i-1,3} + wts(i);
    else slips{i,3} = wts(i);
    end
end
F = slips{1,2}; % initialize minimum factor of safety
Fgens = [Fgens F];

% figure(1)
% for numlayer=1:length(strat)     % loop through layers
%     X=strat{numlayer}(1,:);   % read strat geom
%     Y=strat{numlayer}(2,:);
%     plot(X,Y,'k','LineWidth',3)
%     hold on
% end
% for numslip=1:Mslip     % loop through layers
%     X=slips{numslip,1}(1,:);   % read strat geom
%     Y=slips{numslip,1}(2,:);
%     plot(X,Y,'--r')
%     hold on
% end

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
	Ensure the following conditions for each slip surface in the pool
	of children:
        (i)     x-ordinates must be monotonically increasing
        (ii)    end vertices must be on slope surface
        (iii)   non-end vertices must be within the domain (i.e. below
                the set of line segments in strat{1} and between the
                min and max abscissae of strat{1})
        (iv)    line segments of slip surface must not intersect the
                upper surface, strat{1}
        (v)     slopes of the line segments must be monotonically
                increasing (i.e. surface must be concave up)
        (vi)    enclosed angle between adjacent slice bases should not be
                too sharp (<110deg) to prevent unrealistic convergence of
                factor of safety
	If any of these fail, assign a factor of safety of 1000 to the
	surface so that it will not be selected for the next generation.
5. Evaluation
	Compute the factor of safety for each of the slip surfaces in the
	pool of children (unless it has already been set in step 4).
6. Tournament Selection
	(i) Sort the tournament pool, which contains parents and children from
            the crossover and mutation stages, based on factor of safety
            and assign weights to each slip surface.
    (ii) Immediately place the minimum factor of safety from the tournament
            pool into the population for the next generation to prevent
            loss of this information.
	(iii) Randomly select 3 surfaces at a time and place the one with
            the lowest factor of safety into the population for the
            next generation. Assign a weight of 0 to the surface that
            was selected so that it is only selected once.
	(iv) Once the new population is filled, sort it and assign new
            weights for the next round of crossover.
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
            nsubslice = nslice / (nvtx-1);
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
            nsubslice = nslice / (nvtx-1);
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
        nsubslice = nslice / (nvtx-1);
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
        nsubslice = nslice / (nvtx-1);
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
        
        pass = 1;
        
        for j = 1:nvtx-1
            
            xcur = pool{i,1}(1,j);      ycur = pool{i,1}(2,j);
            xnxt = pool{i,1}(1,j+1);    ynxt = pool{i,1}(2,j+1);
            
            if j > 1,	mprv = mcur;    end
            mcur = (ynxt-ycur)/(xnxt-xcur);
            bcur = ycur - mcur*xcur;
            
            % (4,i) x-coords monotonically increasing
            if xnxt < xcur
                pass = 0;
                break;
            end
            
            % (4,ii) end vertices must be on slope surface
            if j == 1
                
                for k = 1:npsrf-1
                    
                    xly1 = strat{1}(1,k);   yly1 = strat{1}(2,k);
                    xly2 = strat{1}(1,k+1); yly2 = strat{1}(2,k+1);
                    
                    if xcur >= xly1 && xcur <= xly2
                        
                        yint = yly1 + (xcur-xly1)*(yly2-yly1)/(xly2-xly1);
                        ycur = yint;
                        pool{i,1}(2,1) = yint;
                        mcur = (ynxt-ycur)/(xnxt-xcur);
                        bcur = ycur - mcur*xcur;
                        break;
                        
                    end
                    
                end
                
            elseif j == nvtx-1
                
                for k = npsrf:-1:2
                    
                    xly1 = strat{1}(1,k-1); yly1 = strat{1}(2,k-1);
                    xly2 = strat{1}(1,k);   yly2 = strat{1}(2,k);
                    
                    if xnxt >= xly1 && xnxt <= xly2
                        
                        yint = yly1 + (xnxt-xly1)*(yly2-yly1)/(xly2-xly1);
                        ynxt = yint;
                        pool{i,1}(2,nvtx) = yint;
                        mcur = (ynxt-ycur)/(xnxt-xcur);
                        bcur = ycur - mcur*xcur;
                        break;
                        
                    end
                    
                end
            
            % (4,iii) non-end vertices must be in the domain
            else
                
                if xcur < xmin || xcur > xmax
                    pass = 0;
                    break;
                end

                for k = 1:npsrf-1

                    xly1 = strat{1}(1,k);   yly1 = strat{1}(2,k);
                    xly2 = strat{1}(1,k+1); yly2 = strat{1}(2,k+1);

                    if xcur >= xly1 && xcur <= xly2
                        ys = yly1 + (xcur-xly1)*(yly2-yly1)/(xly2-xly1);
                        break;
                    end
                end

                if ycur > ys
                    pass = 0;
                    break;
                end

            end
            
            % (4,iv) Slip surface line segments must not intersect
            %           uppermost stratigraphic layer
        
            for k = 1:npsrf-1

                xly1 = strat{1}(1,k);   yly1 = strat{1}(2,k);
                xly2 = strat{1}(1,k+1); yly2 = strat{1}(2,k+1);
                mly = (yly2-yly1)/(xly2-xly1);
                bly = yly1 - mly*xly1;

                xint = (bly-bcur)/(mcur-mly);

                if  xint-xcur > 1e-3 && ...
                    xint-xnxt < -1e-3 && ...
                    xint-xly1 > 1e-3 && ...
                    xint-xly2 < -1e-3
                    pass = 0;
                    break;
                end

            end
            if ~pass,	break;	end
            
            
            % (4,v) surface must be concave up (optional)
            if cncvu && j > 1 && mcur < mprv
                pass = 0;
                break;
            end
            
            
            % (4,vi) angles must be >110deg between slice bases (optional)
            if obtu && j > 1
                
                xprv = pool{i,1}(1,j-1);    yprv = pool{i,1}(2,j-1);
                
                asq = (xcur-xprv)^2 + (ycur-yprv)^2;
                a = sqrt(asq);
                bsq = (xnxt-xcur)^2 + (ynxt-ycur)^2;
                b = sqrt(bsq);
                csq = (xnxt-xprv)^2 + (ynxt-yprv)^2;
                theta = acos((asq+bsq-csq)/(2*a*b));
                
                if theta < 1.9199     % limit is 110deg in radians
                    pass = 0;
                    break;
                end
                
            end
            
            
        end
        
        % if kinematic admissibility failed, assign high factor of safety
        % so that this surface will not be selected in the tournament
        if ~pass
            pool{i,2} = 1000;
            
        % (5) If the surface is kinematically admissible, evaluate the
        % factor of safety
        else
            
            if evnslc
        
                % initialize surface for evaluation
                evalslip = zeros(2,nslice+1);
                evalslip(:,1:nvtx) = pool{i,1}(:,:);

                % initialize slice counter
                islice = nvtx-1;

                % divide segments in half until desired number of slices reached
                while islice < nslice

                    % find maximum segment width
                    [tmp,imax] = max(diff(evalslip(1,1:islice+1))); %#ok<ASGLU>

                    % shift vertices after max width segment
                    evalslip(:,imax+2:islice+2) = evalslip(:,imax+1:islice+1);

                    % take average for new vertex
                    evalslip(:,imax+1)=0.5*(evalslip(:,imax)+evalslip(:,imax+2));

                    % increment slice counter
                    islice = islice+1;

                end

            else

                evalslip = zeros(2,nslice+1);
                evalslip(:,1) = pool{i,1}(:,1);

                for j = 1:nvtx-1

                    xsl1 = pool{i,1}(1,j);      ysl1 = pool{i,1}(2,j);
                    xsl2 = pool{i,1}(1,j+1);	ysl2 = pool{i,1}(2,j+1);

                    msl = (ysl2-ysl1)/(xsl2-xsl1);
                    b = (xsl2-xsl1)/nsubslice;

                    for k = 1:nsubslice

                        ii = (j-1)*nsubslice+k; iii = ii+1;

                        evalslip(1,iii) = evalslip(1,ii)+b;
                        evalslip(2,iii) = evalslip(2,ii)+msl*b;

                    end
                end
                
            end

            pool{i,2} = funcF(evalslip,strat,phily,cohly,gamly,gamsly,...
                                piez,gamw,Q,omega,Kc,0,varargin{:});
            nof = nof+1;
            
        end
        
    end
    
    
    % (6,i) Sort the tournament pool by factor of safety and apply the
    % weighting scheme
    wts = zeros(Mpool,1);
    for i = 1:Mpool
        wts(i) = pool{i,2};
    end
    [wts,ix] = sort(wts);
    wts = wts-wts(Mpool);
    wtSum = sum(wts);
    wts = wts./wtSum;
    tmp = pool;
    for i = 1:Mpool

        pool{i,1} = tmp{ix(i),1};
        pool{i,2} = tmp{ix(i),2};

        if i > 1, pool{i,3} = pool{i-1,3} + wts(i);
        else pool{i,3} = wts(i);
        end
    end
    
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
    wts = zeros(Mslip,1);
    for i = 1:Mslip
        wts(i) = slips{i,2};
    end
    [wts,ix] = sort(wts);
    wts = wts-wts(Mslip);
    wtSum = sum(wts);
    wts = wts./wtSum;
    tmp = slips;
    for i = 1:Mslip

        slips{i,1} = tmp{ix(i),1};
        slips{i,2} = tmp{ix(i),2};

        if i > 1, slips{i,3} = slips{i-1,3} + wts(i);
        else slips{i,3} = wts(i);
        end
    end
    
    
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