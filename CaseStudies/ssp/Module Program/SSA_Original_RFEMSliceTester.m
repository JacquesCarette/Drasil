
function [ErrorArray,FSArray,TimeArray]=...
    SSA_Original_RFEMSliceTester(funcF,slip,strat,phi,coh,gam,gams,piez,gamw,K,ltor,Ely, nuly, Expected,nCheck)

A=length(nCheck);
ErrorArray=zeros(1,A-1);
TimeArray=zeros(1,A-1);
FSArray=zeros(1,A-1);

XMin=slip(1,1);
XMax=slip(1,length(slip));
XLine=slip(1,:);
YPoints=slip(2,:);

c=1;
for j=nCheck
    nSlices=j;
    SliceDist=(XMax-XMin)/(nSlices);
    XSlices=XMin:SliceDist:XMax;
    YSlices=zeros(1,length(XSlices));

    XSlices(1)=XLine(1);
    XSlices(nSlices+1)=XLine(end);
    YSlices(1)=YPoints(1);
    YSlices(nSlices+1)=YPoints(end);
    for i=2:nSlices
        k=find(XLine<XSlices(i),1,'last');
        DeltaX=XLine(k+1)-XLine(k);
        DeltaY=YPoints(k+1)-YPoints(k);
        DeltaS=XSlices(i)-XLine(k);
        YSlices(i)=YPoints(k)+ (DeltaS*DeltaY)/(DeltaX);
    end
    Q=[];
    omega=[];
    
    slipnew=[XSlices;YSlices];
    
    rt = cputime;
    [FS] = ...
    funcF(slipnew,strat,phi,coh,gam,gams,piez,gamw,Q,omega,K,ltor,Ely, nuly);
    rtStar = cputime - rt;
    
    ErrorArray(c)=abs(FS-Expected)/Expected*100;
    TimeArray(c)=rtStar;
    FSArray(c)=FS;
    
    c=c+1;
    
end

end


