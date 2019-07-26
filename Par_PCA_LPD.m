
clear;
parpool(12)
tic

format long
filename='test.txt';

M = dlmread(filename);
[aa1,aa2]=size(M);
aa1
aa2

linear=[];
planer=[];
volumetric=[];

n=71;
 Mdl = KDTreeSearcher(M);
 [row1,dis]=knnsearch(Mdl,M,'k',n);
 D = zeros(3,size(M,1));
row1(:,1)=[];

EE = sparse(3,size(M,1));

parfor i=1:size(M,1)

     LSP(i)= PCA_Func_LSP(M(row1(i,:),:),dis(i,n));

end


parfor i=1:size(M,1)

     [D(:,i),EE(:,i)] = PCA_Func_Radius(M,i,row1(i,:),LSP(i));

end
save Data1 D



%  lin=M(linear,:);
 lin = M(find(EE(1,:)),:);
 lin(:,end+1)=1;
 
 % plan=[planer M(planer,:)];
%  plan=M(planer,:);
 plan = M(find(EE(2,:)),:);
 plan(:,end+1)=2;
 % Vol=[volumetric M(volumetric,:)];
%  Vol=M(volumetric,:);
 Vol = M(find(EE(3,:)),:);
 Vol(:,end+1)=3;
 
 dlmwrite('linear_M.txt' ,lin,'precision','%.10f', 'delimiter', ' ', 'newline','pc');
 dlmwrite('planimetry_M.txt',plan,'precision','%.10f', 'delimiter', ' ', 'newline','pc');
 dlmwrite('Volumetric_M.txt',Vol,'precision','%.10f', 'delimiter', ' ', 'newline','pc');
time1= toc;
save sim_time time1 


