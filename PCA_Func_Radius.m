function [D,EE] = PCA_Func_Radius(M,i,tmp,LSP) 
    tmp1=[];
    r=4*LSP;
     for j=1:size(M)
        
        
        S=sqrt(M(i,1)-M(j,1))^2+(M(i,2)-M(j,2))^2+(M(i,3)-M(j,3)^2);
        
      if (S<=r && S > 0)
         tmp1=[tmp1;j];
      end
     end
    
    MM=[M(tmp,1),M(tmp,2),M(tmp,3)];
    [~,~,latent] = princomp(MM);
    l=sqrt(latent);
    D(1)= (l(1)-l(2))/l(1);
    D(2)=(l(2)-l(3))/l(1);
    D(3)=l(3)/l(1);

    EE = sparse(3,1);
    if ((D(1)>D(2)) && (D(1)>D(3)))
        EE(1,1)=1;
    
    elseif ((D(2)>D(1)) && (D(2)>D(3)))
       EE(2,1)=1;
    
    elseif ((D(3)>D(1)) && (D(3)>D(2)))
       EE(3,1)=1;
       
    else
        EE(4,1)=1;

    end
end