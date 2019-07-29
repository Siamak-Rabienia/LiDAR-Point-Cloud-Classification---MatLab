function [LSP] = PCA_Func_LSP(M,dis) 

   n=71; 
        MM=[M(:,1),M(:,2),M(:,3)];
        [~,~,latent] = princomp(MM);
        l=sqrt(latent);
        D = zeros(3,1);
    D(1)= (l(1)-l(2))/l(1);
    D(2)=(l(2)-l(3))/l(1);
    D(3)=l(3)/l(1);

    if ((D(1)>D(2)) && (D(1)>D(3)))
        LPD= n/(2*dis);
        LSP=1/LPD;
    end
    
    if ((D(2)>D(1)) && (D(2)>D(3)))
       LPD = n/pi*(dis)^2;
        LSP=1/(sqrt(LPD));
    end
    
   if ((D(3)>D(1)) && (D(3)>D(2)))
       LPD= n/((4/3)*pi*(dis)^3);
        LSP=1/(nthroot(LPD,3));
   end


end