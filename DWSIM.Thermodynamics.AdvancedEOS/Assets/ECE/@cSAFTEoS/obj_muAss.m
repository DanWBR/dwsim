function res = obj_muAss(EoS,mix,Xa,ddeltaAB_droi,T,NumAss,sigma,d,ghs,dens_num)
%Auxiliary function for the calculation of associaton chemical potential
%with SAFT (calculates eq. A3 of reference)
%
%Reference: Chapman et al., Ind. Eng. Chem. Res. 29 (1990) 1709-1721

%Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
%This program is free software: you can redistribute it and/or modify
%it under the terms of the GNU General Public License as published by
%the Free Software Foundation, either version 3 of the License, or
%(at your option) any later version.
%This program is distributed in the hope that it will be useful,
%but WITHOUT ANY WARRANTY; without even the implied warranty of
%MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%GNU General Public License for more details.
%You should have received a copy of the GNU General Public License
%along with this program.  If not, see <http://www.gnu.org/licenses/>.

numC = mix.numC;

A = zeros(sum(NumAss)*numC,sum(NumAss)*numC);
indx3 = 0;
for i2 = 1:numC
    indx1 = 0;
    for i = 1:numC
        for j = 1:NumAss(i)
            indx1 = indx1 + 1;
            indx3 = indx3 + 1;
            indx2 = 0;
            sum1 = 0;
            for k = 1:numC
                for l = 1:NumAss(k)
                    indx2 = indx2 + 1;
                    if i == k %retrieves value from component matrix
                        kappa = mix.comp(i).EoSParam(5);
                        kappa = kappa(j,l);
                        epsilon = mix.comp(i).EoSParam(6);
                        epsilon = epsilon(j,l);
                        
                    else %applies mixing rules
                        kappa1 = max(max(mix.comp(i).EoSParam(5)));
                        epsilon1 = max(max(mix.comp(i).EoSParam(6)));
                        kappa2 = max(max(mix.comp(k).EoSParam(5)));
                        epsilon2 = max(max(mix.comp(k).EoSParam(6)));
                        kappa = sqrt(kappa1*kappa2)*(sqrt(sigma(i)*sigma(k))/(0.5*(sigma(i)+sigma(k))))^3;
                        epsilon = 0.5*(epsilon1 + epsilon2);
                    end
                    delta(indx1,indx2) = ((d(i)+d(k))/2)^3*ghs(i,k)*kappa*(exp(epsilon/T)-1);
                     
                    sum1 = sum1 + dens_num*mix.x(k)*(Xa(indx2)*ddeltaAB_droi(indx1,indx2,i2));
                    
                    A(indx1+(i2-1)*sum(NumAss),indx2+(i2-1)*sum(NumAss)) = A(indx1+(i2-1)*sum(NumAss),indx2+(i2-1)*sum(NumAss)) + Xa(indx1)^2*dens_num*mix.x(k)*delta(indx1,indx2);
                end
            end
            
            sum2 = 0;
            for k = 1:NumAss(i2)
                if i == i2 %retrieves value from component matrix
                    kappa = mix.comp(i).EoSParam(5);
                    kappa = kappa(j,k);
                    epsilon = mix.comp(i).EoSParam(6);
                    epsilon = epsilon(j,k);

                else %applies mixing rules
                    kappa1 = max(max(mix.comp(i).EoSParam(5)));
                    epsilon1 = max(max(mix.comp(i).EoSParam(6)));
                    kappa2 = max(max(mix.comp(i2).EoSParam(5)));
                    epsilon2 = max(max(mix.comp(i2).EoSParam(6)));
                    kappa = sqrt(kappa1*kappa2)*(sqrt(sigma(i)*sigma(i2))/(0.5*(sigma(i)+sigma(i2))))^3;
                    epsilon = 0.5*(epsilon1 + epsilon2);
                end
                
                delta = ((d(i)+d(i2))/2)^3*ghs(i,i2)*kappa*(exp(epsilon/T)-1);
                
                sum2 = sum2 + Xa(sum(NumAss(1:i2-1))+k)*delta;
            end
            
            
            A(indx3,indx3) = A(indx3,indx3) + 1;
            B(indx3) = -(Xa(indx1))^2*(sum1 + sum2);
        end
    end
end

%Solves linear system of equations
res = B/A';