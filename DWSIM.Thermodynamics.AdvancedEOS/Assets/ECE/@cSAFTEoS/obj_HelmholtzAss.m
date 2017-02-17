function [res J EoS] = obj_HelmholtzAss(Xa,EoS,mix,T,NumAss,sigma,d,ghs,dens_num)
%Calculates the fraction for association site in the PC-SAFT EoS
%Auxiliary function, not to be used directly
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
x = mix.x;
for i = 1:numC
    kappa_v{i} = mix.comp(i).EoSParam(5);
    epsilon_v{i} =  mix.comp(i).EoSParam(6);
end

delta = zeros(sum(NumAss),sum(NumAss));
indx1 = 0;
for i = 1:numC
    for j = 1:NumAss(i)
        indx1 = indx1 + 1;
        indx2 = 0;
        for k = 1:numC
            for l = 1:NumAss(k)
                indx2 = indx2 + 1;
                if i == k %retrieves value from component matrix
                    kappa = kappa_v{i};
                    kappa = kappa(j,l);
                    epsilon = epsilon_v{i};
                    epsilon = epsilon(j,l);
                else %applies mixing rules
                    kappa1 = max(max(kappa_v{i}));
                    epsilon1 = max(max(epsilon_v{i}));
                    kappa2 = max(max(kappa_v{k}));
                    epsilon2 = max(max(epsilon_v{k}));
                    kappa = sqrt(kappa1*kappa2)*(sqrt(sigma(i)*sigma(k))/(0.5*(sigma(i)+sigma(k))))^3;
                    epsilon = 0.5*(epsilon1 + epsilon2);
                end
                delta(indx1,indx2) = ((d(i)+d(k))/2)^3*ghs(i,k)*kappa*(exp(epsilon/T)-1);
            end
        end
    end
end

%Calculates the equations
res = zeros(sum(NumAss),1);
indx1 = 0;
for i = 1:numC
    for j = 1:NumAss(i)
        indx1 = indx1 + 1;
        indx2 = 0;
        sum1 = 0;
        for k = 1:numC
            for l = 1:NumAss(k)
                indx2 = indx2 + 1;
                sum1 = sum1 + x(k)*dens_num*Xa(indx2)*delta(indx1,indx2);
            end
        end
        res(indx1) = Xa(indx1) - (1+sum1)^-1;
    end
end

%Calculates the jacobian
indx1 = 0;
J = zeros(sum(NumAss),sum(NumAss));

for i = 1:numC
    for j = 1:NumAss(i)
        indx1 = indx1 + 1;
        indx2 = 0;
        sum1 = 0;
        for k = 1:numC
            for l = 1:NumAss(k)
                indx2 = indx2 + 1;
                sum1 = sum1 + x(k)*dens_num*Xa(indx2)*delta(indx1,indx2);
            end
        end
        
        indx2 = 0;
        for k = 1:numC
            for l = 1:NumAss(k)
                indx2 = indx2 + 1;
                J(indx1,indx2) = J(indx1,indx2) + x(k)*dens_num*delta(indx1,indx2)/(1+sum1)^2;
            end
        end
        
        J(indx1,indx1) = J(indx1,indx1) + 1;
    end
end