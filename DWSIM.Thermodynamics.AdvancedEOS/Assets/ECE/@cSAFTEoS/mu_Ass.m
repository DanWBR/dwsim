function [muass EoS] = mu_Ass(EoS,T,dens_num,mix)
%Calculates the association contribution to the residual chemical potential 
%of mixture mix at temperature T and pressure P using SAFT EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (K)
%dens_num: Number density (molecule/Angstrom^3)
%mix: cMixture object
%
%Results:
%muass: residual chemical potential, association contribution
%EoS: returns EoS used for calculations
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

%Reads pure-component properties
numC = mix.numC;
x = mix.x;
m = zeros(1,numC);
sigma = zeros(1,numC);
epsilon = zeros(1,numC);
NumAss = zeros(1,numC);
for i = 1:numC
    m(i)= mix.comp(i).EoSParam(1);
    sigma(i) = mix.comp(i).EoSParam(2);
    epsilon(i) = mix.comp(i).EoSParam(3);
    NumAss(i) = mix.comp(i).EoSParam(4);
end


%Calculates the temperature-dependant segment diameter
d = zeros(1,mix.numC);
for i = 1:numC
    d(i) = HardSphereDiameter(EoS,T,m(i),sigma(i),epsilon(i));
end

%auxiliary functions
auxil = zeros(1,4);
for j = 1:4
    for i = 1:numC
        auxil(j) = auxil(j) + x(i)*m(i)*d(i)^(j-1);
    end
    auxil(j) = auxil(j)*pi/6*dens_num; %Eq. 27 of reference
end 

%radial distribution function
ghs = zeros(numC, numC);
for i = 1:mix.numC
    for j = 1:mix.numC
        term1 = 1/(1-auxil(4));
        term2 = d(i)*d(j)/(d(i)+d(j))*3*auxil(3)/(1-auxil(4))^2;
        term3 = (d(i)*d(j)/(d(i)+d(j)))^2*2*auxil(3)^2/(1-auxil(4))^3;
        ghs(i,j)= term1 + term2 + term3; %Eq. 25 of reference
    end
end

%Calculates the molar fraction of molecules not bonded at association
%sites, Xa
if length(ver('MATLAB')) == 1
	options = optimset('Display','off','MaxIter',20,'MaxFunEvals',20,'TolX',1e-2,'TolFun',1e-2,'Jacobian','on');
else
	options = optimset('MaxIter',20,'MaxFunEvals',20,'TolX',1e-2,'TolFun',1e-2,'Jacobian','on');  %For compatibility with octave which does not support 'Display' option
end

ini = ones(sum(NumAss),1).*1e-3;
[Xa] = fsolve(@(Xa) obj_HelmholtzAss(Xa,EoS,mix,T,NumAss,sigma,d,ghs,dens_num),ini,options);

%Derivatives for calculation of chemical potential
dgij_drok = zeros(numC, numC, numC);
for i = 1:numC
    for j = 1:numC
        for k = 1:numC
            term1 = d(i)^3/(1-auxil(4))^2;
            term2 = 3*d(j)*d(k)/(d(j)+d(k));
            term3 = d(i)^2/(1-auxil(4))^2;
            term4 = 2*d(i)^3*auxil(3)/(1-auxil(4))^3;
            term5 = 2*(d(j)*d(k)/(d(j)+d(k)))^2;
            term6 = 2*d(i)^2*auxil(3)/(1-auxil(4))^3;
            term7 = 3*d(i)^3*auxil(3)^2/(1-auxil(4))^4;
            dgij_drok(j,k,i) = pi/6*m(i)*(term1 + term2*(term3 + term4) + term5*(term6+term7)); %Eq. A5 of reference
        end
    end
end

for i2 = 1:numC
    indx1 = 0;
    for i = 1:numC
        for j = 1:NumAss(i)
            indx1 = indx1 + 1;
            indx2 = 0;
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
                    ddeltaAB_droi(indx1,indx2,i2) = ((d(i)+d(k))/2)^3*dgij_drok(i,k,i2)*(exp(epsilon/T)-1)*kappa;
                end
            end
        end
    end
end

dXaj_droi_v = obj_muAss(EoS,mix,Xa,ddeltaAB_droi,T,NumAss,sigma,d,ghs,dens_num); %Eq. A3 of reference

%Transforms column-vector parameter dXaj_droi into a matrix
for i2 = 1:numC
    indx1 = 0;
    for i = 1:numC
        for j = 1:NumAss(i)
            indx1 = indx1 + 1;
            dXaj_droi(indx1,i2) = dXaj_droi_v((i2-1)*sum(NumAss)+indx1);
        end
    end
end

%Association contribution to the chemical potential
indx1 = 0;
for i = 1:numC
    sum1 = 0;
    for j = 1:NumAss(i)
        indx1 = indx1 + 1;
        sum1 = sum1 + log(Xa(indx1))-Xa(indx1)/2;
    end
    term1(i) = sum1 + 0.5*NumAss(i);
end

for i = 1:numC
    indx1 = 0;
    sum1 = 0;
    for j = 1:numC
        for k = 1:NumAss(j)
            indx1 = indx1 + 1;
            sum1 = sum1 + dens_num*x(j)*(dXaj_droi(indx1,i)*(1/Xa(indx1)-0.5));
        end
    end
    term2(i) = sum1;
end

muass = zeros(1,numC);
for i = 1:numC
    muass(i) = term1(i) + term2(i); %Eq A2 of reference
end