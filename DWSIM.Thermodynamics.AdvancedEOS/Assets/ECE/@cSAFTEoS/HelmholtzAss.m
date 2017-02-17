function [Aass Xa EoS] = HelmholtzAss(EoS,T,dens_num,mix)
%Calculates the association contribution to the residual Helmholtz energy 
%of mixture mix at temperature T and pressure P using SAFT EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%dens_num: Number density (molecule/Angstrom^3)
%mix: cMixture object
%
%Results:
%Aass: residual Helmholtz energy, association contribution
%Xa: Fraction of associated sites
%EoS: returns EoS used for calculations
%
%Reference Chapman et al., Ind. Eng. Chem. Res. 29 (1990) 1709-1721

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
d = zeros(1,numC);
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
for i = 1:numC
    for j = 1:numC
        term1 = 1/(1-auxil(4));
        term2 = d(i)*d(j)/(d(i)+d(j))*3*auxil(3)/(1-auxil(4))^2;
        term3 = (d(i)*d(j)/(d(i)+d(j)))^2*2*auxil(3)^2/(1-auxil(4))^3;
        ghs(i,j)= term1 + term2 + term3; %Eq. 25 of reference
    end
end

%Calculates the molar fraction of molecules not bonded at association
%sites, Xa
if length(ver('MATLAB')) == 1
	options = optimset('Display','off','MaxIter',50,'MaxFunEvals',50,'TolX',1e-3,'TolFun',1e-3,'Jacobian','on');
else
	options = optimset('MaxIter',50,'MaxFunEvals',50,'TolX',1e-3,'TolFun',1e-3,'Jacobian','on');  %For compatibility with octave which does not support 'Display' option
end

ini = ones(sum(NumAss),1).*1e-3;
[Xa] = fsolve(@(Xa) obj_HelmholtzAss(Xa,EoS,mix,T,NumAss,sigma,d,ghs,dens_num),ini,options);

%Association contribution to Helmholtz energy
Aass = 0;
indx1 = 0;
for i = 1:numC
    sum1 = 0;
    for j = 1:NumAss(i)
        indx1 = indx1 + 1;
        sum1 = sum1 + log(Xa(indx1)) -  Xa(indx1)/2;
    end
    Aass = Aass + x(i)*(sum1 + 0.5*NumAss(i)); %Eq. 21 of reference
end