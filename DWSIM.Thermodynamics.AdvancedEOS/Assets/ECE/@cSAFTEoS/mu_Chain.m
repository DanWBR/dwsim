function [muchain EoS] = mu_Chain(EoS,T,dens_num,mix)
%Calculates the chain contribution to the residual chemical potential 
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

NA = 6.022e23; %Avogadro's number

%Reads pure-component properties
m = zeros(1,mix.numC);
sigma = zeros(1,mix.numC);
epsilon = zeros(1,mix.numC);
for i = 1:mix.numC
    m(i)= mix.comp(i).EoSParam(1);
    sigma(i) = mix.comp(i).EoSParam(2);
    epsilon(i) = mix.comp(i).EoSParam(3);
end

%Calculates the temperature-dependant segment diameter
d = zeros(1,mix.numC);
for i = 1:mix.numC
    d(i) = HardSphereDiameter(EoS,T,m(i),sigma(i),epsilon(i));
end

%auxiliary functions
auxil = zeros(1,4);
for j = 1:4
    for i = 1:mix.numC
        auxil(j) = auxil(j) + mix.x(i)*m(i)*d(i)^(j-1);
    end
    auxil(j) = auxil(j)*pi/6*dens_num; %Eq. 27 of reference
end 

%radial distribution function
ghs = zeros(mix.numC, mix.numC);
for i = 1:mix.numC
    for j = 1:mix.numC
        term1 = 1/(1-auxil(4));
        term2 = d(i)*d(j)/(d(i)+d(j))*3*auxil(3)/(1-auxil(4))^2;
        term3 = (d(i)*d(j)/(d(i)+d(j)))^2*2*auxil(3)^2/(1-auxil(4))^3;
        ghs(i,j)= term1 + term2 + term3; %Eq. 25 of reference
    end
end

dlnghs_dro = zeros(mix.numC,mix.numC);
for i = 1:mix.numC
    for j = 1:mix.numC
        term1 = d(i)^3/(1-auxil(4))^2;
        term2 = (3/2)*d(j)*d(i)^2/(1-auxil(4))^2;
        term3 = 3*d(j)*d(i)^3*auxil(3)/(1-auxil(4))^3;
        term4 = d(j)^2*d(i)^2*auxil(3)/(1-auxil(4))^3;
        term5 = (3/2)*d(j)^2*d(i)^3*auxil(3)^2/(1-auxil(4))^4;
        dlnghs_dro(j,i) = pi/6*NA*m(i)/ghs(j,j)*(term1 + term2 + term3 + term4 + term5); %Eq. A7 of reference
    end
end

muchain = zeros(mix.numC,1);
for i = 1:mix.numC
    sumat = 0;
    for j = 1:mix.numC
        sumat = sumat + mix.x(j)*dens_num/NA*(1-m(j))*dlnghs_dro(j,i);
    end
    
    muchain(i) = (1-m(i))*log(ghs(i,i)) + sumat; %Eq. A6 of reference
end