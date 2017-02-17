function [Ahc EoS] = HelmholtzHC(EoS,T,dens_num,mix)
%Calculates the Hard Chain contribution to the residual Helmholtz energy 
%of mixture mix at temperature T and pressure P using PC-SAFT EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%dens_num: Number density (molecule/Angstrom^3)
%mix: cMixture object
%
%Results:
%Ahc: residual Helmholtz energy, association contribution
%EoS: returns EoS used for calculations
%
%Reference: Gross and Sadowski, Ind. Eng. Chem. Res. 40 (2001) 1244-1260

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
for i = 1:numC
    m(i)= mix.comp(i).EoSParam(1);
    sigma(i) = mix.comp(i).EoSParam(2);
    epsilon(i) = mix.comp(i).EoSParam(3);
end

%Calculates the temperature-dependant segment diameter
d = zeros(1,numC);
for i = 1:numC
    d(i) = HardSphereDiameter(EoS,T,m(i),sigma(i),epsilon(i));
end

%mean segment number
m_prom = 0;
for i = 1:numC
    m_prom = m_prom + m(i)*x(i); %Eq. 6 of reference
end

%auxiliary functions
auxil = zeros(1,4);
for j = 1:4
    for i = 1:numC
        auxil(j) = auxil(j) + x(i)*m(i)*d(i)^(j-1);
    end
    auxil(j) = auxil(j)*pi/6*dens_num; %Eq. 9 of reference
end

%radial distribution function
ghs = zeros(numC,numC);
for i = 1:numC
    for j = 1:numC
        term1 = 1/(1-auxil(4));
        term2 = d(i)*d(j)/(d(i)+d(j))*3*auxil(3)/(1-auxil(4))^2;
        term3 = (d(i)*d(j)/(d(i)+d(j)))^2*2*auxil(3)^2/(1-auxil(4))^3;
        ghs(i,j)= term1 + term2 + term3; %Eq. 8 of reference
    end
end

%Helmholtz energy
term1 = 3*auxil(2)*auxil(3)/(1-auxil(4));
term2 = auxil(3)^3/(auxil(4)*(1-auxil(4))^2);
term3 = (auxil(3)^3/auxil(4)^2-auxil(1))*log(1-auxil(4));
a_hs = (1/auxil(1))*(term1 + term2 + term3);

sum1 = 0;
for i = 1:numC
    sum1 = sum1 + x(i)*(m(i)-1)*log(ghs(i,i));
end

Ahc = m_prom*a_hs - sum1; %Eq. A4 of reference