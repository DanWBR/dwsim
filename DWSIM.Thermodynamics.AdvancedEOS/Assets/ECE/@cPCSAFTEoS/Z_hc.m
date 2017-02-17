function [Zhc EoS] = Z_hc(EoS,T,dens_num,mix)
%Hard-chain contribution to the compressibility coefficient with PC-SAFT EoS
%Auxiliary function, not to be used directly
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

%**************************************************************************
%Zhc
%**************************************************************************
term1 = auxil(4)/(1-auxil(4));
term2 = 3*auxil(2)*auxil(3)/(auxil(1)*(1-auxil(4))^2);
term3 = (3*auxil(3)^3-auxil(4)*auxil(3)^3)/(auxil(1)*(1-auxil(4))^3);
Zhs = term1 + term2 + term3; %Eq. A26 of reference

dens_dg_ddens = zeros(mix.numC,mix.numC);
for i = 1:numC
    for j = 1:numC
        term1 = auxil(4)/(1-auxil(4))^2;
        term2 = (d(i)*d(j))/(d(i)+d(j))*(3*auxil(3)/(1-auxil(4))^2+6*auxil(3)*auxil(4)/(1-auxil(4))^3);
        term3 = (d(i)*d(j)/(d(i)+d(j)))^2*(4*auxil(3)^2/(1-auxil(4))^3 + 6*auxil(3)^2*auxil(4)/(1-auxil(4))^4);
        dens_dg_ddens(i,j) = term1 + term2 + term3; %Eq. A27 of reference
    end
end

sum1 = 0;
for i = 1:numC
    sum1 = sum1 + x(i)*(m(i)-1)*ghs(i,i)^(-1)*dens_dg_ddens(i,i);
end

Zhc = m_prom*Zhs - sum1; %Eq. A25 of reference