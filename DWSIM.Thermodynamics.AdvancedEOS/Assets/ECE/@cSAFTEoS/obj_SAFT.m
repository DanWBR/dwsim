function [result Zcalc] = obj_SAFT(dens_red,EoS,T,P,mix)
%Objective function for the calculation of Z with SAFT EoS
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

%Reads pure-component properties
numC = mix.numC;
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

%Calculates density according to the iterated dens_red
sum1 = 0;
for i = 1:numC
    sum1 = sum1 + mix.x(i)*m(i)*d(i)^3;
end
dens_num = 6/pi*dens_red*sum1^-1; %Eq. 9 of reference

%**************************************************************************
%Compressibility coefficient 
%**************************************************************************
Zseg = Z_seg(EoS,T,dens_num,mix);

Zchain = Z_chain(EoS,T,dens_num,mix);

Zass = Z_ass(EoS,T,dens_num,mix);

%**************************************************************************
%Ojective function
%**************************************************************************
kb = 1.3806504e-23; %Boltzmann K (J/K)

Zcalc = Zseg + Zchain + Zass;

Pcalc = Zcalc*kb*T*dens_num*(1e10)^3;

result = P - Pcalc;