function [Zseg Z0_seg EoS] = Z_seg(EoS,T,dens_num,mix)
%Segment contribution to the compressibility coefficient with SAFT EoS
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

%Reduced density
dens_red = 0;
for i = 1:mix.numC
    dens_red = dens_red + mix.x(i)*m(i)*d(i)^3;
end
dens_red = dens_red*pi/6*dens_num; 
ro_r = 6*dens_red/(2^0.5*pi);

%Mixing rules
sigmaij = zeros(mix.numC, mix.numC);
epsilonij = zeros(mix.numC, mix.numC);
for i =1:mix.numC
    for j = 1:mix.numC
        sigmaij(i,j) = 0.5*(sigma(i) + sigma(j)); %Eq. 7 of reference
        epsilonij(i,j) = sqrt(epsilon(i)*epsilon(j)) * (1 - mix.k(i,j)); %Eq 6 of reference           
    end
end
sumat1 = 0;
for i = 1:mix.numC
    sumat1 = sumat1 + mix.x(i)*m(i);
end
sumat2 = 0;
sumat3 = 0;
for i = 1:mix.numC
    for j = 1:mix.numC
        sumat2 = sumat2 + mix.x(i)*mix.x(j)*m(i)*m(j)*sigmaij(i,j)^3;
        sumat3 = sumat3 + mix.x(i)*mix.x(j)*m(i)*m(j)*sigmaij(i,j)^3*epsilonij(i,j);
    end
end

sigma_m = (sumat2/sumat1^2)^(1/3); %Eq. 4 of reference
epsilon_m = (sumat3/sumat1^2)/sigma_m^3; %Eq. 5 of reference

%Compressibility coefficient
Z0_hs = (1 + dens_red + dens_red^2 - dens_red^3)/(1-dens_red)^3; %Eq. A15 of reference

Z01_disp = ro_r*(-8.595 - 2*(4.5424*ro_r) - 3*(2.1268*ro_r^2) + 4*(10.285*ro_r^3)); %Eq. A17 of reference

Z02_disp = ro_r*(-1.9075 + 2*(9.9724*ro_r) - 3*(22.216*ro_r^2) + 4*(15.904*ro_r^3)); %Eq. A18 of reference

Z0_disp = Z01_disp/(T/epsilon_m) + Z02_disp/(T/epsilon_m)^2; %Eq. A16 of reference

Z0_seg = Z0_hs + Z0_disp; %Eq. A14 of reference

Zseg = 1 + (Z0_seg-1)*sumat1; %Eq. A13 of reference