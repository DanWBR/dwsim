function [Zass EoS] = Z_ass(EoS,T,dens_num,mix)
%Associating contribution to the compressibility coefficient with SAFT EoS
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
x = mix.x;

NumAss = zeros(1,numC);
for i = 1:numC
    NumAss(i) = mix.comp(i).EoSParam(4);
end

if sum(NumAss > 0)
    muass = mu_Ass(EoS,T,dens_num,mix);
    Aass = HelmholtzAss(EoS,T,dens_num,mix);

    sum1 = 0;
    for i = 1:numC
        sum1 = sum1 + x(i)*muass(i);
    end

    Zass = sum1 - Aass; %Eq. A10 of reference
else
   Zass = 0;
end