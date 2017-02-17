function d = HardSphereDiameter(EoS,T,m,sigma,epsilon)
%Hard Sphere Diameter with PC-SAFT EoS
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

fm = 0.0010477 + 0.025337*(m-1)/m; %Eq. 3 of reference

f = (1 + 0.2977*(T/epsilon))/(1 + 0.33163*(T/epsilon) + fm*(T/epsilon)^2); %Eq. 2 of reference

d = sigma*f; %Eq. 1 of reference