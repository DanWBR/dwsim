function d = HardSphereDiameter(EoS,T,m,sigma,epsilon)
%Hard Sphere Diameter with PC-SAFT EoS
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

d = sigma*(1-0.12*exp(-3*epsilon/T)); %Eq. 3 of reference