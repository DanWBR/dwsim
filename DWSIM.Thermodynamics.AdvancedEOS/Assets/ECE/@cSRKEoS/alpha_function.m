function [alpha EoS] = alpha_function(EoS,comp,T)
%Implements the standard alpha function of the SRK EoS
%Auxiliary function, not to be used directly
%
%Reference: Walas, Phase Equilibria in Chemical Engineering,
%Butterword-Heineman, Newton MA (1985)

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

Tr = T/comp.Tc;
w = comp.w;
m = 0.48508 + 1.55171*w - 0.15613*w^2;
alpha = (1 + m*(1 - sqrt(Tr)))^2; % Eq. 4, Table 1.11 of reference