function alpha = alpha_function(EoS,comp,T)
%Implements the standard alpha function of the Peng-Robinson Wong-Sandler EoS
%Auxiliary function, not to be used directly

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
m = 0.37464 + 1.54226*comp.w - 0.26992*comp.w^2;
alpha = (1 + m*(1 - sqrt(Tr)))^2;