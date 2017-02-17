function [alpha EoS] = alpha_function(EoS,comp,T)
%Implements the alpha function of the Peng-Robinson Stryjek-Vera EoS
%Auxiliary function, not to be used directly
%
%Reference: Poling, B. E., Prausnitz, J. M., O'Connell, J. P., 2001. The
%Properties of Gases and Liquids, 5th Ed. McGraw-Hill, New York.

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
m = (0.378893 + 1.4897153*w - 0.17131848*w^2 + 0.0196554*w^3) + comp.EoSParam(1)*(1 + sqrt(Tr))*(0.7 - Tr); %Table 4.7 of reference
alpha = (1 + m*(1 - sqrt(Tr)))^2;
   