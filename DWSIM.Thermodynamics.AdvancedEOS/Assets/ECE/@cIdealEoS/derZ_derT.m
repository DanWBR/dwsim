function res = derZ_derT(P,EoS,mix,T,phase)
%Calculates the integrand for the calculation of residual enthalpy:
%(dZ/dT)_P dP/P
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

res = zeros(length(P),1);
for i = 1:length(P)
    Z1 = compr(EoS,T,P(i),mix,phase);
    Z2 = compr(EoS,T+1e-3,P(i),mix,phase);

    res(i) = (Z2 - Z1)/1e-3*1/P(i);
end