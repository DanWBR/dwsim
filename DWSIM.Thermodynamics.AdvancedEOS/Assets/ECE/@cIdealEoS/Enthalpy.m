function [Hres,EoS] = Enthalpy(EoS,mix,T,P,phase)
%function [Hres,EoS] = Enthalpy(EoS,mix,T,P,phase)
%
%Calculates the residual enthalpy of the mixture
%This function calculates the residual enthalpy numerically
%Overload it with an equation of state-specific analytical equation
%
%Parameters:
%EoS: Equation of State used for calculations
%mix: mixture (cMixture object)
%T: temperature (K)
%P: pressure (Pa)
%phase: set phase = 'liq' to calculate the density of a liquid phase or
%   phase = 'gas' to calculate the density of a gas phase
%
%Results:
%
%Hres: residual enthalpy (J/mol)
%EoS: returns EoS used for calculations

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

Hres = 8.31*T^2*quadl('derZ_derT',1,P,1e-3,0,EoS,mix,T,phase);