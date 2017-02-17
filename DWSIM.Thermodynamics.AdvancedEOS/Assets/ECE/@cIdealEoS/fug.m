function [f, EoS] = fug(EoS,T,P,mix,phase,varargin)
%Calculates the fugacity coefficient of a mixture mix at temperature T
%and pressure P
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (K)
%mix: cMixture object
%liquid: set phase = 'liq' to calculate the fugacity of a liquid phase 
%   phase = 'gas' to calculate the fugacity of a gas phase
%   phase = 'sol' to calculate the fugacity of a solid phase
%
%Results:
%f: fugacity coefficient
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

if (strcmp(phase,'gas') == 1) || (strcmp(phase,'liq') == 1)
    f = fugF(EoS,T,P,mix,phase,varargin{:});
elseif strcmp(phase,'sol') == 1
    f = fugS(EoS,T,P,mix,varargin{:});
else
    error(['The value "' phase '" of "phase" parameter is incorrect.'])
end