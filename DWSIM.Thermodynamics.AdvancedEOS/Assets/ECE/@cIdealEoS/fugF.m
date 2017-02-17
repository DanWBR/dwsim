function [f EoS] = fugF(EoS,T,P,mix,phase,varargin)
%Calculates the fugacity coefficient of a fluid mixture mix at temperature T
%and pressure P
%This is the default implementation and returns fug(gas) = 1 and
%fug(liquid) = Psat/P (Raoult's Law)
%Overload this function together with compr to define a new EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (K)
%mix: cMixture object
%phase: set phase = 'liq' to calculate the fugacity of a liquid phase or
%   phase = 'gas' to calculate the fugacity of a gas phase
%
%Results:
%fug: fugacity coefficient
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

%Calculates fugacity coefficient with Raoult's Law
f = zeros(mix.numC,1);
for i = 1:mix.numC
    if strcmp(phase,'gas') == 1
        f(i) = 1;
    elseif strcmp(phase,'liq') == 1
        comp = mix.comp(i);
        Psat = exp(comp.AntA - comp.AntB/(T+comp.AntC));
        f(i) = Psat/P;
    else
        error(['The value "' liquid '" of "phase" parameter is incorrect.']);
    end
end