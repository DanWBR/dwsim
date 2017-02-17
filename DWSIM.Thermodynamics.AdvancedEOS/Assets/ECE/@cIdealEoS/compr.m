function [Z EoS] = compr(EoS,varargin)
%Calculates the compressibility coefficient of mixture mix at temperature T
%and pressure P
%This is the default implementation and it returns Z = 1 (ideal gas)
%Overload this function together with fugF to define a new EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%
%Results:
%Z: compresibility coefficient
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

Z = 1;