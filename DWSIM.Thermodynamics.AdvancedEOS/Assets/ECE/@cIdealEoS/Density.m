function [denMass,denMol,Z,EoS] = Density(EoS,mix,T,P,phase)
%function [denMass,denMol,Z,EoS] = Density(EoS,mix,T,P,phase)
%
%Calculates the density of the mixture
%
%Parameters:
%EoS: Equation of State used for calculations
%mix: mixture (cMixture object)
%T: temperature (K)
%P: pressure (Pa)
%phase: set phase = 'liq' to calculate the density of a liquid phase or
%  phase = 'gas' to calculate the density of a gas phase
%
%Results:
%denMass: density (kg/m3)
%denMol: molar density (mol/m3)
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

R = 8.31; %Gas constant, J/mol K

%Calculates the compressibility coefficient
Z = compr(EoS,T,P,mix,phase);

%Calculates the molar volume
V = Z*R*T/P; %m3/mol

%Calculates the molar density
denMol = 1/V; %mol/m3

%Calculates the mass density
denMass = denMol * mix.MW / 1000; %kg/m3
