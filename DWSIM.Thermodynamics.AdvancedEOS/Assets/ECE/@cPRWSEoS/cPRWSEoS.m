function this = cPRWSEoS(varargin)
%Constructor of the cPRWSEoS class
%This class implements the Peng-Robinson EoS with Wong-Sandler 
%mixing rules based on NRTL and standard alpha function
%Inherits basic functionality from cIdealEoS
%
%This EoS uses the following parameters of cSubstance:
%MW: molecular weight (g/mol)
%Tc: Critical Temperature (K)
%Pc: Critical Pressure (Pa)
%w: Acentric factor
%This EoS uses the following interaction parameters of cMixture:
%k1: Binary interaction parameter k (quadratic mixing rules)
%k2: Non-randomness parameter
%k3: Binary interaction parameter tau 

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

ParentEoS = cIdealEoS;

this = struct( ...
    'mID', 'Peng-Robinson Wong-Sandler');        %EoS ID
	
this = class(this,'cPRWSEoS',ParentEoS);

this.cIdealEoS.ID = 'Peng-Robinson Wong-Sandler';