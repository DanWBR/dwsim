function this = cSRKEoS(varargin)
%Constructor of the cPREoS class
%This class implements the Soave-Redlich-Kwong EoS with Van der Waals quadratic
%mixing rules and standard alpha function
%Inherits basic functionality from cIdealEoS
%
%This EoS uses the following parameters of cSubstance:
%MW: molecular weight (g/mol)
%Tc: Critical Temperature (K)
%Pc: Critical Pressure (Pa)
%w: Acentric factor
%This EoS uses the following interaction parameters of cMixture:
%k1: Binary interaction parameter k (quadratic mixing rules)
%k2: Binary interaction parameter l (quadratic mixing rules)
%
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
    'mID', 'Soave-Redlich-Kwong');        %EoS ID
	
this = class(this,'cSRKEoS',ParentEoS);

this.cIdealEoS.ID = 'Soave-Redlich-Kwong';