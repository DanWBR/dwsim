function this = cPHSCEoS(varargin)
%Constructor of the cPHSCEoS class
%This class implements the Perturbed Hard Sphere Chain EoS
%Inherits basic functionality from cIdealEoS
%
%This EoS uses the following parameters of cSubstance:
%MW: molecular weight (g/mol)
%EoSParam(1) - V, characteristic volume (m3/mol)
%EoSParam(2) - A, characteristic surface area (m2/mol)
%EoSParam(3) - E, characteristic cohesive energy (J/mol)
%This EoS uses the following interaction parameters of cMixture:
%k1: Binary interaction parameter k

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
    'mID', 'Perturbed Hard Sphere Chain');        %EoS ID
	
this = class(this,'cPHSCEoS',ParentEoS);

this.cIdealEoS.ID = 'Perturbed Hard Sphere Chain';