function this = cSAFTEoS(varargin)
%Constructor of the cSAFTEoS class
%This class implements the SAFT EoS
%Inherits basic functionality from cIdealEoS
%
%This EoS uses the following parameters of cSubstance:
%MW: molecular weight (g/mol)
%EoSParam(1) - m, number of segments per chain
%EoSParam(2) - sigma, segment diameter (Angstrom)
%EoSParam(3) - epsilon/k, depth of pair potential (K)
%EoSParam(4) - Number of association sites (only for associating molecules)
%EoSParam(5) - Effective association volume(only for associating molecules)
%EoSParam(6) - e/k, association energy (K) (only for associating molecules)
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
    'mID', 'SAFT');        %EoS ID
	
this = class(this,'cSAFTEoS',ParentEoS);

%this.mID = 'SAFT';
this.cIdealEoS.ID = 'SAFT';