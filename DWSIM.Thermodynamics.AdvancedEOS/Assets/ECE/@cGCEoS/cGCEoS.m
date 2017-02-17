function this = cGCEoS(varargin)
%Constructor of the cGCEoS class
%This class implements the Group Contribution EoS
%Inherits basic functionality from cIdealEoS
%
%This EoS uses the following parameters of cSubstance:
%MW: molecular weight (g/mol)
%Tc: Critical Temperature (K)
%EoSParam(1) - dc - critical diameter (cm/mol)
%EoSParam(2) - group decomposition

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

this = struct('mID', 'Group Contribution',... %ID
	'mGroup', [],...          %Groups 
    'mk1G', [],...            %Group interaction coefficient k1
    'mk2G', [],...            %Group interaction coefficient k2
    'malfaG', []);            %Group interaction coefficient alfa 

this = class(this,'cGCEoS',ParentEoS);

this = FillDefaultGroups(this); %Introduces the default group parameters and interaction coefficients

this.cIdealEoS.ID = 'Group Contribution';