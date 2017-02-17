function this = cPSRKEoS(varargin)
%Constructor of the cPSRKEoS class
%This class implements the Predictive Soave Redlich Kwong EoS
%Inherits basic functionality from cIdealEoS
%
%This EoS uses the following parameters of cSubstance:
%MW: molecular weight (g/mol)
%Tc: Critical Temperature (K)
%Pc: Critical Pressure (Pa)
%EoSParam(1) - c1 (alpha function parameter)
%EoSParam(2) - c2 (alpha function parameter)
%EoSParam(3) - c3 (alpha function parameter)
%EoSParam(4) - Group decomposition

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
    'mID', 'Predictive Soave-Redlich-Kwong',...
	'mGroup', [],...          %Groups 
    'maG', [],...             %Group interaction coefficient a
    'mbG', [],...             %Group interaction coefficient b
    'mcG', []);               %Group interaction coefficient c  

this = class(this,'cPSRKEoS',ParentEoS);

this = FillDefaultGroups(this); %Introduces the default group parameters and interaction coefficients

this.cIdealEoS.ID = 'Predictive Soave-Redlich-Kwong';