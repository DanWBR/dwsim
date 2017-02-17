function this = cNewEoS(varargin)
%Constructor of the cNewEOS class
%Use this class as template for implementing a new equation of state

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

ParentEoS = cIdealEoS; %Change if a more specific parent EoS can be defined

this = struct( ...
    'mID', 'New');        %Add here additional properties required by the EoS
	
this = class(this,'cNewEoS',ParentEoS);

this.cIdealEoS.ID = 'New'; %Change by a more specific tag