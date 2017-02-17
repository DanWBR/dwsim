function this = cIdealEoS(varargin)
%Constructor of the cIdealEOS class
%This class implements calculations with ideal mixtures (ideal gas +
%Raoult's law)
%
%This class is the base class for all EoS classes
%Overload the compr and fugF functions to define a new equation of state

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

%Defalut constructor
this = struct( ...
    'mID', 'Ideal Gas');        %EoS ID

this = class(this,'cIdealEoS');