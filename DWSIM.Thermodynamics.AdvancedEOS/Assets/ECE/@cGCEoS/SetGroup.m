function this = SetGroup(this,Index,Label,Tref,q,g1,g2,g3)
%Sets the parameters of a group of GC EoS
%
%Parameters:
%EoS: cGCEoS object
%Index: Group index
%Label: label of the group
%Tref: Refernce temperature of the group (K)
%q: q parameter of the group
%g1: g parameter of the group
%g2: g' parameter of the group
%g3: g'' parameter of the group
%
%Result:
%EoS: cGCEoS object

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


group = struct( ...
    'Index', Index, ...
    'Label', Label, ...
    'Tref', Tref, ...
    'q', q,...
    'g1', g1,...
    'g2', g2,...
    'g3', g3);

this.mGroup(Index) = group;

this.mk1G(Index,Index) = 1;