function [index, Label, Tref, q, g1, g2, g3] = GetGroup(this,index)
%Gets the parameters of a group of the GC-EoS
%
%Parameters:
%EoS: cGCEoS object
%index: Group index
%
%Results:
%index: group index
%Label: label of the group
%Tref: reference temperature (K)
%q: q parameter
%g1: g parameter
%g2: g' parameter
%g3: g'' parameter

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

group = this.mGroup(index);

Label = group.Label;
Tref = group.Tref;
q = group.q;
g1 = group.g1;
g2 = group.g2;
g3 = group.g3;