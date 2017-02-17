function this = SetkG(this,i,j,indx,val)
%Sets the interaction coefficient indx between groups i and j of GCEoS
%
%Parameters:
%EoS: cGCEoS object
%i: group index
%j: group index
%indx: changed interaction coefficient, 'k1', 'k2' or 'alfa'
%val: value of interaction coefficent
%
%Results:
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

switch indx
    case 'k1'
        this.mk1G(i,j) = val;
        this.mk1G(j,i) = val;
    case 'k2'
        this.mk2G(i,j) = val;
        this.mk2G(j,i) = val;
    case 'alfa'
        this.malfaG(i,j) = val;
    otherwise
        error('Reference to non-existent interaction coefficient.');
end