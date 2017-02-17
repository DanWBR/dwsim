function this = SetkG(this,i,j,indx,val)
%Sets the interaction coefficient indx between groups i and j of PSRK
%EoS
%
%Parameters:
%EoS: cPSRKEoS object
%i: group index
%j: group index
%indx: required interaction coefficient, 'a', 'b' or 'c'
%val: value of interaction coefficent
%
%Results:
%EoS: cPSRKEoS object

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
    case 'a'
        this.maG(i,j) = val;
    case 'b'
        this.mbG(i,j) = val;
    case 'c'
        this.mcG(i,j) = val;
    otherwise
        error('Reference to non-existent interaction coefficient.');
end