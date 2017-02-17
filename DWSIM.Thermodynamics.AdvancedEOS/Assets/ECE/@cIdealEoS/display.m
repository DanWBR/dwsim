function display(this)
%Displays the properties of cIdealEoS objects in the command window

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

%handle empty inputname
display_name = inputname(1);
if isempty(display_name)
    display_name = 'ans';
end

%handle scalar vs. vector this
if length(this) == 1 %scalar case
    disp([inputname(1) ' = ' this.mID ' Equation of State']);
    
else %array case
    s = sprintf('cIdealEoS object: %d by %d', size(this,1), size(this,2));
	disp(s);
end