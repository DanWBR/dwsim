function this = CheckMolFrac(this)
%Checks the consistency of the molar fracion vector
%(Private function, not to be used directly)

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

%Flag 0 if no inconsitencies are detected
flag = 0;

%Checks the number of components
comps = this.mComponents;
if size(comps,1) > size(comps,2)
    comps = comps';
    this.mComponents = comps;
end

molFrac = this.mMolFrac;

if length(molFrac) ~= length(comps)
    %Fills with zeros unspecified mol fractions
    if length(molFrac) < length(comps)
        flag = 1;
        for i = length(molFrac):length(comps)
            molFrac(i) = 0;
        end
        this.mMolFrac = molFrac;
    end

    %Removes extra numbers in mol fraction vector
    if length(molFrac) > length(comps)
        flag = 1;
        molFrac_sav = molFrac;
        molFrac = zeros(length(comps));
        for i = 1:length(comps)
            molFrac(i) = molFrac_sav(i);
        end
        this.mMolFrac = molFrac;
    end
end

total = sum(molFrac);

%Checks that mol fractions add 1
if abs(total - 1) > 1e-4
    flag = 1;
end

if flag == 1
   warning('MATLAB:EoS', 'Inconsistence corrected in molar fraction vector.');
end