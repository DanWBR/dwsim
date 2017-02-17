function massFrac = massFrac(this)
%Returns the mass fraction composition of the mixture

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

this = CheckMolFrac(this);

comp = this.mComponents;

numC = max(size(comp));

mass = 0;
for i = 1:numC
    mass = mass + this.mMolFrac(i) * comp(i).MW;
end

massFrac = zeros(1,numC);
if mass ~= 0 %Avoids divisions by zero
    for i = 1:numC
        massFrac(i) = this.mMolFrac(i) * comp(i).MW / mass;
    end
end