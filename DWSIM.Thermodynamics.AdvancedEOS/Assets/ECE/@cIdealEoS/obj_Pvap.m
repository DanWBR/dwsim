function [res flag] = obj_Pvap(P,EoS,comp,T,flag)
%Objective function for the calculation of vapor pressure
%Auxiliary function, not to be used directly

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

P = max(P,1);

mix = cMixture;
mix.comp = comp;
mix.x = 1;

%Fugacity of the liquid
fL = fug(EoS,T,P,mix,'liq');
ZL = compr(EoS,T,P,mix,'liq');

%Fugacity of the gas
fG = fug(EoS,T,P,mix,'gas');
ZG = compr(EoS,T,P,mix,'gas');

%Condition of equal fugacity
res = fL-fG;

%Checks if the result is in the single phase region
if abs(ZL-ZG) < 1e-4
    flag = 2;
end

pause(0.01); %Prevents hang of Matlab GUI