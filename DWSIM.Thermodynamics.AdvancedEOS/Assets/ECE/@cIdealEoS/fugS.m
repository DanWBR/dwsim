function f = fugS(EoS,T,P,mix,varargin)
%Calculates the fugacity coefficient of a solid mixture mix at temperature T
%and pressure P
%Refers the fugacity of the solid phase to the fugacity of a hypothetical,
%reference subcooled liquid
%(Source: Shariati and Peters, J. Supercrit. Fluids 23 (2002) 195-208
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (K)
%mix: cMixture object
%
%Results:
%f: fugacity coefficient
%EoS: returns EoS used for calculations

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

f = zeros(mix.numC,1);
for i = 1:mix.numC
    if abs(mix.comp(i).Tf - 0) < 1e-3
        f(i) = 1e300; %Components with Tf = 0 K are considered not to form solid phases
    else
        %Calculates the  fugacity in the reference, subcooled liquid
        mixP = cMixture;
        mixP.comp = mix.comp(i);
        mixP.x = 1;
        
        fL = fug(EoS,T,P,mixP,'liq');
        
        %Calculates the fugacity of the solid phase
        
        lnf = log(fL) + mix.comp(i).Hf/8.31*(1/mix.comp(i).Tf - 1/T); %Eq. 22 of reference article
        
        f(i) = exp(lnf);
    end
end

