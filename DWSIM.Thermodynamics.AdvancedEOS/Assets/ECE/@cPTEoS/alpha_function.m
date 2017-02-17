function [alfa, Omega_a, Omega_b, Omega_c EoS] = alpha_function(EoS,mix,T)
%Implements the alpha function of the Patel-Teja EoS
%Auxiliary function, not to be used directly
%
%Reference: Patel and Teja, Chem. Eng. Sci. 37 (1982) 463-473

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

numC = mix.numC;
alfa = zeros(numC,1);
zc = zeros(numC,1);
FPT = zeros(numC,1);
Omega_a = zeros(numC,1);
Omega_b = zeros(numC,1);
Omega_c = zeros(numC,1);
Tc = zeros(numC,1);
Tr = zeros(numC,1);

for i = 1:numC
   Tc(i) = mix.comp(i).Tc;
   Tr(i) = T/Tc(i); 
    
   zc(i) = mix.comp(i).EoSParam(1);
   FPT(i) = mix.comp(i).EoSParam(2);
   
   Omega_c(i) = 1 - 3*zc(i); %Eq. 8 of reference
   Omega_br = roots([1 (2-3*zc(i)) 3*(zc(i)^2) -zc(i)^3]); %Eq. 10 of reference (start)
   Omega_brr = [];
   for ii = 1:3
    if isreal(Omega_br(ii)) && Omega_br(ii)>=0
   		Omega_brr = [Omega_brr Omega_br(ii)];   
   	end
   end
   Omega_b(i) = min(Omega_brr); %Eq. 10 of reference (end)
   Omega_a(i) = 3*(zc(i)^2) + 3*(1-2*zc(i))*Omega_b(i) + Omega_b(i)^2 + 1 -3*zc(i); %Eq. 9 of reference    
   alfa(i) = (1 + FPT(i)*(1 - sqrt(Tr(i))))^2;
end
   