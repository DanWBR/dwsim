function [alfa, Omega_a, Omega_b, Omega_c EoS] = alpha_function(EoS,mix,T)
%Implements the alpha function of the Patel-Teja EoS
%Auxiliary function, not to be used directly
%
%Reference: Valderrama, J. Chem. Eng. Jpn 23 (1990) 87-91

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
Tr = zeros(numC,1);
alfa = zeros(numC,1);
zc = zeros(numC,1);
FPT = zeros(numC,1);
Omega_a = zeros(numC,1);
Omega_b = zeros(numC,1);
Omega_c = zeros(numC,1);

for i = 1:mix.numC %Table 2 of Reference
   Tr(i) = T/mix.comp(i).Tc;
   
   zc(i) = mix.comp(i).EoSParam(1);
   
   Omega_a(i) = 0.66121 - 0.76105*zc(i);
   Omega_b(i) = 0.02207 + 0.20868*zc(i);
   Omega_c(i) = 0.57765 - 1.87080*zc(i);
   
   w = mix.comp(i).w;
   
   FPT(i) = 0.46283 + 3.58230*(zc(i)*w)+8.19417*(zc(i)*w)^2;
   
   alfa(i) = (1 + FPT(i)*(1 - sqrt(Tr(i))))^2;
end
   