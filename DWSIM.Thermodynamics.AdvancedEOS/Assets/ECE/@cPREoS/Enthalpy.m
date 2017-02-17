function [Hres,EoS] = Enthalpy(EoS,mix,T,P,phase)
%Calculates the residual enthalpy of the mixture
%
%Parameters:
%EoS: Equation of State used for calculations
%mix: mixture (cMixture object)
%T: temperature (K)
%P: pressure (Pa)
%phase: set phase = 'liq' to calculate the density of a liquid phase or
%   phase = 'gas' to calculate the density of a gas phase
%
%Results:
%
%Hres: residual enthalpy (J/mol)
%EoS: returns EoS used for calculations(EoS,mix,T,P,liquid)

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

R = 8.31; %Ideal gas constant, J/mol K

%Calculates the compresibility coefficient
Z = compr(EoS,T,P,mix,phase);

%Reduced variables
Tr = zeros(mix.numC,1);
Pr = zeros(mix.numC,1);
for i = 1:mix.numC
   Tr(i) = T/mix.comp(i).Tc;
   Pr(i) = P/mix.comp(i).Pc;
end

%Pure component parameters
alpha = zeros(mix.numC,1);
a = zeros(mix.numC,1);
b = zeros(mix.numC,1);
for i = 1:mix.numC
   alpha(i) = alpha_function(EoS,mix.comp(i),T); %Evaluates the alpha function (may differ in modifications of PR-EoS)
   a(i) = 0.45724*(R*mix.comp(i).Tc)^2/mix.comp(i).Pc*alpha(i);
   b(i) = 0.0778*R*mix.comp(i).Tc/mix.comp(i).Pc;
end

%Mixing rules
aij = zeros(mix.numC,mix.numC);
bij = zeros(mix.numC,mix.numC);
for i = 1:mix.numC
   for j = 1:mix.numC
      aij(i,j) = (1-mix.k1(i,j))*sqrt(a(i)*a(j));
      bij(i,j) = (1-mix.k2(i,j))*(b(i) + b(j))/2;
   end
end

%Mixture parameters
am = 0;
bm = 0;
for i = 1:mix.numC
   for j = 1:mix.numC
      am = am + mix.x(i)*mix.x(j)*aij(i,j);
      bm = bm + mix.x(i)*mix.x(j)*bij(i,j);
   end
end
A = am*P/(R*T)^2;
B = bm*P/(R*T);

D = 0;
for i = 1:mix.numC
   for j = 1:mix.numC
      D = D + mix.x(i)*mix.x(j)*(0.37464 + 1.54226*mix.comp(j).w - 0.26992*mix.comp(j).w^2)*(1-mix.k1(i,j))*sqrt(a(i))*sqrt(a(j)/alpha(j)*Tr(j));
   end
end

%Residual enthalpy
Hres = R*T*(1-Z+(A/(2.828*B))*(1+D/am)*log((Z+2.414*B)/(Z-0.414*B)));