function [f Z EoS] = fugF(EoS,T,P,mix,phase,varargin)
%Calculates the fugacity and compressibility coefficient of mixture mix at temperature T
%and pressure P using PSRK EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (K)
%mix: cMixture object
%phase: set phase = 'liq' to calculate the fugacity of a liquid phase or
%   phase = 'gas' to calculate the fugacity of a gas phase
%
%Results:
%f: fugacity coefficient
%Z: compressibility coefficient
%EoS: returns EoS used for calculations
%
%Reference: Holderbaum and Gmehling, Fluid Phase Equilibr. 70 (1991) 251-265

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

%************************************************
%Calculates the compressibility coefficient
%************************************************
R = 8.31; %Ideal gas constant, J/mol K

%Avoids divisions by zero
for i = 1:mix.numC
    if mix.x(i) == 0
        mix.x(i) = 1e-10;
    end
end

%Reads pure component properties 
numC = mix.numC;
comp = mix.comp;
x = mix.x;
Tc = zeros(numC,1);
Pc = zeros(numC,1);
w = zeros(numC,1);
c1 = zeros(numC,1);
c2 = zeros(numC,1);
c3 = zeros(numC,1);
for i = 1:numC
    Tc(i) = comp(i).Tc;
    Pc(i) = comp(i).Pc;
    w(i) = comp(i).w;
    c1(i) = comp(i).EoSParam(1);
    c2(i) = comp(i).EoSParam(2);
    c3(i) = comp(i).EoSParam(3);
end

Tr = zeros(numC,1);
Pr = zeros(numC,1);
%Reduced variables
for i = 1:numC
   Tr(i) = T/Tc(i);
   Pr(i) = P/Pc(i);
end

%Pure component parameters
alfa = zeros(numC,1);
a = zeros(numC,1);
b = zeros(numC,1);
A = zeros(numC,1);
B = zeros(numC,1);
for i = 1:numC
    if Tr(i) < 1
        alfa(i) = (1 + c1(i)*(1-Tr(i)^0.5) + c2(i)*(1-Tr(i)^0.5)^2 + c3(i)*(1-Tr(i)^0.5)^3)^2; %Eq. 5 of reference
    else
        alfa(i) = (1 + c1(i)*(1-Tr(i)^0.5))^2; %Eq. 5 of reference
    end 
    
   a(i) = 0.42748*(R*Tc(i))^2/Pc(i)*alfa(i);
   b(i) = 0.08664*R*Tc(i)/Pc(i); %Eq. 8 of reference
   
   A(i) = a(i)*P/(R^2*T^2);
   B(i) = b(i)*P/(R*T);
end

%Mixing rules
bm = 0;
for i = 1: numC
    bm = bm + x(i)*b(i); %Eq. 7 of reference
end

sum_a1 = 0;
sum_a2 = 0;
for i = 1:numC
    sum_a1 = sum_a1 + x(i)*a(i)/b(i);
    sum_a2 = sum_a2 + x(i)*log(bm/b(i));
end

A1 = -0.64663;

[ge,gamma] = UNIFAC(EoS,mix,T);

am = bm*(ge/A1 + sum_a1 + R*T/A1*sum_a2); %Eq. 6 of reference

A = am*P/(R*T)^2;
B = bm*P/(R*T);


%Compressibility coefficient calculation, resolution of cubic equation
Z = roots([1 -1 (A-B-B^2) -(A*B)]);

%Removes complex and negative roots
ZR = [];
for i = 1:3
   if isreal(Z(i)) && Z(i) > 0
   	ZR = [ZR Z(i)];   
   end
end

%Selects the coefficient corresponding to liquid (smallest positive root) or gas
%(largest root) phases
if strcmp(phase,'liq') == 1
    Z = min(ZR);   
elseif strcmp(phase,'gas') == 1
    Z = max(ZR);
else
    error(['The value "' phase '" of "phase" parameter is incorrect']);
end

%************************************************
%Calculates the fugacity coefficient
%************************************************
f = zeros(numC,1);
for comp = 1:numC
    %Molar volume
    v_mol = Z*R*T/P;

    %Fugacity coefficient
    alfa_fi = a(comp)/(b(comp)*R*T); %Eq. A2 of reference
    alfa_par_fi = 1/A1*(log(gamma(comp)) + log(bm/b(comp)) + b(comp)/bm - 1) + alfa_fi; %Eq. A3 of reference

    log_fi = b(comp)/bm * (P*v_mol/(R*T)-1) - log(P*(v_mol-bm)/(R*T)) - alfa_par_fi*log((v_mol+bm)/v_mol); %Eq. A1 of reference

    f(comp) = exp(log_fi);
end