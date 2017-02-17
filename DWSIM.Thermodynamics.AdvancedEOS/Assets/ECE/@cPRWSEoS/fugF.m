function [f Z] = fugF(EoS,T,P,mix,phase)
%Calculates the fugacity and compressibility coefficient of mixture mix at temperature T
%and pressure P using Peng-Robinson Wong-Sandler EoS
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
%fug: fugacity coefficient
%EoS: returns EoS used for calculations
%compr: compressibility coefficient 

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

%Reads pure component properties and interaction coefficients
numC = mix.numC;
comp = mix.comp;
x = mix.x;
Tc = zeros(numC,1);
Pc = zeros(numC,1);
w = zeros(numC,1);
for i = 1:numC
    Tc(i) = comp(i).Tc;
    Pc(i) = comp(i).Pc;
    w(i) = comp(i).w;
end
k1 = mix.k1;
k2 = mix.k2;
k3 = mix.k3;

%Pure component parameters
alpha = zeros(numC,1);
a = zeros(numC,1);
b = zeros(numC,1);
for i = 1:numC
   alpha(i) = alpha_function(EoS,comp(i),T); %Evaluates the alpha function (may differ in modifications of PR-EoS)
   a(i) = 0.45724*(R*Tc(i))^2/Pc(i)*alpha(i);
   b(i) = 0.0778*R*Tc(i)/Pc(i);
end

%Activity coefficient model
A_ex = 0;
g = zeros(numC,numC);
for i = 1:numC
    for j = 1:numC
        g(i,j) = exp(-k2(i,j)*k3(i,j));        
    end
end
for i = 1:numC
    Num_A_ex = 0;
    Den_A_ex = 0;
    for j = 1:numC
        Num_A_ex = Num_A_ex + x(j)*k3(j,i)*g(j,i);
    end
    for k = 1:numC
        Den_A_ex = Den_A_ex + x(k)*g(k,i);    
    end
    A_ex = A_ex + R*T*x(i)*Num_A_ex/Den_A_ex;
end

%Mixing rules
C = log(sqrt(2) - 1)/sqrt(2);
D = A_ex/(C*R*T);
Q = 0;
b_mix = zeros(numC, numC);
for i = 1:numC
   D = D + x(i)*a(i)/(b(i)*R*T);
   for j = 1:numC
        b_mix(i,j) = ((b(i) - a(i)/(R*T)) + (b(j) - a(j)/(R*T)))/2*(1-k1(i,j));
        Q = Q + x(i)*x(j)*b_mix(i,j);
   end
end

%Mixture parameters
am = R*T*Q*D/(1-D);
bm = Q/(1-D);
A = am*P/(R*T)^2;
B = bm*P/(R*T);

%Compressibility coefficient calculation, resolution of cubic equation
Z = roots([1 -(1-B) (A-3*B^2-2*B) -(A*B-B^2-B^3)]);

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
%Molar volume
v = Z*R*T/P;

f = zeros(numC,1);
for comp = 1:numC
    %Activity coefficient
    Num_A_ex = 0;
    Den_A_ex = 0;
    for j = 1:numC
        Num_A_ex = Num_A_ex + x(j)*k3(j,comp)*g(j,comp);
    end
    for k = 1:numC
        Den_A_ex = Den_A_ex + x(k)*g(k,comp);    
    end
    term_1 = Num_A_ex/Den_A_ex;

    term_2 = 0;
    for j = 1:numC
        Num_A_ex = 0;
        for l = 1:numC
           Num_A_ex = Num_A_ex + x(l)*k3(l,j)*g(l,j);
        end
        Den_A_ex = 0;
        for k = 1:numC
            Den_A_ex = Den_A_ex + x(k)*g(k,j);  
        end
        term_2 = term_2 + x(j)*g(comp,j)/Den_A_ex*(k3(comp,j) - Num_A_ex/Den_A_ex);
    end
    log_gamma = term_1 + term_2;
    
    %Auxiliary derivatives
    der2Q_dn2 = 0;
    for j = 1:numC
        der2Q_dn2 = der2Q_dn2 + 2*x(j)*b_mix(comp,j);
    end

    derD_dn = a(comp)/(b(comp)*R*T) + log_gamma/C;

    derb_dn = 1/(1-D)*der2Q_dn2 - Q/(1-D)^2*(1-derD_dn);

    der2a_dn2 = R*T*(D*derb_dn + bm*derD_dn);

    %Fugacity coefficient
    term1 = -log(P*(v-bm)/(R*T));
    term2 = 1/bm*derb_dn*(Z-1);
    term3 = 1/(2*sqrt(2))*am/(bm*R*T);
    term4 = (1/am*der2a_dn2 - 1/bm*derb_dn);
    term5 = log((v+bm*(1-sqrt(2)))/(v+bm*(1+sqrt(2))));

    f(comp) = exp(term1 + term2 + term3*term4*term5);
end
