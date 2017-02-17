function [muHC EoS] = mu_HC(EoS,T,dens_num,mix)
%Calculates the hard chain contribution to the residual chemical potential 
%of mixture mix at temperature T and pressure P using PC-SAFT EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (K)
%dens_num: Number density (molecule/Angstrom^3)
%mix: cMixture object
%
%Results:
%muass: residual chemical potential, association contribution
%EoS: returns EoS used for calculations
%
%Reference: Gross and Sadowski, Ind. Eng. Chem. Res. 40 (2001) 1244-1260

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

%Reads pure-component properties
m = zeros(1,mix.numC);
sigma = zeros(1,mix.numC);
epsilon = zeros(1,mix.numC);
for i = 1:mix.numC
    m(i)= mix.comp(i).EoSParam(1);
    sigma(i) = mix.comp(i).EoSParam(2);
    epsilon(i) = mix.comp(i).EoSParam(3);
end

%Calculates the temperature-dependant segment diameter
d = zeros(1,mix.numC);
for i = 1:mix.numC
    d(i) = HardSphereDiameter(EoS,T,m(i),sigma(i),epsilon(i));
end

%mean segment number
m_prom = 0;
for i = 1:mix.numC
    m_prom = m_prom + m(i)*mix.x(i); %Eq. 6 of reference
end

%auxiliary functions
auxil = zeros(1,4);
for j = 1:4
    for i = 1:mix.numC
        auxil(j) = auxil(j) + mix.x(i)*m(i)*d(i)^(j-1);
    end
    auxil(j) = auxil(j)*pi/6*dens_num; %Eq. 9 of reference
end

%radial distribution function
ghs = zeros(mix.numC, mix.numC);
for i = 1:mix.numC
    for j = 1:mix.numC
        term1 = 1/(1-auxil(4));
        term2 = d(i)*d(j)/(d(i)+d(j))*3*auxil(3)/(1-auxil(4))^2;
        term3 = (d(i)*d(j)/(d(i)+d(j)))^2*2*auxil(3)^2/(1-auxil(4))^3;
        ghs(i,j)= term1 + term2 + term3; %Eq. 8 of reference
    end
end

%Compressibility coefficient
Zhc = Z_hc(EoS,T,dens_num,mix);

%Helmholtz energy
term1 = 3*auxil(2)*auxil(3)/(1-auxil(4));
term2 = auxil(3)^3/(auxil(4)*(1-auxil(4))^2);
term3 = (auxil(3)^3/auxil(4)^2-auxil(1))*log(1-auxil(4));
a_hs = (1/auxil(1))*(term1 + term2 + term3); %Eq. A6 of reference
sum1 = 0;
for i = 1:mix.numC
    sum1 = sum1 + mix.x(i)*(m(i)-1)*log(ghs(i,i));
end
Ahc = m_prom*a_hs - sum1; %Eq. A4 of reference

%Chemical potential
dauxil_dxk= zeros(4,mix.numC);
for j = 1:4
    for i = 1:mix.numC
        dauxil_dxk(j,i)=pi/6*dens_num*m(i)*d(i)^(j-1); %Eq. A34 of reference
    end
end

dahs_dxk = zeros(1,mix.numC);
for i = 1:mix.numC
    term1 = -dauxil_dxk(1,i)/auxil(1)*a_hs;
    term2 = 3*(dauxil_dxk(2,i)*auxil(3) + auxil(2)*dauxil_dxk(3,i))/(1-auxil(4));
    term3 = 3*auxil(2)*auxil(3)*dauxil_dxk(4,i)/(1-auxil(4))^2;
    term4 = 3*auxil(3)^2*dauxil_dxk(3,i)/(auxil(4)*(1-auxil(4))^2);
    term5 = auxil(3)^3*dauxil_dxk(4,i)*(3*auxil(4)-1)/(auxil(4)^2*(1-auxil(4))^3);
    term6 = ((3*auxil(3)^2*dauxil_dxk(3,i)*auxil(4)-2*auxil(3)^3*dauxil_dxk(4,i))/auxil(4)^3-dauxil_dxk(1,i))*log(1-auxil(4));
    term7 = (auxil(1)-auxil(3)^3/auxil(4)^2)*dauxil_dxk(4,i)/(1-auxil(4));
    
    dahs_dxk(i) = term1 + 1/auxil(1)*(term2 + term3 + term4 + term5 + term6 +term7); %Eq. A36 of reference
end

dgij_dxk = zeros(mix.numC, mix.numC, mix.numC);
for i = 1:mix.numC
    for j = 1:mix.numC
        for k = 1:mix.numC
            term1 = dauxil_dxk(4,k)/(1-auxil(4))^2;
            term2 = (d(i)*d(j)/(d(i)+d(j)))*(3*dauxil_dxk(3,k)/(1-auxil(4))^2 + 6*auxil(3)*dauxil_dxk(4,k)/(1-auxil(4))^3);
            term3 = (d(i)*d(j)/(d(i)+d(j)))^2*(4*auxil(3)*dauxil_dxk(3,k)/(1-auxil(4))^3 + 6*auxil(3)^2*dauxil_dxk(4,k)/(1-auxil(4))^4);
            dgij_dxk(i,j,k) = term1 + term2 + term3; %Eq. A37 of reference
        end
    end
end

dahc_dxk = zeros(1,mix.numC);
for i = 1:mix.numC
    sum1 = 0;
    for j = 1:mix.numC
        sum1 = sum1 + mix.x(j)*(m(j)-1)/ghs(j,j)*dgij_dxk(j,j,i);
    end
    dahc_dxk(i) = m(i)*a_hs + m_prom*dahs_dxk(i) - sum1 + (1-m(i))*log(ghs(i,i)); %Eq. A35 of reference
end

%Chemical potential
sum1 = 0;
for i = 1:mix.numC
    sum1 = sum1 + mix.x(i)*dahc_dxk(i);
end

muHC = zeros(1,mix.numC);
for i = 1:mix.numC
    muHC(i) = Ahc + Zhc + dahc_dxk(i) - sum1 ; %Eq. A33 of reference
end
