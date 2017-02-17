function [f Z EoS] = fugF(EoS,T,P,mix,phase,varargin)
%Calculates the fugacity and compressibility coefficient of mixture mix at temperature T
%and pressure P using PHSC EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (K)
%mix: cMixture object
%phase: set phase = 'liq' to calculate the fugacity of a liquid phase or
%   phase = 'gas' to calculate the fugacity of a gas phase
%
%Optional parameters (set [] to keep default value)
%Z_ini: Initial guess for the compressibility coefficient
%   If not defined, the program uses an initial guess Z_ini = 0.8 for gas
%   phase and a Z_ini corresponding to a liquid density of 800 kg/m3 for
%   the liquid phase
%options: parameters of the fzero numerical resolution method (structure
%   generated with "optimset")
%
%Reference: Favari et al., Chem. Eng. Sci. 55 (2000) 2379-2392
%
%Results:
%f: fugacity coefficient
%Z: compressibility coefficient
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

%************************************************
%Calculates the compressibility coefficient
%************************************************
Z = compr(EoS,T,P,mix,phase,varargin{:});

%************************************************
%Calculates the fugacity coefficient
%************************************************

%Constants
a1 = 1.8681;	
a2 = 0.0619;
a3 = 0.6715;
a4 = 1.7317;
b1 = 0.7303;
b2 = 0.1649;
b3 = 0.2697;
b4 = 2.3973;
kb = 1.38048e-23; %Boltzmann K (J/K)
R = 8.31; %Gas K (J/K)
Na = 6.022e23; %Avogadro No

%Number density
dens = P/(Z*kb*T);

%Calculates sigma and epsilon for each component from V, A and E
numC = mix.numC;
sigma = zeros(numC,1);
r = zeros(numC,1);
epsilon = zeros(numC,1);
for i = 1:numC
    V = mix.comp(i).EoSParam(1);
    A = mix.comp(i).EoSParam(2);
    E = mix.comp(i).EoSParam(3);
    
    sigma(i) = 6*V/A;
    r(i) = A/(pi*sigma(i)^2*Na);
    epsilon(i) = E/(r(i)*R);
end
k = mix.k1;
x = mix.x;

%Mixing rules for sigma and epsilon
sigmaij = zeros(numC,numC);
epsilonij = zeros(numC,numC);
for i =1:numC
    for j = 1:numC
        sigmaij(i,j) = 0.5*(sigma(i) + sigma(j)); %Eq. 17 of reference
        epsilonij(i,j) = sqrt(epsilon(i)*epsilon(j)) * (1 - k(i,j)); %Eq. 18 of reference          
    end
end

%Pure component parameters a and b
a = zeros(numC,1);
b = zeros(numC,1);
for i = 1:numC
    Fa = a1*exp(-a2*(T/epsilon(i)))+a3*exp(-a4*(T/epsilon(i))^1.5); %Eq. 4 of reference
    Fb = b1*exp(-b2*(T/epsilon(i))^0.5)+b3*exp(-b4*(T/epsilon(i))^1.5); %Eq. 5 of reference
    
    a(i) = (2*pi/3)*sigma(i)^3*epsilon(i)*kb*Fa; %Eq. 2 of reference
    b(i) = (2*pi/3)*sigma(i)^3*Fb; %Eq. 3 of reference          
end

%Mixture parameters aij and bij
aij = zeros(numC,numC);
bij = zeros(numC,numC);
for i = 1:numC
    for j = 1:numC
        Fa = a1*exp(-a2*(T/epsilonij(i,j)))+a3*exp(-a4*(T/epsilonij(i,j))^1.5); %Eq. 4 of reference
        Fb = b1*exp(-b2*(T/epsilonij(i,j))^0.5)+b3*exp(-b4*(T/epsilonij(i,j))^1.5); %Eq. 5 of reference
        
        aij(i,j) = (2*pi/3)*sigmaij(i,j)^3*epsilonij(i,j)*kb*Fa; %Eq. 15 of reference
        bij(i,j) = (2*pi/3)*sigmaij(i,j)^3*Fb; %Eq. 16 of reference
    end
end

%Packing fraction
nu = 0;
for i = 1:numC
    nu = nu + x(i)*r(i)*b(i);    
end
nu = nu*dens/4; %Eq. 20 of reference

%Function g(d+)
aus_ix = 0;
for i = 1:numC
    aus_ix = aus_ix + x(i)*r(i)*b(i)^(2/3);    
end
aus_ix = aus_ix*dens/4;

ix = zeros(numC,numC);
for i = 1:numC
    for j = 1:numC
        ix(i,j) = (b(i)*b(j)/bij(i,j))^(1/3)*aus_ix; %Eq. 21 of reference         
    end
end

g = zeros(numC,numC);
for i = 1:numC
    for j = 1:numC
        g(i,j) = 1/(1 - nu) + 3/2 * ix(i,j)/(1 - nu)^2 + 1/2 * ix(i,j)^2/(1 - nu)^3; %Eq. 19 of reference           
    end
end

%Segment density
ro_r = 0;
for i = 1:numC
    ro_r = ro_r + dens*x(i)*r(i); 
end

%Segment fraction
fi = zeros(numC,1);
sum = 0;
for i = 1:numC
    sum = sum + x(i)*r(i);    
end
for i = 1:numC
    fi(i) = x(i)*r(i)/sum;    
end

%Functions Q and W
I1 = -log(1-nu);
I2 = -I1 + nu/(1-nu);
I3 = -I2 + 1/2*nu^2/(1-nu)^2; %Eq. 2.24 of reference 2

W = zeros(numC,numC);
Q = zeros(numC,numC);
for i = 1:numC
    for j = 1:numC
        W(i,j) = I1/nu + 3/2*ix(i,j)/nu^2*I2 + 1/2*ix(i,j)^2/nu^3*I3; %Eq. 2.20 of reference 2
        Q(i,j) = -log(1-nu) + 3/2*ix(i,j)/(1-nu) + 1/4*ix(i,j)^2/(1-nu)^2; %Eq. 2.21 of reference 2
    end
end

%Fugacity coefficient
f = zeros(numC,1);
for comp = 1:numC
    term1 = 0;
    for i = 1:numC
        term1 = term1 + x(i)*r(i)*bij(i,comp)*W(i,comp);
    end
    term1 = term1*2*r(comp)*dens; %Eq. 2.26 of reference 2 (first addend)
    
    term2 = 0;
    for i = 1:numC
        for j = 1:numC
            dWdnu = 1/(nu*(1-nu)) + log(1-nu)/nu^2 + 3/2*ix(i,j)*(-2*log(1-nu)/nu^3+(3*nu-2)/(nu^2*(1-nu)^2))+1/2*ix(i,j)^2*(3*log(1-nu)/nu^4 + (4*nu^2-6*nu+3)/(nu^3*(1-nu)^3) - 3/2*1/(nu^2*(1-nu)^2));
            dWdix = 3/2*1/nu^2*(log(1-nu) + nu/(1-nu)) + ix(i,j)/nu^3*(-log(1-nu) - nu/(1-nu) + 1/2*nu^2/(1-nu)^2);
            NdnudN = dens/4*r(comp)*bij(comp,comp); %Eq. 2.29 of reference 2 
            NdixdN = dens/4*(bij(i,i)*bij(j,j)/bij(i,j))^(1/3)*r(comp)*bij(comp,comp)^(2/3); %Eq. 2.30 of reference 2 
            
            NdWdN = dWdnu*NdnudN + dWdix*NdixdN; %Eq. 2.27 of reference 2 
            
            term2 = term2 + x(i)*x(j)*r(i)*r(j)*bij(i,j)*NdWdN; 
        end
    end
    term2 = term2*dens; %Eq. 2.26 of reference 2 (second addend)
    
    term3 = 0;
    for i = 1:numC
        dQdnu = 1/(1-nu) + 3/2*ix(i,i)/(1-nu)^2 + 1/2*ix(i,i)^2/(1-nu)^3;
        dQdix = 3/2*1/(1-nu) + 1/2*ix(i,i)/(1-nu)^2;
        NdnudN = dens/4*r(comp)*bij(comp,comp); %Eq. 2.29 of reference 2 
        NdixdN = dens/4*(bij(i,i)*bij(i,i)/bij(i,i))^(1/3)*r(comp)*bij(comp,comp)^(2/3); %Eq. 2.30 of reference 2 
        
        NdQdN = dQdnu*NdnudN + dQdix*NdixdN; %Eq. 2.28 of reference 2 
        
        term3 = term3 + x(i)*(r(i)-1)*NdQdN;
    end
    term3 = term3*-1; %Eq. 2.26 of reference 2 (fourth addend)
    
    term4 = -(r(comp)-1)*Q(comp,comp); %Eq. 2.26 of reference 2 (third addend)
    
    term5 = 0;
    for i = 1:numC
        term5 = term5 + x(i)*r(i)*aij(i,comp);
    end
    term5 = term5*-2*r(comp)*dens/(kb*T); %Eq. 2.26 of reference 2 (fifth addend)
    
    f(comp) = exp(term1+term2+term3+term4+term5-log(Z));
end