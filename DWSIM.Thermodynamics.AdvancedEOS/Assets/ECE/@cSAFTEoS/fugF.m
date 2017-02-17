function [f Z EoS] = fugF(EoS,T,P,mix,phase,varargin)
%Calculates the fugacity and compresibility coefficient of mixture mix at temperature T
%and pressure P using SAFT EoS
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
%options: parameters of the fsolve numerical resolution method (structure
%   generated with "optimset")
%
%Results:
%f: fugacity coefficient
%Z: compresibility coefficient
%EoS: returns EoS used for calculations
%
%Reference: Chapman et al., Ind. Eng. Chem. Res. 29 (1990) 1709-1721

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

%**************************************************************************
%Calculates the compresibility coefficient
%**************************************************************************
%Constants

kb = 1.3806504e-23; %Boltzmann K (J/K)

[Z EoS] = compr(EoS,T,P,mix,phase,varargin{:});
dens_num = P/(Z*kb*T)*1/(1e10)^3;

%**************************************************************************
%Calculates the contributions to the chemical potential
%**************************************************************************

%Chain contribution
muChain = mu_Chain(EoS,T,dens_num,mix);

%Segment contribution
muSeg = mu_Seg(EoS,T,dens_num,mix);

%Association contribution
NumAss = zeros(1,mix.numC);

for i = 1:mix.numC
    NumAss(i) = mix.comp(i).EoSParam(4);
end

if sum(NumAss) > 0
    muAss = mu_Ass(EoS,T,dens_num,mix);
else
    muAss = zeros(mix.numC,1);
end

%**************************************************************************
%Calculates the fugacity coefficient
%**************************************************************************
f = zeros(1,mix.numC);
for i = 1:mix.numC
    logfi = muChain(i) + muSeg(i) + muAss(i) - log(Z); %Eq. A28 of reference
    f(i) = exp(logfi);
end