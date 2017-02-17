%This example shows the calculation of an isothermal flash of a mixture
%of hexane and carbon dioxide using Peng-Robinson EOS without interaction
%coefficients

%Definition of components
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01; %g/mol
CO2.Tc = 304.2; %K
CO2.Pc = 7.38e6; %Pa
CO2.w = 0.2252;

Hex = cSubstance;
Hex.name = 'Hexane';
Hex.MW = 86.18; %g/mol
Hex.Tc = 507.4; %K
Hex.Pc = 3.014e6; %Pa
Hex.w = 0.2975;

%Definition of mixture
mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = Hex;

%Specification of EoS
EoS = cPREoS;

%In order to calculate a flash point, the  global composition of the
%mixture must be set in 'mix.x' or 'mix.molFrac'
mix.x = [0.5 0.5];

%In order to perform flash calculations, inital estimations of the
%composition of the gas phase, the composition of the liquid phase, and the
%vaporized fraction (moles in the gas phase/total number of moles) must be
%provided
[beta, x, y, K, val, time] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)