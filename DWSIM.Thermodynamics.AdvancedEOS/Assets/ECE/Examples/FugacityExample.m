%This example shows the calculation of the fugacity coefficient
%of a mixture of hexane and carbon dioxide using Peng-Robinson EOS without 
%interaction coefficients

%Definition of components
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01; %g/mol
CO2.Tc = 304.2; %K
CO2.Pc = 7.38e6; %Pa
CO2.w = 0.2252;

Hex = cSubstance;
Hex.name = 'Carbon Dioxide';
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

%The global composition of the mixture must be set in 'mix.x' or
%'mix.molFrac'
mix.x = [0.02 0.98];
 
%In the following example the fugacity coefficient of a liquid phase
%is calculated
f = fugF(EoS,313,10e6,mix,'liq')

%The following example shows the calculation of the coefficient of a gas phase
mix.x = [0.999 0.001];
f = fugF(EoS,313,1e6,mix,'gas')
