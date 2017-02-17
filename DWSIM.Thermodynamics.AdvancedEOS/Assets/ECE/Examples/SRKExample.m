%This example shows the application of the Soave-Redlich-Kwong equation of state
%for the thermodynamic modelling of carbon dioxide + iso-butanol mixtures
%Reference for the parameters: Vázquez da Silva and Barbosa, Fluid Phase
%Equilib. 198 (2002) 229-237

%For application of the cubic SRK-EoS, the following pure component
%parameters are required: critical temperature, critical pressure and
%acentric factor
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.Tc = 304.1; 
CO2.Pc = 7.38e6;
CO2.w = 0.225;

iBut = cSubstance;
iBut.name = 'iso-Butanol';
iBut.Tc = 547.7; 
iBut.Pc = 4.3e6;
iBut.w = 0.588;

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = iBut;
mix.x = [0.5 0.5];

%SRK-EoS uses conventional quadratic mixing rules, requiring two interaction
%parameters 'k1' and 'k2' for mixture parameters 'a' and 'b', respectively
%Both parameters can be temperature-dependant
mix.k1 = [0 0.1091;0.1091 0];
mix.k2 = [0 -0.0033;-0.0033 0];

EoS = cSRKEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)