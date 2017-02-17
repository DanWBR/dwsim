%This example shows the application of the PC-SAFT equation of state
%for the thermodynamic modelling of carbon dioxide + water mixtures
%Reference for the parameters: Á. Martín et al, Fluid Phase Equilibr. 286
%(2009) 162-169

%For application of the PCSAFT-EoS, the following pure component
%parameters are required: molecular weight, number of segments per chain,
%segment diameter (Angstrom), depth of pair potential (K), Number of
%association sites (only for associating molecules), Effective association
%volume(only for associating molecules), association energy (K) (only for
%associating molecules)

CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01;
CO2.EoSParam(1) = 2.0729; %m
CO2.EoSParam(2) = 2.7852; %sigma
CO2.EoSParam(3) = 169.21; %epsilon/k

H2O = cSubstance;
H2O.name = 'Water';
H2O.MW = 18.015;
H2O.EoSParam(1) = 1.09528; %m
H2O.EoSParam(2) = 2.88980; %sigma
H2O.EoSParam(3) = 365.956; %epsilon/k
H2O.EoSParam(4) = 2; %NumAss
H2O.EoSParam(5) = [0 0.03487;0.03487 0]; %Association parameters (2B scheme)
H2O.EoSParam(6) = [0 2515.7;2515.7 0]; %Association parameters (2B scheme)

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = H2O;
mix.x = [0.5 0.5];

%Pr-EoS uses conventional quadratic mixing rules, requiring one interaction
%parameter k, which is used to calculate the cohesive energy of the
%mixture. This parameter can be temperature-dependant
mix.k(1,2) = '-28.782/T + 0.0731';
mix.k(2,1) = '-28.782/T + 0.0731';

EoS = cPCSAFTEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)