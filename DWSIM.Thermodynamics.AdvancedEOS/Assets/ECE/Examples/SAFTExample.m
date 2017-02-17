%This example shows the application of the SAFT equation of state
%for the thermodynamic modelling of carbon dioxide + ethanol mixtures
%Reference for the parameters: Huang and Radosz, Ind. Eng. Chem. Res. 30
%(1991) 1994-2005

%For application of the SAFT-EoS, the following pure component
%parameters are required: molecular weight, number of segments per chain,
%segment diameter (Angstrom), depth of pair potential (K), Number of
%association sites (only for associating molecules), Effective association
%volume(only for associating molecules), association energy (K) (only for
%associating molecules)

CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01;
CO2.EoSParam(1) = 1.417; %m
CO2.EoSParam(2) = 3.172; %sigma
CO2.EoSParam(3) = 216.08; %epsilon/k

EtOH = cSubstance;
EtOH.name = 'Ethanol';
EtOH.MW = 46.07;
EtOH.EoSParam(1) = 2.457; %m
EtOH.EoSParam(2) = 3.044; %sigma
EtOH.EoSParam(3) = 213.48; %epsilon/k
EtOH.EoSParam(4) = 2; %Number of association sites
EtOH.EoSParam(5) = [0 0.0292;0.0292 0]; %Association parameters (2B scheme)
EtOH.EoSParam(6) = [0 2619;2619 0]; %Association parameters (2B scheme)

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = EtOH;
mix.x = [0.5 0.5];

%Pr-EoS uses conventional quadratic mixing rules, requiring one interaction
%parameter k, which is used to calculate the cohesive energy of the
%mixture. This parameter can be temperature-dependant
mix.k = [0 0.0602;0.0602 0];

EoS = cSAFTEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0.1 0.9],[0.99 0.01],0.5)