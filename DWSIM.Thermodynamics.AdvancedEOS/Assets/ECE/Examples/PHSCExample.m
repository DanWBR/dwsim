%This example shows the application of the PHSC equation of state
%for the thermodynamic modelling of carbon dioxide + ethyl acetate mixtures
%Reference for the parameters: Á. Martín et al, Chem. Eng. Proc. 47 (2008)
%1594-1602

%For application of the PHSC-EoS, the following pure component
%parameters are required: molecular weight, characteristic volume V,
%characteristic surface area A and characteristic cohesive energy E
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01; 
CO2.EoSParam(1) = 1.49e-5; %V (m3/mol) 
CO2.EoSParam(2) = 401270; %A (m2/mol) 
CO2.EoSParam(3) = 4323.4; %E (J/mol) 

EtAc = cSubstance;
EtAc.name = 'Ethyl Acetate';
EtAc.MW = 88.1; 
EtAc.EoSParam(1) = 4.497e-5; %V (m3/mol) 
EtAc.EoSParam(2) = 892791; %A (m2/mol) 
EtAc.EoSParam(3) = 8442.2; %E (J/mol) 

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = EtAc;
mix.x = [0.5 0.5];

%PHSC-EoS uses conventional quadratic mixing rules, requiring one interaction
%parameter k, which is used to calculate the cohesive energy of the
%mixture. This parameter can be temperature-dependant
mix.k= [0 -0.0677;-0.0677 0];

EoS = cPHSCEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0.1 0.9],[0.999 0.001],0.5)