%This example shows the application of the Peng-Robinson equation of state
%for the thermodynamic modelling of carbon dioxide + ethyl acetate mixtures
%Reference for the parameters: Á. Martín et al, Ind. Eng. Chem. Res 46
%(2007) 1552 - 1562

%For application of the cubic PR-EoS, the following pure component
%parameters are required: critical temperature, critical pressure and
%acentric factor
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.Tc = 304.1; 
CO2.Pc = 7.38e6;
CO2.w = 0.225;

EtAc = cSubstance;
EtAc.name = 'Ethyl Acetate';
EtAc.Tc = 523.2; 
EtAc.Pc = 3.83e6;
EtAc.w = 0.365;

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = EtAc;
mix.x = [0.5 0.5];

%Pr-EoS uses conventional quadratic mixing rules, requiring two interaction
%parameters 'k1' and 'k2' for mixture parameters 'a' and 'b', respectively
%Both parameters can be temperature-dependant
mix.k1(1,2) = '0.000325*T^2 - 0.203*T + 31.61';
mix.k1(2,1) = '0.000325*T^2 - 0.203*T + 31.61';

mix.k2(1,2) = '-839.41/T + 2.3633';
mix.k2(2,1) = '-839.41/T + 2.3633';

EoS = cPREoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)