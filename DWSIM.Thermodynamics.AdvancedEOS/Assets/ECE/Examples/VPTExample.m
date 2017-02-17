%This example shows the application of the Valderrama-Patel-Teja equation of state
%for the thermodynamic modelling of carbon dioxide + hydrogen sulfide
%Reference for the parameters: Valderrama et al., Chem. Eng. Sci. 42 (1987)
%2935-2940

%For application of the cubic VPT-EoS, the following pure component
%parameters are required: critical temperature, critical pressure, acentric factor
%and critical compressibility coefficient

CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.Tc = 304.1;
CO2.Pc = 7.38e6;
CO2.w = 0.225;
CO2.EoSParam(1) = 0.309; %Zc

H2S = cSubstance;
H2S.name = 'Hydrogen sulfide';
H2S.Tc = 373.5;
H2S.Pc = 8.963e6;
H2S.w = 0.083;
H2S.EoSParam(1) = 0.284; %Zc

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = H2S;
mix.x = [0.5 0.5];

%VPT EoS uses three binary interaction coefficients: k1, k2 and k3, for the
%calculation of mixture parameters a, b and c respectively with quadratic
%mixing rules
mix.k1= [0 0.11;0.11 0];

EoS = cVPTEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,5e6,300,mix,[0 1],[1 0],0.5)