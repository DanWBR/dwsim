%This example shows the application of the Patel-Teja equation of state
%for the thermodynamic modelling of water + ammonia mixtures
%Reference for the parameters: Mejberi and Bellagi, Int. J. Refrig. 29
%(2006) 211-218

%For application of the cubic PT-EoS, the following pure component
%parameters are required: critical temperature, critical pressure, critical 
%compressibility coefficient and alpha factor F

H2O = cSubstance;
H2O.name = 'Water';
H2O.Tc = 647.1;
H2O.Pc = 22.064e6;
H2O.EoSParam(1) = 0.26959; %Zc
H2O.EoSParam(2) = 0.69365; %F

NH3 = cSubstance;
NH3.name = 'Ammonia';
NH3.Tc = 405.4;
NH3.Pc = 11.339e6;
NH3.EoSParam(1) = 0.28289; %Zc
NH3.EoSParam(2) = 0.6446; %F

mix = cMixture;
mix.comp(1) = H2O;
mix.comp(2) = NH3;
mix.x = [0.5 0.5];

%PT EoS uses three binary interaction coefficients: k1, k2 and k3, for the
%calculation of mixture parameters a, b and c respectively with quadratic
%mixing rules
mix.k1= [0 -0.25;-0.25 0];

EoS = cPTEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,10e6,523,mix,[0 1],[1 0],0.5)