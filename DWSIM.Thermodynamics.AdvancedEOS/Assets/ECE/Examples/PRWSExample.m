%This example shows the application of the Peng-Robinson equation of state
%with Wong-Sandler mixing rules for the thermodynamic modelling of water+
%carbon dioxide mixtures
%Reference for the parameters: Á. Martín et al, J. Supercrit. Fluids 41 
%(2007) 126

%For application of the cubic PRWS-EoS, the following pure component
%parameters are required: critical temperature, critical pressure and
%acentric factor
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01;
CO2.Tc = 304.2;
CO2.Pc = 7.38e6;
CO2.w = 0.225;

H2O = cSubstance;
H2O.name = 'Water';
H2O.MW = 18.015;
H2O.Tc = 647.3;
H2O.Pc = 22.048e6;
H2O.w = 0.3442;

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = H2O;
mix.x = [0.5 0.5];

%PR with WS mixing rules requires three binary interaction parameters:
%the binary interaction parameter k, and NRTL parameters
%tau (interaction energy parameter) and alpha (non-randomness parameter)
%All of them can be temperature-dependant
mix.k1 = [0 0.3073;0.3073 0]; %Binary interaction parameter k
mix.k2 = [0 0.1141;0.1141 0]; %NRTL parameter alpha
mix.k3 = [0 4.3870;0.3930 0]; %NRTL paramter tau

EoS = cPRWSEoS; 
[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)