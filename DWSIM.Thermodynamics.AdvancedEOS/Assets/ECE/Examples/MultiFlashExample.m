%This example shows the calculation of a 3-phase flash of a mixture
%of water, hexane and carbon dioxide using Peng-Robinson EOS 
%IMPORTANT NOTICE: For illustration purposes, a binary interaction coefficient 
%between water and hexane has been set in order to obtain immiscibility
%between these components in calculations. However, since it has not been
%tested that the value of this coefficient is correct, calculations are
%likely to be innacurate for practical applications

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

H2O = cSubstance;
H2O.name = 'Water';
H2O.MW = 18.015; %g/mol
H2O.Tc = 647.3; %K
H2O.Pc = 22.048e6; %Pa
H2O.w = 0.3442;

%Definition of mixture
mix = cMixture;
mix.comp(1) = H2O;
mix.comp(2) = Hex;
mix.comp(3) = CO2;
mix.x = [0.4 0.4 0.2];

mix.k(1,2) = -0.3; %Interaction coefficient between water and hexane, set to obtain liquid-liquid immiscibility
mix.k(2,1) = -0.3; %(not representative of the real value of the interaction coefficient between these components)

%Specification of EoS
EoS = cPREoS;

%To run the calculation, it is necessary to provide estimations of the
%phases that may be present, as well as the composition of each phase and its
%phase fraction 
%In this case, it is estimated that two immiscible liquids and a gas may be
%present. This is specified in the last input parameter, {'gas', 'liq',
%'liq'}
%The estimated composition of each phase is provided in the 5th parameter.
%This parameter consists in a matrix, in which each row corresponds to the
%composition of one phase, in the same order as the previous parameter.
%In this case, it is estimated that the gas phase is pure CO2, the first
%liquid phase pure hexane, and the second liquid phase pure water
%Finally, as estimations of the phase fractions values of 0.2 for the gas
%phase and 0.4 for each liquid phase are provided
[beta, thita, x, K, liquid, val, time, EoS] = MultiFlash(EoS,1e5,313,mix,[0 0 1;0 1 0;1 0 0],[0.2 0.4 0.4],{'gas', 'liq', 'liq'})

%As result, the composition of each phase is obtained in the 'x' output
%parameter. This parameter is a matrix, and each row corresponds to the
%composition of one phase.
%The first output parameter, 'beta', represents the calculated phase
%fraction. In this case, non-zero values of beta are obtained, meaning that
%the three phases are present in the equilibrium
%If calculations indicate that one or several of the phases are not
%present, beta=0 will be obtained for these phases. Moreover, a non-zero
%value of the output parameter thita will be obtained.
%Thita is a measurement of the proximity of a certain phase to stability. 
%In the following examples, temperature is increased, and with
%the specified parameters, the volatility of hexane gradually increases,
%until the organic liquid phase dissapears. Further increments of
%temperature cause an increase in the value of thita
[beta, thita, x, K, liquid, val, time, EoS] = MultiFlash(EoS,1e5,323,mix,[0 0 1;0 1 0;1 0 0],[0.2 0.4 0.4],{'gas', 'liq', 'liq'})
[beta, thita, x, K, liquid, val, time, EoS] = MultiFlash(EoS,1e5,333,mix,[0 0 1;0 1 0;1 0 0],[0.2 0.4 0.4],{'gas', 'liq', 'liq'})
[beta, thita, x, K, liquid, val, time, EoS] = MultiFlash(EoS,1e5,343,mix,[0 0 1;0 1 0;1 0 0],[0.2 0.4 0.4],{'gas', 'liq', 'liq'})