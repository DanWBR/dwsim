%This example shows the calculation of a P-xy diagram of a mixture
%of hexane and carbon dioxide using Peng-Robinson EOS without interaction
%coefficients

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

%Definition of mixture
mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = Hex;

%Specification of EoS
EoS = cPREoS;

%In order to calculate the Pxy diagram, an estimation of the equilibrium
%temperature and gas composition of the first point of the diagram must be provided
%By default, this point corresponds to pure component 2
[P,x,y,handle,EoS] = PxyDiagram(EoS,mix,313,1e4,[1 0]);

%As additional parameters, the range liquid molar fractions covered by the
%diagram as well as the number of points calculated can be specified. By
%default, these values are [0 1] (ranging from pure component 2 to pure
%component 1) and 20 points. Furthermore, by setting a 1 in the third
%optional input parameter, 'diagnostics', information will be written in
%the command screen after the calculation of each point in the diagram
[P,x,y,handle,EoS] = PxyDiagram(EoS,mix,313,1e4,[1 0],50,[0 0.94],1);