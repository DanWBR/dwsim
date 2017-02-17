%This example shows the calculation of the residual enthalpy of
%carbon dioxide using Peng-Robinson EOS 

%Definition of component
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01; %g/mol
CO2.Tc = 304.2; %K
CO2.Pc = 7.38e6; %Pa
CO2.w = 0.2252;

%Definition of mixture
mix = cMixture;
mix.comp = CO2;

%Specification of EoS
EoS = cPREoS;

%The following example shows the calculation of the residual enthalpy
%(J/mol) at a given T,P. To calculate the total enthalpy, the ideal gas
%contribution should be added according to DH = DHideal - DHresidual
[Hres,EoS] = Enthalpy(EoS,mix,313,10,'gas')

%The following example shows the calculation of the vaporization enthalpy
%of carbon dioxide at 270 K 
%The function VaporPressure is first used to obtain the equilibrium pressure 
%at that temperature
%The result is multiplied by -1 to take into account the definition of
%residual enthalpy, DH = DHideal - DHresidual
P = VaporPressure(EoS,CO2,270,3e6);
Hvap =  -1*(Enthalpy(EoS,mix,270,P,'gas') - Enthalpy(EoS,mix,270,P,'liq')) 