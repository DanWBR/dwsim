%Components
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01; %g/mol
CO2.Tc = 304.2; %K
CO2.Pc = 7.38e6; %Pa
CO2.w = 0.2252;

EtOH = cSubstance;
EtOH.name = 'Ethanol';
EtOH.MW = 46.07; %g/mol
EtOH.Tc = 513.92; %K
EtOH.Pc = 6.14e6; %Pa
EtOH.w = 0.644;

%Mixture
mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = EtOH;

%Liquid composition 
mix.x = [0.4 0.6];

%Equation of state
EoS = cPREoS;

%Bubble point calculation
[T,P,y,K] = BubblePoint(EoS,mix,313,5e6,[1 0],'P')