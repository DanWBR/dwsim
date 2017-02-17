%Components
EtOH = cSubstance;
EtOH.name = 'Ethanol';
EtOH.MW = 46.07; %g/mol
EtOH.Tc = 513.92; %K
EtOH.Pc = 6.14e6; %Pa
EtOH.w = 0.644;

H2O = cSubstance;
H2O.name = 'Water';
H2O.MW = 18.015;
H2O.Tc = 647.3;
H2O.Pc = 22.048e6;
H2O.w = 0.3442;

%Mixture
mix = cMixture;
mix.comp(1) = EtOH;
mix.comp(2) = H2O;

%Interactrion coefficients
mix.k1 = [0 0.2690;0.2690 0];
mix.k2 = [0 0.1978;0.1978 0];
mix.k3 = [0 1.3169;0.0516 0];

%Equation of state
EoS = cPRWSEoS;

%T-xy diagrams
TxyDiagram(EoS,mix,300,1e5,[1 0]); %1 bar
TxyDiagram(EoS,mix,400,1e6,[1 0]); %10 bar
TxyDiagram(EoS,mix,500,3e6,[1 0]); %30 bar