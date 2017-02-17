%Components
hex = cSubstance;
hex.name = 'Hexadecane';
hex.MW = 226.44; %g/mol
hex.EoSParam(1) = 7.245; %dc
hex.EoSParam(2) = [1 2;2 14]; %group decomposition (2 CH3, 14 CH2)

%Mixture
mix = cMixture;
mix.comp = hex;
mix.x = 1;

%Equation of state
EoS = cGCEoS;

%Calculates liquid density at ambient conditions
Density(EoS,mix,298,1e5,'liq')