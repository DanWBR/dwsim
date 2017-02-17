%This example shows the calculation of a bubble point of a mixture
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

%In order to calculate a bubble point, the composition of the liquid phase
%must be set in 'mix.x' or 'mix.molFrac'
mix.x = [0.2 0.8];

%Two possible calculations can be made: in the following case, the pressure
%of formation of the first bubble of gas and the composition of said gas is
%calculated at a given temperature and liquid composition
%It is necessary to provide a value of pressure, which is used as an
%initial estimation of the bubble pressure. It is also neccesary to provide
%an initial estimation of the composition of the gas phase
%The returned values are the equilibrium temperature and pressure, the
%composition of gas, distribution coefficients K = y/x, a flag indicating
%if the calculation were succesful and the final value of the objective
%function of the calculation algorithm, and the time used in calculations
%(type 'help BubblePoint' to get more details)
[T,P,y,K,flag,val,time] = BubblePoint(EoS,mix,313,1e6,[1 0],'P')

%In the following case, the temperature of formation of the first bubble of 
%gas and the composition of said gas is calculated at a given pressure and 
%liquid composition
%It is necessary to provide a value of temperature, which is used as an
%initial estimation of the bubble temperature
[T,P,y,K,flag,val,time] = BubblePoint(EoS,mix,313,1e6,[1 0],'T')

%As an optional parameter, the settings of the 'fzero' numerical method used
%to solve the equality of fugacities equilibrium condition can
%be modified, providing a structure generated with the 'optimset' function
%of Matlab (type 'help fzero' and 'help optimset' to obtain more
%information about these two Matlab functions)
%For example, with the following 'options' parameter the tolerance and maximum number 
%of iterations are established more strictly than default values (which use TolX = 1e-6)
options = optimset('TolX',1e-10,'MaxIter',50,'MaxFunEvals',50);
[T,P,y,K,flag,val,time] = BubblePoint(EoS,mix,313,1e6,[1 0],'P',options)

%Additional parameters of the numerical resolution algorithm such as the
%tolerance for convergence or the maximum number of iterations can be set
%with optimset. A careful selection of these parameters for each specific
%case can considerably reduce calculation times