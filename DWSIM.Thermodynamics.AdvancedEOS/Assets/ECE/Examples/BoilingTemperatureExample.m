%This example shows the calculation of the boiling temperature of hexane
%using Peng-Robinson EOS 

%Definition of component
Hex = cSubstance;
Hex.name = 'Hexane';
Hex.MW = 86.18; %g/mol
Hex.Tc = 507.4; %K
Hex.Pc = 3.014e6; %Pa
Hex.w = 0.2975;

%Specification of EoS
EoS = cPREoS;

%Calculation of the boiling temperature of hexane at 1 bar
%In this example, a value of 343 K is provided as initial guess of the
%boiling temperature
[T, flag, val, time, EoS] = BoilingTemp(EoS,Hex,1e5,343)

%If no initial guess of the boiling temperature is given, the program will
%calculate an estimation using the Antoine parameters of the component
%(AntA, AntB, AntC). If they have not been defined, the resulting initial
%guess for temperature will be 0 K, and probably calculations will either
%take a long time or will not converge
try
	[T, flag, val, time, EoS] = BoilingTemp(EoS,Hex,1e5)
catch
	disp('Call to BoilingTemp without temperature guess or Antoine parameters generated an error');
end

%Defining the Antoine parameters will allow to obtain a better estimation
%of temperature, thus reducing the calculation time
Hex.AntA = 20.7294;
Hex.AntB = 2697.55;
Hex.AntC = -48.78;
[T, flag, val, time, EoS] = BoilingTemp(EoS,Hex,1e5)

%As a second optional parameter, the settings of the 'fzero' numerical method used
%to solve the equality of fugacities and to calculate the vapor pressure can
%be modified, providing a structure generated with the 'optimset' function
%of Matlab (type 'help fzero' and 'help optimset' to obtain more
%information about these two Matlab functions)
%For example, with the following 'options' parameter the tolerance and maximum number 
%of iterations are established more strictly than default values (which use TolX = 1e-6)
options = optimset('TolX',1e-10,'MaxIter',50,'MaxFunEvals',50);

%Use square brackets '[]' in the optional parameter list to keep the
%default value of intermediate optional parameters
%In this case, the square brackets indicate that no numerical estimation of
%the vapor pressure is provided, and therefore the program will estimate it
%using the Antoine equation
[T, flag, val, time, EoS] = BoilingTemp(EoS,Hex,1e5,[],options)