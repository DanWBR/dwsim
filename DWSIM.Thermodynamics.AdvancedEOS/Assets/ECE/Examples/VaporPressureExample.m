%This example shows the calculation of the vapor pressure of hexane
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

%Calculation of the vapor pressure of hexane at 400 K
%In this example, a value of 5 bar is provided as initial guess of the
%vapor pressure
[P, flag, val, time, EoS] = VaporPressure(EoS,Hex,400,5e5)

%If no initial guess of the vapor pressure is given, the program will
%calculate an estimation using the Antoine parameters of the component
%(AntA, AntB, AntC). If they have not been defined, the resulting initial
%guess for pressure will be 1 Pa, and probably calculations will either
%take a long time or will not converge
try
	[P, flag, val, time, EoS] = VaporPressure(EoS,Hex,400)
catch
	disp('Call to VaporPressure without pressure guess or Antoine parameters generated an error');
end

%Defining the Antoine parameters will allow to obtain a better estimation
%of pressure, thus reducing the calculation time
Hex.AntA = 20.7294;
Hex.AntB = 2697.55;
Hex.AntC = -48.78;
[P, flag, val, time, EoS] = VaporPressure(EoS,Hex,400)

%As a second optional parameter, the settings of the 'fzero' numerical method used
%to solve the equality of fugacities and to calculate the vapor pressure can
%be modified, providing a structure generated with the 'optimset' function
%of Matlab (type 'help fzero' and 'help optimset' to obtain more
%information about these two Matlab functions)
%For example, with the following 'options' parameter the tolerance and maximum number 
%of iterations are established more strictly than default values (which use TolX = 1e-4)
options = optimset('TolX',1e-6,'MaxIter',50,'MaxFunEvals',50);

%Use square brackets '[]' in the optional parameter list to keep the
%default value of intermediate optional parameters
%In this case, the square brackets indicate that no numerical estimation of
%the vapor pressure is provided, and therefore the program will estimate it
%using the Antoine equation
[P, flag, val, time, EoS] = VaporPressure(EoS,Hex,400,[],options)