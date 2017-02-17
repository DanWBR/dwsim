%This file shows several examples of utilization of cMixture objects

%The basic definition of a cSubstance object
CO2 = cSubstance;

%After defining the object, its properties can be modified using dot syntax
CO2.Tc = 304.2;
CO2.Pc = 7.38e6;

%Properties can also be accessed using dot syntax
CO2.Tc

%Typing the name of a cSubstance object in the command window will display
%its properties
CO2

%The properties of the object can also be defined when creating the object
CO2 = cSubstance('Carbon Dioxide',44.01,304.2,7.38e6)
%Type 'help cSubstance' to obtain a list of the parameters that can be
%defined during the creation of the object

%The property 'EoSParam' alflows storing additional parameters as required 
%for application of some models
CO2.EoSParam = 10

%Any number of 'EoSParam' properties can be defined
CO2.EoSParam(37) = 0.1

%EoSParam properties can be defined or accessed using standard
%dot/parenthesis syntax
CO2.EoSParam(1)
CO2.EoSParam(2)
CO2.EoSParam(37)

%When a reference to 'EoSParam' without specifying index is made,
%EoSParam(1) will be retrieved or set
CO2.EoSParam

%Each element of EoSParam can be either a scalar or a matrix
CO2.EoSParam(2) = [1 2 3;4 5 6];
CO2.EoSParam(1)
CO2.EoSParam(2)

%cSubstance objects can be used as any other variable
CO2_copy = CO2

%They can also be grouped in vectors, matrixes etc.
substance_vector(1) = CO2; 
substance_vector(2) = CO2_copy;