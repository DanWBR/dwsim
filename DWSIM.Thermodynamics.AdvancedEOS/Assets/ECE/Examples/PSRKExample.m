%This example shows the application of the Predictive Soave-Redlich-Kwong
%equation of state for the thermodynamic modelling of carbon dioxide + 
%hexane mixtures

%For application of the cubic PSRK-EoS, the following pure component
%parameters are required: critical temperature, critical pressure, acentric
%factor, alpha-function parameters(c1, c2 and c3) and group decomposition
%The standard group parameters of the PSRK equation are already included in
%the program, taken from the following reference: Horstmann et al., Fluid
%Phase Equlibr. 227 (2005) 157-164
%Groups are identified by the 'sub-Group' parameter as defined in the annex
%of this reference 
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.Tc = 304.2;
CO2.Pc = 7.37646e6;
CO2.w = 0.2252;
CO2.EoSParam(1) = 0.8255; %c1, alpha function
CO2.EoSParam(2) = 0.16755; %c2, alpha function
CO2.EoSParam(3) = -1.70390; %c3, alpha function
CO2.EoSParam(4) = [117 1]; %Group decomposition. Matrix in which the first 
                            %element in each row correspond to the
                            %'sub-Group' identifier of the group, and the
                            %second element to the number of said groups in
                            %the molecule
                            
Hex = cSubstance;
Hex.name = 'Hexane';
Hex.Tc = 507.4;
Hex.Pc = 3.014419e6;
Hex.w = 0.2975;
Hex.EoSParam(1) = 0.93264; %c1, alpha function
Hex.EoSParam(2) = 0; %c2, alpha function
Hex.EoSParam(3) = 0; %c3, alpha function
Hex.EoSParam(4) = [1 2;2 4]; %Group decomposition. Matrix in which the first 
                            %element in each row correspond to the
                            %'sub-Group' identifier of the group, and the
                            %second element to the number of said groups in
                            %the molecule

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = Hex;
mix.x = [0.5 0.5];

EoS = cPSRKEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)

%Alternatively, all group parameters can be defined manually. This can
%also be done to define new groups not included in the standard table

EoS = ClearGroups(EoS);
EoS = SetGroup(EoS,1,1,'CH3',0.9011,0.8480);
EoS = SetGroup(EoS,2,1,'CH2',0.6744,0.5400);
EoS = SetGroup(EoS,3,2,'CO2',1.3000,0.9820);

EoS = SetkG(EoS,1,2,'a',919.80); %IMPORTANT: Interaction coefficients are 
                                 %defined between main groups, not between
                                 %sub-groups
EoS = SetkG(EoS,2,1,'a',-38.672); 
EoS = SetkG(EoS,1,2,'b',-3.9132); 
EoS = SetkG(EoS,2,1,'b',0.8615); 
EoS = SetkG(EoS,1,2,'c',0.004631); 
EoS = SetkG(EoS,2,1,'c',-0.001791); 
    
CO2.EoSParam(4) = [3 1]; %Redefines group decomposition according to the new groups
Hex.EoSParam(4) = [1 2;2 4];
mix.comp(1) = CO2;
mix.comp(2) = Hex;

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)               