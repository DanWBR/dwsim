%This example shows the application of the Group Contribution
%equation of state for the thermodynamic modelling of carbon dioxide + 
%n-nonane mixtures

%For application of GC-EoS, the following pure component
%parameters are required: critical temperature, molecular weight, critical 
%diameter and group decomposition
%The standard group parameters of the GC equation are already included in
%the program, taken from the following reference: Fornari, Fluid
%Phase Equlibr. 262 (2007) 187-209
%Groups are identified by the 'Group ID' parameter as defined in Table 1 of
%this reference
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01;
CO2.Tc = 304.2;
CO2.EoSParam(1) = 3.1294; %dc
CO2.EoSParam(2) = [46 1]; %Group decomposition. Matrix in which the first 
                            %element in each row correspond to the
                            %'Group ID' identifier, and the
                            %second element to the number of said groups in
                            %the molecule
                            
Non = cSubstance;
Non.name = 'n-Nonane';
Non.MW = 128.2;
Non.Tc = 594.7;
Non.EoSParam(1) = 5.778; %dc
Non.EoSParam(2) = [1 2;2 7]; %Group decomposition. Matrix in which the first 
                            %element in each row correspond to the
                            %'sub-Group' identifier of the group, and the
                            %second element to the number of said groups in
                            %the molecule

mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = Non;
mix.x = [0.5 0.5];

EoS = cGCEoS; 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)

%Alternatively, all group parameters can be defined manually. This can
%also be done to define new groups not included in the standard table

EoS = ClearGroups(EoS);
EoS = SetGroup(EoS,1,'CH3',600, 0.848, 316910, -0.9274, 0);
EoS = SetGroup(EoS,2,'CH2',600, 0.540, 356080, -0.8755, 0);
EoS = SetGroup(EoS,3,'CO2',304.2, 1.261, 531890, -0.5780, 0);

CO2.EoSParam(2) = [3 1];
Non.EoSParam(2) = [1 2;2 7];
mix.comp(1) = CO2;
mix.comp(2) = Non;

%Interaction parameters CH3-CH2
EoS = SetkG(EoS,1,2,'k1',1); 
EoS = SetkG(EoS,1,2,'k2',0); 
EoS = SetkG(EoS,1,2,'alfa',0); 
EoS = SetkG(EoS,2,1,'alfa',0); 

%Interaction parameters CH3-CO2
EoS = SetkG(EoS,1,3,'k1',0.892); 
EoS = SetkG(EoS,1,3,'k2',0); 
EoS = SetkG(EoS,1,3,'alfa',3.369); 
EoS = SetkG(EoS,3,1,'alfa',3.369); 

%Interaction parameters CH2-CO2
EoS = SetkG(EoS,2,3,'k1',0.814); 
EoS = SetkG(EoS,2,3,'k2',0); 
EoS = SetkG(EoS,2,3,'alfa',3.369); 
EoS = SetkG(EoS,3,2,'alfa',3.369); 

[beta, x, y, K, val, time, EoS] = Flash(EoS,1e6,313,mix,[0 1],[1 0],0.5)                                                               