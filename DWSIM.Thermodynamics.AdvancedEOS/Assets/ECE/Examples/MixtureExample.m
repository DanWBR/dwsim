%This file shows several examples of utilization of cMixture objects

%The basic definition of a cMixture object
mixture = cMixture;

%cMixture objects store cSubstance objects representing components in a
%mixture
CO2 = cSubstance;
CH4 = cSubstance;

mixture.comp(1) = CO2; 
mixture.comp(2) = CH4;

%They also store the composition of the mixture
mixture.x = [0.8 0.2];

%The properties of cSubstance objects stored in mixtures can be retrieved and
%modified using dot/parenthesis syntax
mixture.comp(1).name = 'Carbon dioxide';
mixture.comp(1).MW = 44;
mixture.comp(2).name = 'Methane';
mixture.comp(2).MW = 16;

mixture.comp(1)
mixture.comp(2)

%Typing the name of a cMixture object in the command window will display
%its properties
mixture

%The properties of the object can also be defined when creating the object
CO2.name = 'Carbon dioxide';
CO2.MW = 44;
CH4.name = 'methane';
CH4.MW = 16;
subst_vect(1) = CO2;
subst_vect(2) = CH4;
mixture = cMixture(subst_vect,[0.8 0.2])

%Type 'help cMixture' to obtain a list of the parameters that can be
%defined during the creation of the object

%CMixture objects also calculate the mass fraction and molecular weight of
%mixtures
massFrac = mixture.massFrac
MW = mixture.MW
%These properties are calculated from the molar composition and the
%molecular weight of components. Therefore, incorrect results will be
%obtained if any of these parameters is not defined

%The property 'numC' can be used to obtain the number of components in a
%mixture
numC = mixture.numC

%'numC can also be used to define the number of components in a mixture
mixture2 = cMixture;
mixture2.numC = 3;

%The properties of components can be then defined using dot/parenthesis
%syntax
mixture2.comp(1).name = 'Carbon dioxide';
mixture2.comp(2).name = 'Methane';
mixture2.comp(3).name = 'Nitrogen';
mixture2

%Besides components and compositions, cMixture objects also store
%interaction coefficients between components

%Interaction coefficients can be defined as a matrix
mixture.k = [0 0.1;0.1 0];

%It is also possible to define individual components of the matrix
mixture.k(1,2) = 0.3;
k1 = mixture.k

%It is possible to define up to 9 different interaction coefficients,
%denominated 'k1', 'k2' ... 'k9'
mixture.k2 = [0 0.2;0.2 0];
mixture.k9 = [0 0.9;0.9 0];
k9 = mixture.k9(2,1)

%Interaction coefficients 'k' and 'k1' are the same coefficient
k = mixture.k(2,1)
k1 = mixture.k1(2,1)

%It is also possible to define temperature-dependant interaction
%coefficients
mixture.k(1,2) = '10/T + 0.05';

%If a reference to a temperature-dependant interaction coefficient is made,
%the program will attempt to evaluate the temperature-dependant expression
%and return the resulting numerical value
T = 300;
k1 = mixture.k(1,2)

%If this is not possible (for instance, because T is not defined or an
%error is obtained when attempting to evaluate the expression), a value of
%0 will be returned
clear T;
k1 = mixture.k(1,2)

%Since the numerical value of the interaction coefficient rather than the 
%temperature dependand function is returned, care should be taken when
%defining temperature dependant-functions for several components of the
%interaction coefficient
%For instance, with the following code a constant value will be
%assigned to k(2,1) instead of the temperature-dependant function
T = 300;
mixture.k2(1,2) = '-25/T + 0.07';
mixture.k2(2,1) = mixture.k2(1,2);
k12 = mixture.k2(1,2)
k21 = mixture.k2(2,1)
T = 350;
k12 = mixture.k2(1,2)
k21 = mixture.k2(2,1)

%In this case, the temperature-dependant function should be assigned to
%each component of the matrix instead
mixture.k2(1,2) = '-25/T + 0.07';
mixture.k2(2,1) = '-25/T + 0.07';
T = 300;
k12 = mixture.k2(1,2)
k21 = mixture.k2(2,1)
T = 350;
k12 = mixture.k2(1,2)
k21 = mixture.k2(2,1)

%cMixture objects can be used as any other variable
mixture3 = mixture2

%They can also be grouped in vectors, matrixes, etc
mix_vector(1) = mixture;
mix_vector(2) = mixture2;
mix_vector(3) = mixture3;
mix_vector