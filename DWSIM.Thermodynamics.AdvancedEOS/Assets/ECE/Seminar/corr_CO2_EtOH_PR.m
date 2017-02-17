function res = corr_CO2_EtOH_PR(par)
%To run the minimization:
%[X FVAL FLAG] = fminsearch('corr_CO2_EtOH_PR',0)

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

%The binary interaction parameter is the correlated parameter
mix.k = [0 par;par 0];

%Equation of State
EoS = cPREoS;

%Matrix of experimental data (T (K), P(MPa), xCH4)
data = [313.14	0.91	0.053	0.985
313.14	1.84	0.101	0.986
313.14	2.92	0.172	0.988
313.14	3.93	0.228	0.989
313.14	4.5	0.28	0.991
313.14	4.97	0.32	0.991
313.14	5.49	0.361	0.992
313.14	5.9	0.4	0.992
313.14	6.31	0.44	0.991
313.14	6.65	0.49	0.99
313.14	7.02	0.54	0.989
313.14	7.31	0.584	0.987
313.14	7.6	0.659	0.982
313.14	7.73	0.749	0.977];

numData = size(data,1);

%Calculates a bubble point at each experimental T,x and compares
%experimental and calculated P
dif = 0;

for i = 1:numData
    mix.x = [data(i,3) 1-data(i,3)];
    guess_y = [1 0];
    guess_P = data(i,2)*1e6;
    T = data(i,1);
    [T,P,y,K,flag,val,time,EoS] = BubblePoint(EoS,mix,T,guess_P,guess_y,'P'); 
    
    if flag == 1
	    Pcalc(i) = P;
		ycalc(i) = y(1);  

		s = sprintf('%d/%d: Pexp %d Pa/ Pcalc %d Pa',i,numData,guess_P,P);
		disp(s); 
        dif = dif + abs(P-guess_P)/P*100;
    else
        dif  = dif + 100; %Adds a 100% error when the calculations did not converge
    end
end
res = dif/numData;

%Reports iteration results
s1 = '';
for i = 1:length(par)
    s = sprintf('%.2f',par(i));
    s1 = [s1,s,','];
end

s2 = sprintf('%.2f',res);
s = ['ITERATION RESULTS: parameter(s): ',s1,' objective function: ',s2];
disp(s);

figure(1)
plot(data(:,3),data(:,2)*1e6,'ob')
hold on;
plot(data(:,4),data(:,2)*1e6,'ob');
plot(data(1:length(Pcalc),3),Pcalc,'-b');
plot(ycalc,Pcalc,'-b');
hold off;
xlabel ('x_C_O_2');
ylabel ('P (MPa)');