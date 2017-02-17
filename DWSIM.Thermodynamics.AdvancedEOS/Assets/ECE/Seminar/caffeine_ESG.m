function dif = caffeine_ESG(par)
%To run the correlation:
%[X FVAL FLAG] = fminsearch('caffeine_ESG', 0)

%Components
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01; %g/mol
CO2.Tc = 304.2; %K
CO2.Pc = 7.38e6; %Pa
CO2.w = 0.2252;

Caf = cSubstance;
Caf.name = 'Caffeine';
Caf.MW = 194.19; %g/mol
Caf.Tc = 780.04; %K
Caf.Pc = 4.217e6; %Pa
Caf.w = 0.793;
Caf.Tf = 534.2; %K
Caf.Hf = 4010; %J/mol

%Mixture
mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = Caf;
mix.k1 = [0 par;par 0];
mix.x = [0.5 0.5];

%Equation of state
EoS = cPREoS;

%Matrix of experimental data, T (K), P(MPa), Solubility (mol frac.)
data = [313	19.9	2.95E-04
313	21.9	3.29E-04
313	23.9	3.87E-04
313	25.9	4.01E-04
313	27.9	4.28E-04
313	29.9	4.58E-04
313	31.9	4.85E-04
313 34.9    5.44E-04];	

numData = size(data,1);

%Calculates the solubility each experimental T, P and compares
%experimental and calculated solubility
Scalc = zeros(numData, 1);
dif = 0;
for i = 1:numData
    T = data(i,1);
    P = data(i,2)*1e6;
    
    [S,val,time,flag] = Solubility(EoS,mix,T,P,[1-data(i,3) data(i,3)]);
    
    Scalc(i) = S(2);
    
    s = sprintf('%d/%d: Sexp %f/ Scalc %f',i,numData,data(i,3),S(2));
    disp(s); 
    
    if flag == 1
        dif = dif + abs(S(2)-data(i,3))/data(i,3)*100;
    else
        dif  = dif + 100; %Adds a 100% error when the calculations did not converge
    end
end

dif = dif/numData;

%Reports iteration results
s1 = '';
for i = 1:length(par)
    s = sprintf('%.2f',par(i));
    s1 = [s1,s,','];
end

s2 = sprintf('%.2f',dif);
s = ['ITERATION RESULTS: parameter(s): ',s1,' objective function: ',s2];
disp(s);

figure(1)
hold off;
plot(data(:,2),data(:,3),'ob');
hold on;
plot(data(:,2),Scalc,'-b');
xlabel('P (MPa)');
ylabel('y_c_a_f_f');
legend('Experimental','Model');