function dif = mandelic_ESG(par)
%To run the correlation:
%[X FVAL FLAG] = fminsearch('mandelic_ESG', 0)

%Components
CO2 = cSubstance;
CO2.name = 'Carbon Dioxide';
CO2.MW = 44.01; %g/mol
CO2.Tc = 304.2; %K
CO2.Pc = 7.38e6; %Pa
CO2.w = 0.2252;

Mand = cSubstance;
Mand.name = 'Mandelic acid';
Mand.MW = 152.14; %g/mol
Mand.Tc = 861.36; %K
Mand.Pc = 4.529e6; %Pa
Mand.w = 1.178;
Mand.Tf = 411; %K
Mand.Hf = 13000; %J/mol

%Mixture
mix = cMixture;
mix.comp(1) = CO2;
mix.comp(2) = Mand;
mix.k1 = [0 par;par 0];
mix.x = [0.9 0.1];

%Equation of state
EoS = cPREoS;

%Matrix of experimental data, T (K), P(MPa), Solubility (mol frac.)
data = [318.15	10.23	3.60E-05
318.15	12.09	1.41E-04
318.15	14.16	3.20E-04
318.15	16.3	6.04E-04
318.15	18.37	9.70E-04
318.15	20.5	1.37E-03
318.15	22.57	1.79E-03];	

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
ylabel('y_m_a_n_d');
legend('Experimental','Model');