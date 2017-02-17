function res = corr_met_hex_PCSAFT(par)
%To run the minimization:
%[X FVAL FLAG] = fminsearch('corr_met_hex_PCSAFT',0)

%Components
CH4 = cSubstance;
CH4.name = 'Methane';
CH4.MW = 16.043;
CH4.EoSParam(1) = 1; %m
CH4.EoSParam(2) = 3.7039; %sigma
CH4.EoSParam(3) = 150.03; %epsilon/k

Hex = cSubstance;
Hex.name = 'Hexane';
Hex.MW = 86.177;
Hex.EoSParam(1) = 3.0576; %m
Hex.EoSParam(2) = 3.7983; %sigma
Hex.EoSParam(3) = 236.77; %epsilon/k

%Mixture
mix = cMixture;
mix.comp(1) = CH4;
mic.comp(2) = Hex;
mix.x = [0 1];

%The binary interaction parameter is the correlated parameter
mix.k = [0 par;par 0];

%Equation of State
EoS = cPCSAFTEoS;

%Matrix of experimental data (T (K), P(MPa), xCH4)
data = [273.16	1.0349	0.0567
273.16	2.0691	0.1116
273.16	4.1368	0.2127
273.16	6.8947	0.3328
273.16	9.6526	0.4435
273.16	11.032	0.4924
273.16	13.789	0.5933
273.16	15.168	0.6401];

numData = size(data,1);

%Calculates a bubble point at each experimental T,x and compares
%experimental and calculated P
dif = 0;
Pcalc = zeros(1,size(data,1));
ycalc = zeros(1,size(data,1));
for i = 1:numData
    mix.x = [data(i,3) 1-data(i,3)];
    guess_y = [1 0];
    guess_P = data(i,2)*1e6;
    T = data(i,1);
    
    [T,P,y,K,flag,val,time,EoS] = BubblePoint(EoS,mix,T,guess_P,guess_y,'P'); 
    
    Pcalc(i) = P;
    ycalc(i) = y(1);  
    
    s = sprintf('%d/%d: Pexp %d Pa/ Pcalc %d Pa',i,numData,guess_P,P);
    disp(s); 
    
    if flag == 1
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
plot(data(:,3),Pcalc,'-b');
hold off;
xlabel ('x_C_H_4');
ylabel ('P (MPa)');