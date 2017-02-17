function [ge,gamma] = UNIFAC(EoS,mix,T)
%Calculates UNIFAC activity coefficient for application in PSRK EoS
%Auxiliary function, not to be used directly

%Copyright (c) 2011 Ángel Martín, University of Valladolid (Spain)
%This program is free software: you can redistribute it and/or modify
%it under the terms of the GNU General Public License as published by
%the Free Software Foundation, either version 3 of the License, or
%(at your option) any later version.
%This program is distributed in the hope that it will be useful,
%but WITHOUT ANY WARRANTY; without even the implied warranty of
%MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%GNU General Public License for more details.
%You should have received a copy of the GNU General Public License
%along with this program.  If not, see <http://www.gnu.org/licenses/>.

R_IG = 8.31; %Gas constant, J/mol·K

numC = mix.numC;
x = mix.x;

for i = 1:numC
    v_mat{i} = mix.comp(i).EoSParam(4);
end

%Finds unique groups
Ug = [];
for i = 1:numC
    v = v_mat{i};
    
    Ug = [Ug v(:,1)'];
end
Ug = unique(Ug);
Ug = sort(Ug);

%Molecule parameters
r = zeros(numC,1);
q = zeros(numC,1);
for i = 1:numC
    v = v_mat{i};

    for k = 1:size(v,1)
        r(i) = r(i) + v(k,2)*EoS.mGroup(v(k,1)).R;
        q(i) = q(i) + v(k,2)*EoS.mGroup(v(k,1)).Q;
    end 
end

%Group composition
sum_X_tot = 0;
for i = 1:numC
    v = v_mat{i};
    
    for k = 1:size(v,1)
        sum_X_tot = sum_X_tot + x(i)*v(k,2);
    end
end

sum_X = zeros(1,NumG(EoS));
for i = 1:numC
    v = v_mat{i};
   
    for k = 1:size(v,1)
        sum_X(v(k,1)) = sum_X(v(k,1)) + x(i)*v(k,2);
    end
end

X = zeros(NumG(EoS),1);
for k = 1:NumG(EoS)
    X(k) = sum_X(k)/sum_X_tot;
end

%***********************************************************
%Combinatorial
%***********************************************************
sum_thita = 0;
sum_phi = 0;
for i = 1:numC
    sum_thita = sum_thita + q(i)*x(i);
    sum_phi = sum_phi + r(i)*x(i);
end

thita = zeros(numC,1);
phi = zeros(numC,1);
for i = 1:numC
    thita(i) = q(i)*x(i)/sum_thita;
    phi(i) = r(i)*x(i)/sum_phi;
end

z = 10;
l = zeros(numC,1);
for i = 1:numC
   l(i) = z/2*(r(i)-q(i))-(r(i)-1);
end

sum_l = 0;
for i  = 1:numC
    sum_l = sum_l + x(i)*l(i);
end

log_gamma_c = zeros(numC,1);
for i = 1:numC
    log_gamma_c(i) = log(phi(i)/x(i)) + z/2*q(i)*log(thita(i)/phi(i)) + l(i) - phi(i)/x(i)*sum_l;
end

%***********************************************************
%Residual
%***********************************************************
int = zeros(NumG(EoS),NumG(EoS));
for a = 1:length(Ug)
    for b = 1:length(Ug)
        i = Ug(a);
        j = Ug(b);
        int(i,j) = exp(-(GetkG(EoS,i,j,'a') + GetkG(EoS,i,j,'b')*T + GetkG(EoS,i,j,'c')*T^2)/T);
    end
end


%Group residual activity coefficient, global composition
sum_thitaR = 0;
for a = 1:length(Ug)
    i = Ug(a);
    sum_thitaR = sum_thitaR + EoS.mGroup(i).Q*X(i);
end

for a = 1:length(Ug)
    i = Ug(a);
    thitaR(i) = EoS.mGroup(i).Q*X(i)/sum_thitaR;
end

for c = 1:length(Ug)
    k = Ug(c);
    sumGk_1 = 0;
    sumGk_2 = 0;
    for a = 1:length(Ug)
        i = Ug(a);
        sumGk_1 = sumGk_1 + thitaR(i)*int(i,k);
        sumGk_3 = 0;
        for b = 1:length(Ug)
            j = Ug(b);
            sumGk_3 = sumGk_3 + thitaR(j)*int(j,i);
        end
        sumGk_2 = sumGk_2 + thitaR(i)*int(k,i)/sumGk_3;
    end
    log_GammaR(k) = EoS.mGroup(k).Q*(1-log(sumGk_1)-sumGk_2);
end

log_gamma_r = zeros(numC,1);
for i = 1:numC
    v = v_mat{i};
    
    %Reference composition
    sum_X_inf = 0;
    X_inf = zeros(1,NumG(EoS));
    for k = 1:size(v,1)
        sum_X_inf = sum_X_inf + v(k,2);
    end
    for k = 1:1:size(v,1)
        X_inf(v(k,1)) = v(k,2)/sum_X_inf;
    end

    sum_thitaR_inf = 0;
    for a = 1:length(Ug)
        k = Ug(a);
        sum_thitaR_inf = sum_thitaR_inf + EoS.mGroup(k).Q*X_inf(k);
    end
    for a = 1:length(Ug)
        k = Ug(a);
        thitaR_inf(k) = EoS.mGroup(k).Q*X_inf(k)/sum_thitaR_inf;
    end
    
    %Reference group residual activity coefficient
    for a = 1:length(Ug)
        k = Ug(a);
        sumGk_1 = 0;
        sumGk_2 = 0;
        for b = 1:length(Ug)
            m = Ug(b);
            sumGk_1 = sumGk_1 + thitaR_inf(m)*int(m,k);
            sumGk_3 = 0;
            for c = 1:length(Ug)
                j = Ug(c);
                sumGk_3 = sumGk_3 + thitaR_inf(j)*int(j,m);
            end
            sumGk_2 = sumGk_2 + thitaR_inf(m)*int(k,m)/sumGk_3;
        end
        log_GammaR_inf(k) = EoS.mGroup(k).Q*(1-log(sumGk_1)-sumGk_2);
    end
    
    %Total
    for k = 1:size(v,1)
        log_gamma_r(i) = log_gamma_r(i) + v(k,2)*(log_GammaR(v(k,1))-log_GammaR_inf(v(k,1)));
    end
end


%***********************************************************
%Total
%***********************************************************
log_gamma = zeros(numC,1);
gamma = zeros(numC,1);
for i = 1:numC
    log_gamma(i) = log_gamma_c(i) + log_gamma_r(i);
    gamma(i) = exp(log_gamma(i));
end

ge = 0;

for i = 1:numC
    ge = ge + x(i)*log_gamma(i);
end
ge = ge*R_IG*T;