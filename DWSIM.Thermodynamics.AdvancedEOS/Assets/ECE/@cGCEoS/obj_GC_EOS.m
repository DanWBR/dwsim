function [obj_fun, Z, fi] = obj_GC_EOS(Z,EoS,T,P,mix)
%Objective function for calculation of compressibility and fugacity
%coefficients with GC-EOS
%Auxiliary function, not to be used directly
%
%Reference: S. Skjold-Jorgensen, Fluid Phase Equilibr 16 (1984) 317-351

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

R = 82.05; %cm3 atm/mol K
P = P/1e5/1.01325; %atm
V = Z*R*T/P; %cm3/mol

%Reads component&mixture properties
numC = mix.numC;
x = mix.x;

%**************************************************************************
%Compresibility coefficient
%**************************************************************************
%Zfv 
d = zeros(numC,1);
dc = zeros(numC,1);
for i = 1:numC
    dc(i) = mix.comp(i).EoSParam(1);
    d(i) = 1.065655*dc(i)*(1-0.12*exp(-2*mix.comp(i).Tc(i)/(3*T))); %Eq. 14 of reference
end

l1 = 0;
l2 = 0;
l3 = 0;
for i = 1:numC
    l1 = l1 + x(i)*d(i); %Eq. 3 of reference
    l2 = l2 + x(i)*d(i)^2; %Eq. 3 of reference
    l3 = l3 + x(i)*d(i)^3; %Eq. 3 of reference
end

Y = (1-pi*l3/(6*V))^(-1); %Eq. 3 of reference

dY_dV = -Y^2*l3*pi/(6*V^2); %Eq. B2 of reference

Zfv = -V*(3*l1*l2/l3 + (l2^3/l3^2)*(2*Y-1-1/Y)+1/Y)*dY_dV; %Eq. B2 of reference

%Zatr
NumG = 0;
v = [];
for i = 1:numC
    NumG_ind(i) = size(mix.comp(i).EoSParam(2),1);
    NumG = NumG + NumG_ind(i);
    v_ind{i} = mix.comp(i).EoSParam(2);
    v = [v; v_ind{i}];
end

z_coord = 10; %coordination number

T_ref_ast = zeros(NumG);
for i = 1:NumG
    for j = 1:NumG
        T_ref_ast(i,j) = 0.5*(EoS.mGroup(v(i,1)).Tref+EoS.mGroup(v(j,1)).Tref); %Eq. 18 of reference
    end
end

k = zeros(NumG);
for i = 1:NumG
    for j = 1:NumG
        indx1 = v(i,1);
        indx2 = v(j,1);
        k(i,j) = EoS.mk1G(indx1 ,indx2)*(1+EoS.mk2G(indx1 ,indx2)*log(T/T_ref_ast(i,j))); %Eq. 18 of reference
    end
end

g = zeros(NumG,1);
for i = 1:NumG
   g(i) = EoS.mGroup(v(i,1)).g1*(1+EoS.mGroup(v(i,1)).g2*(T/EoS.mGroup(v(i,1)).Tref-1)+EoS.mGroup(v(i,1)).g3*log(T/EoS.mGroup(v(i,1)).Tref)); %Eq. 15 of reference
end

gij = zeros(NumG);
for i = 1:NumG
    for j = 1:NumG
        gij(i,j) = k(i,j)*(g(i)*g(j))^0.5; %Eq. 17 of reference
    end
end

Dg = zeros(NumG);
for i = 1:NumG
    for j = 1:NumG
        Dg(j,i) = gij(j,i)-gij(i,i); %Eq. 8 of reference
    end
end

q_p = 0;
for i = 1:numC
    sum_int = 0;
    for j = 1: NumG_ind(i)
        vcomp = v_ind{i};
        group_indx = vcomp(j,1);
        group_number = vcomp(j,2);
        sum_int = sum_int+group_number*EoS.mGroup(group_indx).q;
    end
    q_p = q_p+x(i)*sum_int; %Eq. 8 of reference
end

tita = zeros(NumG,1);
indx = 0;
for i = 1:numC
    for j = 1: NumG_ind(i)
        indx = indx + 1;
        vcomp = v_ind{i};
        sum_int = x(i)*vcomp(j,2); 
        tita(indx) = EoS.mGroup(vcomp(j,1)).q/q_p*sum_int; %Eq. 8 of reference
    end
end

tau = zeros(NumG);
for i = 1:NumG
    for j =1:NumG
        indx1 = v(i,1);
        indx2 = v(j,1);
        tau(j,i) = exp(EoS.malfaG(indx2,indx1)*Dg(j,i)*q_p/(V*R*T)); %Eq. 8 of reference
    end
end

H1 = zeros(NumG,1);
H2 = zeros(NumG,1);
H3 = zeros(NumG,1);
H4 = zeros(NumG,1);
for j = 1:NumG %Eq. B9 of reference
   for k = 1:NumG
        indx1 = v(j,1);
        indx2 = v(k,1);
        H1(j) = H1(j) + tita(k)*tau(k,j)*gij(k,j)*q_p/(V*R*T)*EoS.malfaG(indx2,indx1)*Dg(k,j)*q_p/(V*R*T);
        H2(j) = H2(j) + tita(k)*tau(k,j)*gij(k,j)*q_p/(V*R*T);
        H3(j) = H3(j) + tita(k)*tau(k,j)*EoS.malfaG(indx2,indx1)*Dg(k,j)*q_p/(V*R*T);
        H4(j) = H4(j) + tita(k)*tau(k,j);
   end
end

v_m = zeros(NumG,1);
indx = 0;
for i = 1:numC
    for j = 1: NumG_ind(i)
        indx = indx + 1;
        vcomp = v_ind{i};
        v_m(indx) = vcomp(j,2)*x(i); %Eq. B3 of reference
    end
end

sum_int = 0;
for j = 1:NumG
   sum_int = sum_int + v_m(j)*EoS.mGroup(v(j,1)).q*(H1(j)+H2(j)-H2(j)*H3(j)/H4(j))/H4(j); 
end
Zatr = sum_int*(-z_coord/2); %Eq. B3 of reference

%Compresibility coefficient
Z_ini = Z;
Z = Zfv+Zatr+1; %Eq. B1 of reference
obj_fun = Z-Z_ini;

%**************************************************************************
%Fugacity coefficient
%**************************************************************************

%fifv
lp1 = zeros(numC,1);
lp2 = zeros(numC,1);
lp3 = zeros(numC,1);
Yp = zeros(numC,1);
fifv = zeros(numC,1);
for i = 1:numC
    lp1(i) = d(i); %Eq. B8 of reference
    lp2(i) = d(i)^2; %Eq. B8 of reference
    lp3(i) = d(i)^3;  %Eq. B8 of reference
    Yp(i) = Y^2*pi*d(i)^3/(6*V); %Eq. B8 of reference
    
    fifv(i) = 3*(Y-1)*(l1*l2/l3)*(lp1(i)/l1+lp2(i)/l2-lp3(i)/l3+Yp(i)/(Y-1))+(l2^3/l3^2)*(Y^2-Y-log(Y))*(3*lp2(i)/l2-2*lp3(i)/l3)+(l2^3/l3^2)*(2*Y-1-1/Y)*Yp(i)+log(Y)+(1/Y)*Yp(i); %Eq. B8 of reference
end


%fiatr
PS = zeros(numC,NumG);
MS = zeros(numC,1);
indx = 0;
for i = 1:numC
    for j = 1: NumG_ind(i)
        indx = indx + 1;
        vcomp = v_ind{i};
        PS(i,indx) = vcomp(j,2)*EoS.mGroup(vcomp(j,1)).q; %Eq. B9 of reference
        
        MS(i) = MS(i) + PS(i,indx); %Eq. B9 of reference
    end
end

H5 = zeros(numC,NumG);
H6 = zeros(numC,NumG);
for i = 1:numC %Eq. B9 of reference
    for j = 1:NumG
        H5(i,j) = 0;
        H6(i,j) = 0;
        for k = 1:NumG
            H5(i,j) = H5(i,j)+PS(i,k)*tau(k,j)*gij(k,j)*q_p/(V*R*T);
            H6(i,j) = H6(i,j)+PS(i,k)*tau(k,j);
        end
    end
end

fiatr = zeros(numC,1);
for i = 1:numC
    sum_1 = 0;
    sum_2 = 0;
    sum_3 = 0;
    for j = 1:NumG
        sum_1 = sum_1 + PS(i,j)*H2(j)/H4(j);
        sum_2 = sum_2 + tita(j)*(H5(i,j)+MS(i)*H1(j))/H4(j);
        sum_3 = sum_3 + tita(j)*H2(j)*(H6(i,j)-H4(j)*MS(i)+H3(j)*MS(i))/H4(j)^2;
    end
    fiatr(i) = -(z_coord/2)*(sum_1+sum_2-sum_3); %Eq. B9 of reference
end

%Total
log_fi = zeros(numC,1);
fi = zeros(numC,1);
for i = 1:numC
    log_fi(i) = fifv(i)+fiatr(i)-log(Z); %Eq. B7 of reference
    fi(i) = exp(log_fi(i));
end