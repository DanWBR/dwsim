function res = obj_FlashMulti(X,z,K,NumP,NumC,EoS)
%Objective function of MultiFlash, modification of Rachford-Rice equation
%for multiple phases accounting for phase stability
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

epsilon = 1e-6; %Rounding value of beta and thita 

beta = zeros(NumP,1)';
thita = zeros(NumP,1)';
    
%Constructs beta andd thita vectors from X
for i = 2:NumP
    if X(i-1) < 0
        beta(i) = 0;
        thita(i) = -X(i-1);
    else
        beta(i) = X(i-1);
        thita(i) = 0;
    end
end

%The first phase is taken as reference and always must have beta>0 and thita = 0
thita(1) = 0; 
beta(1) = 1-sum(beta);
beta = abs(beta);

%Rounds beta and thita values smaller than epsilon
for i = 1:NumP
    if ((beta(i) <= epsilon))
        beta(i) = 0;
    end
    if (thita(i) <= epsilon)
        thita(i) = 0;
    end
end

beta = abs(beta)/sum(abs(beta)); %Normalization of beta

%Calculation of objective function
res = zeros(1,NumP-1);
for j = 2:NumP
    sumat = 0;
    for i = 1:NumC
        sum_int = 0;
        for k = 2:NumP
            sum_int = sum_int + beta(k)*(K(k,i)*exp(thita(k)) - 1);
        end
        sumat = sumat + z(i)*(K(j,i)*exp(thita(j))-1)/(1+sum_int);
    end
    res(j-1) = sumat*1e8;
end

pause(0.01); %Prevents hang of Matlab GUI