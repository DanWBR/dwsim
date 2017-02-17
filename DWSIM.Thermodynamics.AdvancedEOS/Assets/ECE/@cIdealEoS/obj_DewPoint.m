function [val x K flag] = obj_DewPoint(P_or_T,T_or_P,guess_x,EoS,mix,type,flag)
%Objective function of the dew point algorithm 
%Auxiliary function of class cIdealEoS and derived classes, it should not
%be used directly
%
%Parameters:
%P: Iterated pressure (Pa)
%T: Temperature (K)
%guess_x: initial guess of liquid composition (molar fraction vector)
%EoS: equation of state used for calculations 
%mix: mixture (cMixture object)
%type: type of calculation, 'T' for calculating dew temperature, 'P' for
%   calculating dew pressure
%
%Results:
%val: value of the objective function (Sum xi - 1)
%x: calcuated liquid composition (molar fraction vector)
%K: distribution coefficients K = y/x

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

%Defines the liquid and gas phases
Liquid = mix;
Gas = mix;
y = mix.x;
Liquid.x = guess_x;

numC = mix.numC;

if strcmp(type,'T') == 1
    T = P_or_T;
    P = T_or_P;
elseif strcmp(type,'P') == 1
    P = P_or_T;
    T = T_or_P;    
else
    error(['The value "' type '" of "type" parameter is incorrect.']);
end

fiG = fug(EoS,T,P,Gas,'gas'); %Gas Fugacities

%Iteration loop, gas compositions
tol = 1e-5;
valI = 1;
num_iter = 0;
max_iter = 100;
x_calc_ant = guess_x;
K = zeros(numC,1);
x_calc = zeros(numC,1);
while ((valI > tol) && (num_iter < max_iter)) %Continues iterations until the difference between calculated gas compositions 
                                            %of two successive iterations is lower than tol, or until the maximum number 
                                            %of iterations is reached                                 
    num_iter = num_iter + 1;   
    
    fiL = fug(EoS,T,P,Liquid,'liq'); %Liquid fugacities
    
    for i = 1:numC %Distribution coefficients and calculated gas compositions
        K(i) = fiL(i)/fiG(i);
        x_calc(i) = y(i)/K(i);
    end

    valI = 0;
    for i = 1:numC 
        valI = valI + abs(x_calc_ant(i) - x_calc(i)); %Compares the calculated gas compositions of two successive iterations
    end
    
    x_calc_ant = x_calc;
    Liquid.x = x_calc./sum(x_calc); %Sets the composition of the gas mixture equal to the normalized calculated gas composition
end

if (num_iter == max_iter) %Checks if the maximum number of iterations has been reached, displays a warning if so
    s = sprintf('%f',valI);
    warning('MATLAB:EoS', ['Convergence error in DewPoint. Final value of objective function: ' s '.']);
end

for i = 1:numC %Checks if the trivial solution Liquid = Gas has been obtained, displays a warning if so
    single_phase = 1;
    if (abs(K(i) - 1) > 1e-2)
        single_phase = 0;
    end
end
if single_phase == 1
    flag = 2;
end

val = sum(x_calc) - 1; %Calculates Sum xi - 1 and returns it as result
x = x_calc; %Returns the calculated liquid composition