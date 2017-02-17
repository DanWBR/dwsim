function [beta, x, y, K, val, time, EoS] = Flash(EoS,P,T,mix,guess_x,guess_y,guess_beta)
%function [beta, x, y, K, val, time, EoS] = Flash(EoS,P,T,mix,guess_x,guess_y,guess_beta)
%
%Given the global composition of the mixture mix.x and pressure and
%temperature P and T, performs a flash calculation and determines the
%vaporized fraction and the compositions of liquid and gas in equilibrium
%
%Parameters:
%EoS: EoS used for calculations
%P: Pressure (Pa)
%T: Temperature (K)
%mix: mixture (cMixture object)
%   mix.x must contain the global composition of the mixture
%guess_x: initial guess for liquid composition (molar fraction vector)
%guess_y: initial guess for gas composition (molar fraction vector)
%guess_beta: initial guess for vaporized fraction
%
%Results:
%beta: vaporized fraction V/(V+L) (mol/mol)
%x: liquid composition (molar fraction vector)
%y: gas composition (molar fraction vector)
%K: distribution coefficients K = y/x
%val: final value of the objective function 
%time: time required for calculations
%EoS: returns EoS used for calculations

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

t = cputime; %start timer

%Defines liquid and gas
Liquid = mix;
Liquid.x = guess_x;
Gas = mix;
Gas.x = guess_y;
z = mix.x;

%Options of fsolve for resolution of Rachford-Rice equation
if length(ver('MATLAB')) == 1
	OPTIONS = optimset('Display','off','TolX',1e-5,'TolFun',1e-5,'MaxIter',100,'MaxFunEvals',100);
else
	OPTIONS = optimset('TolX',1e-5,'TolFun',1e-5,'MaxIter',100,'MaxFunEvals',100); %For compatibility with octave which does not support 'Display' option
end

%Tolerance of calculations
tol = 1e-5; 
val = 1;
iter = 0;
iter_max = 100;

beta = guess_beta;

while val > tol && iter < iter_max
    iter = iter + 1;

    yant = guess_y;
    xant = guess_x;

    %Liquid and gas fugacities
    fiL = fug(EoS,T,P,Liquid,'liq');
    fiG = fug(EoS,T,P,Gas,'gas');

    %Distribution coefficients
    K = zeros(1,mix.numC);
    for i = 1:mix.numC
        K(i) = fiL(i)/fiG(i);
    end

    %Solves Rachford-Rice
    [beta val flag] = fsolve(@(beta) RachfordRice(beta,mix.x,K,mix.numC,EoS),beta,OPTIONS);

    if beta < 0 %corrects beta<0 or beta>1
        beta = 0.000001;
    elseif beta > 1
        beta = 0.999999;
    end

    %Calculates liquid and gas compositions
    for i = 1:mix.numC
        guess_x(i) = z(i)/(1+beta*(K(i)-1));
        guess_y(i) = K(i)*Liquid.x(i);
    end
    Liquid.x = guess_x./sum(guess_x);
    Gas.x = guess_y./sum(guess_y);

    %Calculates difference between calculated compositions in successive
    %iterations
    val = 0;
    for i = 1:mix.numC
        val= val + abs((guess_y(i) - yant(i))) + abs((guess_x(i) - xant(i)));
    end

end

if (iter == iter_max)%Checks if number of iterations was exceeded, displays warning if so
    s = sprintf('%f',val);
    warning('MATLAB:EoS',['Convergence error in Flash. Final value of objective function: ' s '.']);
end

if(beta < 1e-4) || (abs(beta-1) < 1e-4) %Checks if calculations converged to single phase, displays warning if so
    warning('MATLAB:EoS','Single phase region in Flash')
end

%Return results
x = Liquid.x;
y = Gas.x;

time = cputime - t; %evaluates time required for calculations