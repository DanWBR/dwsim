function [T,x,y,handle,EoS] = TxyDiagram(EoS,mix,guess_T,P,guess_y,varargin)
%[T,x,y,handle,EoS] = TxyDiagram(EoS,mix,guess_T,P,guess_y,[numP],[interval],[diagnostics])
%
%Calculates and draws a temperature-composition vapor-liquid Txy diagram of 
%a binary system at a constant pressure P
%
%Parameters:
%EoS: Equation of State used for calculations
%mix: mixture (cMixture object)
%guess_T: initial guess of temperature for the first point of the diagram (K)
%P: Pressure (Pa)
%guess_y: initial guess of gas composition for the first point of the
%   diagram (molar fraction)
%
%Optional parameters (set [] to keep default value)
%numP: number of points (default value 20)
%interval: interval of calculation, vector [mol_frac_ini, mol_frac_end],
%   molar fractions referred to first component of mixture 
%   (default value [0 1])
%diagnostics: set diagnostics = 1 to get information after the calculation
%   of each point of the diagram (default value = 0)
%
%Results:
%T: Calculated temperatures (K)
%x: Calculated liquid compositions (molar fraction)
%y: Calculated gas compositions (molar fraction)
%handle: Handle of the Txy diagram 
%EoS: Returns EoS used for calculations

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

%Checks the number of components
if mix.numC ~= 2
    error('TxyDiagram is only available for binary mixtures');
end

%Sets numP if undefined
if nargin < 6
    numP = 20;
else
    if isempty(varargin{1}) == 1
        numP = 20;
    else
        numP = varargin{1};
    end
end

%Sets interval if undefined
if nargin < 7
    interval = [0 1];
else
    if isempty(varargin{2}) == 1
        interval = [0 1];
    else
        interval = varargin{2};
    end
end

%Sets diagnostics if undefined
if nargin < 8
    display = 0;
else
    display = varargin{3};
end

incr = (interval(2) - interval(1))/(numP - 1);

T = [];
x = [];
y = [];

%Calculates diagram
counter = 0;
for xi = interval(1):incr:interval(2)
    counter = counter + 1;
    mix.x = [xi 1-xi];
    
    if (mix.x(1) == 0) %Pure component vapor pressure
        [Ti, flag, val, time] = BoilingTemp(EoS,mix.comp(2),P,guess_T);
        yi = [0 1];
    elseif (mix.x(1) == 1) %Pure component vapor pressure
        [Ti, flag, val, time] = BoilingTemp(EoS,mix.comp(1),P,guess_T);
        yi = [1 0];
    else %Mixture bubble point
        [Ti,Pi,yi,K,flag,val,time] = BubblePoint(EoS,mix,guess_T,P,guess_y,'T');
    end

    if display == 1
        s = sprintf('%d/%d : T = %f K | x = [%f , %f] | y = [%f, %f] | time = %f s',counter,numP,Ti,xi,1-xi,yi(1),yi(2),time);
        disp(s);
    end
    
    if (flag == 1) 
        T = [T; Ti];
        x = [x; [xi 1-xi]];
        y = [y; yi];
        
        guess_T = Ti;
        guess_y = yi;
    end
end

%Generates the figure
handle = figure;
plot(x(:,1),T,'-b');
hold on;
plot(y(:,1),T,'-b');
s = ['x,y of ',mix.comp(1).name, ' (molar fraction)'];
xlabel(s);
s1 = sprintf('%d',P);
s = ['Txy diagram of ',mix.comp(1).name,' + ',mix.comp(2).name, ' at ',s1,' Pa'];
title(s)
ylabel('T (K)')
hold off;
