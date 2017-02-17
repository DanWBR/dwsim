function [P,x,y,handle,EoS] = PxyDiagram(EoS,mix,T,guess_P,guess_y,varargin)
%function [P,x,y,handle,EoS] = PxyDiagram(EoS,mix,T,guess_P,guess_y,[numP], [interval], [diagnostics])
%
%Calculates and draws a pressure-composition vapor-liquid Pxy diagram of 
%a binary system at a constant temperature T
%
%Parameters:
%EoS: Equation of State used for calculations
%mix: mixture (cMixture object). 
%T: Temperature (K)
%guess_P: initial guess of pressure for the first point of the diagram (Pa)
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
%P: Calculated pressures (Pa)
%x: Calculated liquid compositions (molar fraction)
%y: Calculated gas compositions (molar fraction)
%handle: Handle of the Pxy diagram 
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
    error('PxyDiagram is only available for binary mixtures');
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

P = [];
x = [];
y = [];

%Calculates diagram
counter = 0;
for xi = interval(1):incr:interval(2)
    counter = counter + 1;
    mix.x = [xi 1-xi];
  
	if (mix.x(1) == 0) %Pure component vapor pressure
		[Pi, flag, val, time] = VaporPressure(EoS,mix.comp(2),T,guess_P);
		yi = [0 1];
	elseif (mix.x(1) == 1) %Pure component vapor pressure
		[Pi, flag, val, time] = VaporPressure(EoS,mix.comp(1),T,guess_P);
		yi = [1 0];
	else %Mixture bubble point
		[Ti,Pi,yi,K,flag,val,time] = BubblePoint(EoS,mix,T,guess_P,guess_y,'P');
	end

    if display == 1
        s = sprintf('%d/%d : P = %f Pa | x = [%f , %f] | y = [%f, %f] | time = %f s',counter,numP,Pi,xi,1-xi,yi(1),yi(2),time);
        disp(s);
    end
    
    if (flag == 1) 
        P = [P; Pi];
        x = [x; [xi 1-xi]];
        y = [y; yi];
        
        guess_P = Pi;
        guess_y = yi;
    end
end

%Generates the figure
handle = figure;
plot(x(:,1),P,'-b');
hold on;
plot(y(:,1),P,'-b');
s = ['x,y of ',mix.comp(1).name, ' (molar fraction)'];
xlabel(s);
ylabel('P (Pa)')
s1 = sprintf('%.1f',T);
s = ['Pxy diagram of ',mix.comp(1).name,' + ',mix.comp(2).name, ' at ',s1,' K'];
title(s);
hold off;
