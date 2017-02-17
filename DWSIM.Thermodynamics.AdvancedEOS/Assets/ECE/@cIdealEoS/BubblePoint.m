function [T,P,y,K,flag,val,time,EoS] = BubblePoint(EoS,mix,T,P,guess_y,type,varargin)
%function [T,P,y,K,flag,val,time,EoS] = BubblePoint(EoS,mix,T,P,guess_y,type,[options])
%
%Given the composition of the liquid mix.x and the temperature T or pressure P,
%calculates P or T at which the first bubble of gas is formed (bubble
%point pressure or temperature), and the composition of this gas y
%
%Parameters:
%EoS: Equation of State used for calculations (cEOS object)
%mix: mixture (cMixture object). Parameter mix.x or mox.molFrac  must contain 
%   composition of the liquid phase
%T: Temperature (K) (initial guess if type = 'T')
%P: Pressure (Pa) (initial guess if type = 'P')
%guess_y: Initial guess for composition of gas phase (molar fraction vector)
%type: type of calculation
%   if type = 'T', then the bubble T is calculated for a given P
%   if type = 'P', then the bubble P is calculated for a given T
%
%Optional parameters:
%options: parameters of the fzero numerical resolution method (structure
%   generated with "optimset")
%
%Results:
%T: Bubble point temperature (K)
%P: Bubble point pressure (Pa)
%y: Composition of the gas phase (molar fraction vector)
%K: Distribution coefficients, K = y/x
%flag: flag returned by the fzero numerical resolution method:
%     1  FZERO found a zero X.
%     2  Calculations converged to single phase region
%     -1  Algorithm terminated by output function.
%     -3  NaN or Inf function value encountered during search for an interval
%          containing a sign change.
%     -4  Complex function value encountered during search for an interval 
%          containing a sign change.
%     -5  FZERO may have converged to a singular point.
%val: final value of the objective function obj_BubblePointP 
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

%sets options parameter
if nargin == 6
    options = optimset('TolX',1e-6,'TolFun',1e-6,'MaxIter',20,'MaxFunEvals',20); %Default options of fzero
else
    options = varargin{1};
end

try
	if strcmp(type,'T') == 1
		[T val flag] = fzero(@(T) obj_BubblePoint(T,P,guess_y,EoS,mix,type,1),T,options); %Calculates the bubble point temperature
		[val,y,K,flag] = obj_BubblePoint(T,P,guess_y,EoS,mix,type,flag); %Calculates the composition of the gas
	elseif strcmp(type,'P') == 1
		[P val flag] = fzero(@(P) obj_BubblePoint(P,T,guess_y,EoS,mix,type,1),P,options); %Calculates the bubble point pressure
		[val,y,K,flag] = obj_BubblePoint(P,T,guess_y,EoS,mix,type,flag); %Calculates the composition of the gas
	else
		error(['The value "' type '" of "type" parameter is incorrect.']);
	end
catch
	disp('Error in calculations in BubblePoint');
	y = guess_y;
	K = ones(mix.numC, mix.numC);
	flag = -1;
	val = 0;
	time = 0;
end

if flag == 2
    warning('MATLAB:EoS','Single phase region in BubblePoint');
end

time = cputime - t; %evaluates time required for calculations
