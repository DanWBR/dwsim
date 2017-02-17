function [T, flag, val, time, EoS] = BoilingTemp(EoS,comp,P,varargin)
%function [T, flag, val, time, EoS] = BoilingTemp(EoS,comp,P,[guess_T],[options])
%
%Calculates the boiling temperature of component 'comp' at pressure P with the
%equation of state 'EoS'
%
%Parameters:
%EoS: Equation of State used for calculations (cEOS object)
%comp: Component (cSubstance object)
%P: Pressure (Pa)
%
%Optional parameters:
%guess_T: initial guess of boiling point (K). If it is not provided, the
%   initial guess is obtained using the Antoine equation using the 'Ant'
%   parameters of the component
%options: parameters of the fzero numerical resolution method (structure
%   generated with "optimset")
%
%Results:
%T: Boiling temperature (K)
%flag: flag returned by the fzero numerical resolution method:
%     1  FZERO found a zero X.
%     2  Calculations converged to single phase region
%     -1  Algorithm terminated by output function.
%     -3  NaN or Inf function value encountered during search for an interval
%          containing a sign change.
%     -4  Complex function value encountered during search for an interval 
%          containing a sign change.
%     -5  FZERO may have converged to a singular point.
%val: final value of the objective function obj_Pvap
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

%sets guess_T parameter
if nargin < 4
    guess_T = (comp.AntB - (comp.AntA-log(P))*comp.AntC)/(comp.AntA-log(P));
else
    if isempty(varargin{1})
        guess_T = (comp.AntB - (comp.AntA-log(P))*comp.AntC)/(comp.AntA-log(P));
    else
        guess_T = varargin{1};
    end
end

%Sets options vector
if nargin < 5
    options = optimset('TolX',1e-6,'TolFun',1e-6,'MaxIter',50,'MaxFunEvals',50); %Default options of fzero    
else
    options = varargin{2};
end

%Calculates the vapor pressure
[T val flag] = fzero(@(T) obj_BTemp(T,EoS,comp,P,1),guess_T,options);
[val flag] = obj_BTemp(T,EoS,comp,P,flag);
if flag == 2
    warning('MATLAB:EoS','Single phase region in BoilingTemperature');
end

time = cputime - t; %evaluates time required for calculations
