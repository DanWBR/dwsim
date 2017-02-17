function [S,val,time,flag,EoS] = Solubility(EoS,mix,T,P,varargin)
%function [S,val,time,flag,EoS] = Solubility(EoS,mix,T,P,[guess_S])
%
%Calculates the solid-fluid equilibrium of mixture mix at temperature T and
%pressure P with the Equation of State EoS
%
%Parameters:
%EoS: Equation of State used for calculations (cEOS object)
%mix: mixture (cMixture object). Parameter mix.x or mox.molFrac  must contain 
%   global composition of the mixture
%T: Temperature (K)
%P: Pressure (Pa)
%
%Optional parameters:
%guess_S: estimation of solubility, mol fraction vector
%   (default value: global mixture composition)
%
%Results:
%S: composition of the fluid phase (molar fraction)
%val: final value of the objective function obj_BubblePointP 
%time: time required for calculations
%flag: = 1 if calculations finished correctly, = 0 if warnings appeared
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

sol_comp = zeros(mix.numC,1);
sol_comp(mix.numC) = 1;

flag = 1;

if nargin > 4
    if isempty(varargin{1})
        guess_S = mix.x;
    else
        guess_S = varargin{1};
    end
else
    guess_S = mix.x;
end

[beta, thita, x, K, liquid, val, time] = MultiFlash(EoS,P,T,mix,[guess_S; sol_comp'],[0.95 0.05],{'gas','sol'});

for i = 1:2
    if strcmp(liquid(i),'sol') == 1
        indx = i;
    end
    if strcmp(liquid(i),'gas') == 1
        indxF = i;
    end
end
if beta(indx) == 0
    warning('MATLAB:EoS', 'Solid phase not formed in Solubility calculations. Results will not be reliable.');
    flag = 0;
end

S = x(indxF,:);