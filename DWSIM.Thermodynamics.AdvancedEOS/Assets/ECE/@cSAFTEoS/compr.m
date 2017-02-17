function [Z EoS] = compr(EoS,T,P,mix,phase,varargin)
%Calculates the compressibility coefficient of mixture mix at temperature T
%and pressure P using SAFT EoS
%
%Parameters:
%EoS: Equation of state used for calculations
%T: Temperature(K)
%P: Pressure (Pa)
%mix: cMixture object
%phase: set  phase = 'liq' to get the coefficient of the liquid phase, phase = 'gas'  
%   to get the coefficient of the gas phase 
%
%Optional parameters (set [] to keep default value)
%Z_ini: Initial guess for the compressibility coefficient
%   If not defined, the program uses an initial guess Z_ini = 0.8 for gas
%   phase and a Z_ini corresponding to a liquid density of 800 kg/m3 for
%   the liquid phase
%options: parameters of the fsolve numerical resolution method (structure
%   generated with "optimset")
%
%Results:
%Z: compresibility coefficient
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

%**************************************************************************
%Initial guess
%**************************************************************************

if (nargin == 5) || (nargin > 5 && isempty(varargin{1})) 
    if strcmp(phase,'gas') == 1
        ro = P/(0.8*8.31*T)*1e-30*6.022e23; %molecule/A3
        sumat = 0;
        for i = 1:mix.numC
            sumat = sumat + mix.x(i)*mix.comp(i).EoSParam(1)*(mix.comp(i).EoSParam(2))^3;
        end
        ini = pi/6*ro*sumat;
    elseif strcmp(phase, 'liq') == 1
        ro = 800*1000/mix.MW*1e-30*6.022e23; %molecule/A3
        sumat = 0;
        for i = 1:mix.numC
            sumat = sumat + mix.x(i)*mix.comp(i).EoSParam(1)*(mix.comp(i).EoSParam(2))^3;
        end
        ini = pi/6*ro*sumat;
    else
        error('Undefined type of phase in SAFTEoS');
    end
else
    Zini = varargin{1};
    ro = P/(Zini*8.31*T)*1e-30*6.022e23; %molecule/A3
    sumat = 0;
    for i = 1:mix.numC
        sumat = sumat + mix.x(i)*mix.comp(i).EoSParam(1)*(mix.comp(i).EoSParam(2))^3;
    end
    ini = pi/6*ro*sumat;
end


%**************************************************************************
%Calculates Z and mu with SAFT
%**************************************************************************
if nargin == 7
    OPTIONS = varargin{2};
else
	if length(ver('MATLAB')) == 1
		OPTIONS = optimset('Display','off','TolX',1e-6,'TolFun',1e-6,'MaxIter',50,'MaxFunEvals',50);
	else
		OPTIONS = optimset('TolX',1e-6,'TolFun',1e-6,'MaxIter',50,'MaxFunEvals',50);  %For compatibility with octave which does not support 'Display' option
	end
end

[dens_red val flag] = fsolve(@(dens_red) obj_SAFT(dens_red,EoS,T,P,mix), ini, OPTIONS);
[result Z] = obj_SAFT(dens_red,EoS,T,P,mix);

if flag  < 1%displays a warning if calculations did not converge
    warning('MATLAB:EoS','Convergence error in calculation of Z with SAFT. fsolve error code: %d / Final value of objective function: %f',flag, val);
end
