function [beta, thita, x, K, liquid, val, time, EoS] = MultiFlash(EoS,P,T,mix,guess_x,guess_beta,liquid,varargin)
%function [beta, thita, x, K, phase, val, time, EoS] = MultiFlash(EoS,P,T,mix,guess_x,guess_beta,phase)
%
%Given the global composition of the mixture mix.x and pressure and
%temperature P and T, and a set of possible phases defined by guess_x,
%guess_beta and liquid, performs a multiphase calculation to determine
%which phases are stable, phase fractions and phase compositions
%
%Parameters:
%EoS: EoS used for calculations
%P: Pressure (Pa)
%T: Temperature (K)
%mix: mixture (cMixture object)
%   mix.x must contain the global composition of the mixture
%guess_x: initial guess for phase composition composition 
%   (molar fraction matrix, with dimensions number of phases*number of components) 
%guess_beta: initial guess for phase fraction, vector length number of phases
%phase: type of phase, cell vector with elements 'liq' for liquid phases
%   and 'gas' for gas phases (example: {'liq' 'liq' 'gas'})
%
%Results:
%beta: calculated phase fraction, vector length number of phases
%thita: calculated phase stability, vector length number of phases
%x: calculated phase composition (molar fraction matrix, with dimensions
%   number of phases*number of components) 
%K: distribution coefficients, matrix with dimensions number of phases*number of components
%   K(phase i, component j) = x(phase i, component j)/x(phase 1, component j)
%phase: type of phase, cell vector with elements 'liq' for liquid phases
%   and 'gas' for gas phases (order in returned liquid may differ from
%   original order in parameter liquid)
%val: final value of the objective function 
%time: time required for calculations
%EoS: returns EoS used for calculations
%
%Reference: Gupta et al., Fluid Phase Equilibr. 63 (1991) 65-89

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

NumP = length(guess_beta); %Number of phases

Xini = guess_beta(2:NumP); 
for i = 2:NumP
    if Xini(i-1) == 0
        Xini(i-1) = -1;
    end
end

%Options for fsolve of obj_FlashMulti
if length(ver('MATLAB')) == 1
	OPTIONS = optimset('Display','off','MaxIter',200,'MaxFunEvals',200,'TolX',1e-9,'TolFun',1e-6); 
else
	OPTIONS = optimset('MaxIter',200,'MaxFunEvals',200,'TolX',1e-9,'TolFun',1e-6);  %For compatibility with octave which does not support 'Display' option
end
phase = mix;

%Tolerance and maximum number of iterations
tol = 1e-4;
val = 1;
iter = 0;
iter_max = 20;
epsilon = 1e-6; %Rounding value of beta and thita

while ((val > tol) && (iter < iter_max)) %Continues iterations until the difference between calculated gas compositions 
                                         %of two successive iterations is lower than tol, or until the maximum number 
                                         %of iterations is reached     

    iter = iter + 1;
    
    guess_x_old = guess_x;
    
    %Fugacities
    fi = zeros(NumP, mix.numC);
    for j = 1:NumP
        phase.x = guess_x(j,:);
        f = fug(EoS,T,P,phase,liquid(j));
        for i = 1:mix.numC
            fi(j,i) = f(i);
        end
    end

    %Distribution coefficients
    K = zeros(NumP, mix.numC);
    for j = 1:NumP
        for i = 1:mix.numC
            K(j,i) = fi(1,i)/fi(j,i); 
        end
    end

    %Objective function minimization
    X = fsolve(@(X) obj_FlashMulti(X,mix.x,K,NumP,mix.numC,EoS),Xini,OPTIONS);
    
    %Transform results into phase fraction and stability values
    beta = zeros(NumP,1)';
    thita = zeros(NumP,1)';
    
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

    %Builds the Xini vector for next iteration of fsolve
    for i = 2:NumP
        if thita(i) > 0
            Xini(i-1) = -thita(i); 
        else
            Xini(i-1) = beta(i);
        end
    end

    %Updating of compositions
    for i = 1:mix.numC
        sumat = 0;
        for j = 2:NumP
            sumat = sumat + (K(j,i)*exp(thita(j)) - 1)*beta(j); %Composition of reference phase
        end
        guess_x(1,i) = mix.x(i)/(1+sumat);
    end
    
    for i = 1:mix.numC
        for j = 2:NumP
            guess_x(j,i) = guess_x(1,i)*K(j,i)*exp(thita(j)); %Compositions of remaining phases
        end
    end
    
    for j = 1:NumP
        sumat = sum(abs(guess_x(j,:)));
        for i = 1:mix.numC
            guess_x(j,i) = abs(guess_x(j,i))/sumat; %Normalization of compositions
        end
    end

    %Evaluates difference in results in successive iterations
    val = 0;
    for j = 1:NumP
        for i = 1:mix.numC
            val = val + abs(guess_x(j,i) - guess_x_old(j,i));
        end
    end   
    
    x = guess_x; %Returns calculated compositions
end

if (iter == iter_max)
    s = sprintf('%f',val);
    warning('MATLAB:EoS', ['Convergence error in MultiFlash. Final value of objective function: ' s '.']);
end

if (val>tol) && (nargin == 7) %shifts the order of phases if neccesary
    shift = 1;
    while (val>tol) && (shift < NumP)
        for j = 1:NumP
            j_shift = j + shift;
            if j_shift > NumP
                j_shift = j_shift - NumP;
            end
            guess_beta_shift(j_shift) = guess_beta(j);
            liquid_shift(j_shift) = liquid(j);
            for i = 1:mix.numC
                guess_x_shift(j_shift,i) = guess_x(j,i);
            end
        end
        
        [beta_shift, thita_shift, x_shift, K_shift,liquid_shift, val_shift, time_shift, EoS] = MultiFlash(EoS,P,T,mix,guess_x_shift,guess_beta_shift,liquid_shift,1);
        
        shift = shift + 1;
    end
    if val_shift < tol 
        beta = beta_shift;
        thita = thita_shift;
        x = x_shift;
        K = K_shift;
        liquid = liquid_shift;
        val = val_shift;
    end
end

%Merges multiple occurencies of identical phases
for j = 1:NumP-1
    if thita(j) == 0
        for k = j+1:NumP
            if (abs(K(k,1) - K(j,1)) < 1e-3)
                beta(j) = beta(j) + beta(k);
                beta(k) = 0;
            end
        end
    end
end

%Transforms K into format used by other programs
for j = 1:NumP
    for i = 1:mix.numC
        K(j,i) = 1/K(j,i);
    end
end

time = cputime - t; %evaluates time required for calculations 