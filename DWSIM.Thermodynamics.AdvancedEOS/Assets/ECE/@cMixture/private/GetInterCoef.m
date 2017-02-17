function res = GetInterCoef(this,index)
%Reads the interaction coefficient matrix
%(Private function, not to be used directly)

%Copyright (c) 2011 �ngel Mart�n, University of Valladolid (Spain)
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

s = index(1).subs;
if length(s) > 1 %handles calls of the type .ki(j,k)
    if length(s) > 2
        error (['Syntax error ' index(1).subs '. Allowed syntax: .ki, where i ranges from 0 to 9']);    
    end
    k = double(s(2)) - 48;
    if k < 0 || k > 9
        error (['Syntax error ' index(1).subs '. Allowed syntax: .ki, where i ranges from 0 to 9']);    
    end
    if length(index) == 1
        index(2).type = '()';
        index(2).subs(1) = {k};
    else
        if length(index(2).subs) == 2
            index(2).subs(3) = {k};
        else
            error (['Syntax error ' index(1).subs '. Allowed syntax: .ki(j,k), where i ranges from 0 to 9'])    
        end
    end
end

NumC = length(this.mComponents);
Dim_mk = size(this.mk);
if length (Dim_mk) < 3
    Dim_mk(3) = 1;
end
NumT = Dim_mk(3);

if length(index) == 1 %handles calls of the type .k, returns whole interaction coefficient matrix
    res = zeros(NumC,NumC,NumT);
    for k = 1:NumT
        for i = 1:NumC
            for j = 1:NumC
                if (i > Dim_mk(1) || j > Dim_mk(2) || k > Dim_mk(3) ) %Returns interaction coefficient (defaults to zero if not defined)
                    res(i,j,k) = 0;
                else
                    if isempty(this.mk{i,j,k})
                        res(i,j,k) = 0;
                    else
                        try
                            res(i,j,k) = evalin('caller',this.mk{i,j,k});
                        catch
                            res(i,j,k) = 0;
                        end                  
                    end
                end
            end
        end
    end
else
    switch index(2).type
        case '()'
            if length(index(2).subs) == 1 %handles calls of the type .k(i), returns whole interaction coefficient table ki
                k = index(2).subs{1};
                def_k = round(k); %Checks k is a possitive integer
                if ((k<1) || abs(def_k - k) > 1e-3)
                    error ('Indexes of interaction coefficient matrix must be positive integers');
                end

                res = zeros(NumC,NumC);
                for i = 1:NumC
                    for j = 1:NumC
                        if (i > Dim_mk(1) || j > Dim_mk(2) || k > Dim_mk(3) ) %Returns interaction coefficient (defaults to zero if not defined)
                            res(i,j) = 0;
                        else
                            if isempty(this.mk{i,j,k})
                                res(i,j) = 0;
                            else
                                try
                                    res(i,j) = evalin('caller',this.mk{i,j,k});
                                catch
                                    res(i,j) = 0;
                                end
                            end

                        end
                    end
                end

            elseif length(index(2).subs) == 2 %handles calls of the type .k(i,j)
                i = index(2).subs{1};
                j = index(2).subs{2};

                def_i = round(i); %Checks i,j are possitive integers
                if ((i<1) || abs(def_i - i) > 1e-3)
                    error ('Indexes of interaction coefficient matrix must be positive integers');
                end
                def_j = round(j); %Checks i,j are possitive integers
                if ((j<1) || abs(def_j - j) > 1e-3)
                    error ('Indexes of interaction coefficient matrix must be positive integers');
                end

                if (i > NumC || j > NumC) %Checks if i,j agree with the number of components
                    error ('Index exceeds number of components');
                end

                if (i > Dim_mk(1) || j > Dim_mk(2)) %Returns interaction coefficient (defaults to zero if not defined)
                    res = 0;
                else
                    try
                        res = evalin('caller',this.mk{i,j});
                    catch
                        res = 0;
                    end
                end
            elseif length(index(2).subs) == 3 %handles calls of the type .k(i,j,k)
                i = index(2).subs{1};
                j = index(2).subs{2};
                k = index(2).subs{3};

                def_i = round(i); %Checks i,j,k are possitive integers
                if ((i<1) || abs(def_i - i) > 1e-3)
                    error ('Indexes of interaction coefficient matrix must be positive integers');
                end
                def_j = round(j); %Checks i,j,k are possitive integers
                if ((j<1) || abs(def_j - j) > 1e-3)
                    error ('Indexes of interaction coefficient matrix must be positive integers');
                end
                def_k = round(k); %Checks i,j,k are possitive integers
                if ((k<1) || abs(def_k - k) > 1e-3)
                    error ('Indexes of interaction coefficient matrix must be positive integers');
                end

                if (i > NumC || j > NumC) %Checks if i,j agree with the number of components
                    error ('Index exceeds number of components');
                end

                if (i > Dim_mk(1) || j > Dim_mk(2) || k > Dim_mk(3) ) %Returns interaction coefficient (defaults to zero if not defined)
                    res = 0;
                else
                    if isempty (this.mk{i,j,k})
                        res = 0;
                    else
                        try
                            res = evalin('caller',this.mk{i,j,k});
                        catch
                            res = 0;
                        end
                    end
                end
            else
                error ('Call to k with incorrect number of indexes');
            end

        otherwise
            error (['Unexpected operator ' index(2).type]);
    end
end
