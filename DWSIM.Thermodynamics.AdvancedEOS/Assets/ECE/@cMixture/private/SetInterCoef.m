function this = SetInterCoef(this,index,varargin)
%Assigns interaction coefficients
%Private function, not to be used directly
%Use normal assignment syntax instead (e.g. mix.k =...)

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

if length(index) == 1 %handles calls of the type .k, sets whole interaction coefficient matrix
    val = varargin{1};
    val = val{1};
    s_i = size(val,1);
    s_j = size(val,2);
    s_k = size(val,3);

    NumC = length(this.mComponents);

    if ((s_i) ~= NumC ) || ((s_j) ~= NumC ) %Checks k is a square NumC*NumC matrix
        error ('Dimension mismatch - k must be a square NumC*NumC matrix');
    end

    for i = 1:NumC
        for j = 1:NumC
            for k = 1:s_k
                if ischar(val(i,j,k)) == 0
                    strng = num2str(val(i,j,k),10);%sprintf('%f',val(i,j,k));
                else
                    strng = val(i,j,k);
                end
                this.mk(i,j,k) = cellstr(strng);
            end
        end
    end
else
    if strcmp(index(2).type,'()') == 1
        if length(index(2).subs) == 1 %handles calls of the type .k(i), sets whole interaction coefficient table ki
            k = index(2).subs{1};
            def_k = round(k); %Checks k is a possitive integer
            if ((k<1) || abs(def_k - k) > 1e-3)
                error ('Indexes of interaction coefficient matrix must be positive integers');
            end
            val = varargin{1};
            val = val{1};
            s_i = size(val,1);
            s_j = size(val,2);

            NumC = length(this.mComponents);

            if ((s_i) ~= NumC ) || ((s_j) ~= NumC ) %Checks k is a square NumC*NumC matrix
                error ('Dimension mismatch - k must be a square NumC*NumC matrix');
            end

            for i = 1:NumC
                for j = 1:NumC
                    if ischar(val(i,j)) == 0
                        strng = num2str(val(i,j),10);%sprintf('%f',val(i,j));
                    else
                        strng = val(i,j);
                    end
                    this.mk(i,j,k) = cellstr(strng);
                end
            end

        elseif length(index(2).subs) == 2 %handles calls of the type .k(i,j)
            NumC = length(this.mComponents);

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

            val = varargin{1};
            if ischar(val{1}) == 0
                strng = num2str(val{1},10);%sprintf('%f',val{1});
            else
                strng = val{1};
            end
            this.mk(i,j) = cellstr(strng);

        elseif length(index(2).subs) == 3 %handles calls of the type .k(i,j,k)
            NumC = length(this.mComponents);

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

            val = varargin{1};
            if ischar(val{1}) == 0
                strng = num2str(val{1},10);%sprintf('%f',val{1});
            else
                strng = val{1};
            end
            this.mk(i,j,k) = cellstr(strng);
        else
            error ('Call to k with incorrect number of indexes');
        end
    else
        error (['Unexpected operator ' index(2).type]);
    end
end
