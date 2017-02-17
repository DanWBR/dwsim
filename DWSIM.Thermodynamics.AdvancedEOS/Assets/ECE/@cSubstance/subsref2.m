function varargout = subsref(this, index)
%Accessor for cSubstance class

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

if strcmp(index(1).type,'.') == 1
        if strcmp(index(1).subs, 'name' ) == 1
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mName;
                    varargout = {varargout};
                end
        elseif strcmp(index(1).subs, 'MW' ) == 1
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mMW;
                    varargout = {varargout};
                end
        elseif strcmp(index(1).subs, 'Tc' ) == 1
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mTc;
                    varargout = {varargout};
                end
        elseif strcmp(index(1).subs, 'Pc' ) == 1
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mPc;
                    varargout = {varargout};
                end
        elseif strcmp(index(1).subs, 'w' ) == 1
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mw;
                    varargout = {varargout};
                end
        elseif strcmp(index(1).subs, 'AntA' ) == 1
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mAntA;
                    varargout = {varargout};
                end
        elseif strcmp(index(1).subs, 'AntB' ) == 1
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mAntB;
                    varargout = {varargout};
                end
            case 'AntC'
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mAntC;
                    varargout = {varargout};
                end
            case 'Tf'
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mTf;
                    varargout = {varargout};
                end
            case 'Hf'
                if isempty (this)
                    varargout = {};
                else
                    varargout = this.mHf;
                    varargout = {varargout};
                end
            case 'EoSParam'
                if isempty (this)
                    varargout = {};
                else
                    if length(index) == 1 %handles sentences of the type component.EoSParam
                        try
							varargout = this.mEoSParam{1};
						catch
							varargout = {0}; %Returns 0 if undefined
						end
                    elseif length(index) == 2 %handles sentences of the type component.EoSParam(i)
                        if strcmp(index(2).type,'()') == 1 
                            indx = index(2).subs;
                            indx = indx{1};
                            if indx <= length(this.mEoSParam)
                                if isempty (this.mEoSParam{indx})
                                    varargout = {0};
                                else
                                    varargout = this.mEoSParam{indx};
                                end
                            else
                                varargout = {0};
                            end
                        else
                            error (['Unexpected index.type of ' index(2).type]);  
                        end
                    else
                        error (['Unexpected index.type of ' index(3).type]);    
                    end
                end 
                
            otherwise
                error(['Reference to non-existent field ' index(1).subs '.']);
        end
        
elseif strcmp(index(1).type,'.') == 1
        this_subset = this(index(1).subs{:});

        if length(index) == 1
            varargout = {this_subset};
        else
            varargout = cell(size(this_subset));
            [varargout{:}] = subsref(this_subset, index(2:end));
        end
        
else
        error (['Unexpected index.type of ' index(1).type]);
              
end