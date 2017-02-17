function this = subsasgn (this, index, varargin)
%Mutator for cSubstance class

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

switch index(1).type
    
    case '.'
        switch index(1).subs %Checks consistence of input data
            case {'name'}
                v = varargin{1};
                if (~(ischar(v)))
                    error(['Input parameter ' index(1).subs ' must be a string']);
                end
            case {'MW','Tc','Pc','Tf','Hf'}     
                v = varargin{1};

                if (size(v,1) ~= 1) || (size(v,2) ~= 1)
                    error(['Input parameter ' index(1).subs ' must be a scalar']);
                end
                if v < 0
                    warning('MATLAB:EoS',['Input parameter ' index(1).subs ' should be positive']);
                end
                if (~isreal(v))
                    error(['Input parameter ' index(1).subs ' must be a real number']);
                end
            case {'AntA', 'AntB', 'AntC', 'w'}
                v = varargin{1};

                if (size(v,1) ~= 1) || (size(v,2) ~= 1)
                    error(['Input parameter ' index(1).subs ' must be a scalar']);
                end
                if (~isreal(v))
                    error(['Input parameter ' index(1).subs ' must be a real number']);
                end
        end
        switch index(1).subs
            case 'name'
                this.mName = varargin{1};
            case 'MW'
                this.mMW = varargin{1};
            case 'Tc'
                this.mTc = varargin{1};
            case 'Pc'
                this.mPc = varargin{1};
            case 'w'
                this.mw = varargin{1};
            case 'AntA'
                this.mAntA = varargin{1};
            case 'AntB'
                this.mAntB = varargin{1};
            case 'AntC'
                this.mAntC = varargin{1};
           case 'Tf'
                this.mTf = varargin{1};
           case 'Hf'
                this.mHf = varargin{1};
            case 'EoSParam'
                if length(index) == 1 %handles sentences of the type component.EoSParam
                    this.mEoSParam{1} = varargin(1); 
                elseif length(index) == 2 %handles sentences of the type component.EoSParam(i)
                    if strcmp(index(2).type,'()')==1
                        indx = index(2).subs;
                        indx = indx{1};
                        this.mEoSParam{indx} = varargin(1); 
                    else
                        error (['Unexpected index.type of ' index(2).type]);
                    end
                else
                    error (['Unexpected index.type of ' index(3).type]);
                end
                
            otherwise
                error(['Reference to non-existent field ' index(1).subs '.']);
        end
        
    case '()'
        if isempty(this)
            this = cSubstance;
        end
        if length(index) == 1
            this = builtin('subsasgn', this, index, varargin{end:-1:1});
        else
            this_subset = this(index(1).subs{:});
            this_subset = subsasgn (this_subset, index(2:end), varargin{end:-1:1});
            this(index(1).subs{:}) = this_subset;
        end
end

       