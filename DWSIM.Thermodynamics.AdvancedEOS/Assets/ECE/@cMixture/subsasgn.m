function this = subsasgn (this, index, varargin)
%Mutator for cMixture class

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

try
    T = evalin('caller','T'); %Necessary for temperature-dependant interaction coefficients
catch
    
end

switch index(1).type
    
    case '.'
        switch index(1).subs
            case 'x' %Molar fraction
                if length(index) == 2 %Handles sentences of the type mixture.x(i)
                    indx = index(2).subs;
                    indx = indx{1};    
                    if indx > length(this.mComponents)
                        error('Dimensions mismatch between molar fraction vector and component vector in cMixture');
                    else
                        this.mMolFrac(indx) = varargin{1};
                    end
                else %Handles sentences of the type mixture.x
                    if length(varargin{1}) == length(this.mComponents)
                        this.mMolFrac = varargin{1};
                    else
                        error('Dimensions mismatch between molar fraction vector and component vector in cMixture');
                    end
                end
            case 'molFrac' %Molar fraction, alternative denomination
                if length(index) == 2 %Handles sentences of the type mixture.molFrac(i)
                    indx = index(2).subs;
                    indx = indx{1};
                    if indx > length(this.mComponents)
                        error('Dimensions mismatch between molar fraction vector and component vector in cMixture');
                    else
                        this.mMolFrac(indx) = varargin{1};
                    end
                else %Handles sentences of the type mixture.molFrac
                    if length(varargin{1}) == length(this.mComponents)
                        this.mMolFrac = varargin{1};
                    else
                        error('Dimensions mismatch between molar fraction vector and component vector in cMixture');
                    end
                end
            case 'comp' %Components
                if length(index) == 2
                    switch index(2).type
                        case '.' %Handles sentences of the type mixture.component.property. If there is more than one component, assigns property to first component
                            comp = subsasgn(this.mComponents(1), index(2),varargin{1});
                            this.mComponents = comp;
                        case '()' %Handles sentences of the type mixture.component(i)
                            indx = index(2).subs;
                            indx = indx{1};
                            comp = varargin(1);
                            this.mComponents(indx) = comp{1};
                        otherwise
                            error (['Unexpected index.type of ' index(2).type]);
                    end
                elseif length(index) == 3 %
                    if strcmp(index(2).type, '()') == 1 %Handles sentences of the type mixture.component(i).property
                        indx = index(2).subs;
                        indx = indx{1};
                        comp = this.mComponents(indx);
                        comp = subsasgn(comp, index(3), varargin{1});
                        this.mComponents(indx) = comp;
                    elseif index(2).type == '.' %Handles sentences of the type mixture.component.property(i)
                        comp = this.mComponents(1);
                        comp = subsasgn(comp, index(2:3), varargin{1});
                        this.mComponents(1) = comp;    
                    end

                elseif length(index) == 4 %Handles sentences of the type mixture.component(i).property(j)
                    indx = index(2).subs;
                    indx = indx{1};
                    comp = this.mComponents(indx);
                    comp = subsasgn(comp, index(3:4), varargin{1});
                    this.mComponents(indx) = comp;    
                else %Handles sentences of the type mixture.component
                    comp = varargin(1);
                    this.mComponents = comp{1};
                end
                if length(this.mComponents) > length(this.mMolFrac) %Resizes the molar fraction vector
                    molFrac = zeros(length(this.mComponents),1);
                    for i = 1:length(this.mComponents)
                        if i > length(this.mMolFrac)
                            molFrac(i) = 0;
                        else
                            molFrac(i) = this.mMolFrac(i);
                        end
                    end
                    this.mMolFrac = molFrac;
                elseif length(this.mComponents) < length(this.mMolFrac)
                    molFrac = zeros(length(this.mComponents),1);
                    for i = 1:length(this.mComponents)
                        molFrac = this.mMolFrac(i);
                    end
                    this.mMolFrac = molFrac;
                end
            case 'numC' %Assigns number of component. Resizes vector this.mComponents if necessary
                def_numC = round(varargin{1});
                if ((def_numC < 1) || abs(def_numC-varargin{1}) > 1e-3)
                    error('The number of components must be a positive integer.');
                end
                comp = this.mComponents;
                numC = max(size(comp));
                if numC < def_numC 
                    compAdd = cSubstance;
                    for i = numC+1:def_numC
                        comp(i) = compAdd;  %Resizes the comp vector
                    end                    
                    this.mMolFrac = this.mMolFrac(1:numC); %Resizes the molar fraction vector
                elseif numC > def_numC
                    comp_sav = comp;
                    comp = cSubstance;
                    for i = 1:def_numC
                        comp(i) = comp_sav(i); %Resizes the comp vector
                    end
                    molFrac = zeros(1:numC);
                    for i = 1:numC
                        if i > def_numC
                            molFrac(i) = 0;
                        else
                            molFrac(i) = this.mMolFrac(i); 
                        end
                    end
                    this.mMolFrac = molFrac; %Resizes the molar fraction vector
                end
                this.mComponents = comp;
            case 'massFrac'
                error('Attempt to modify read-only property massFrac');
            case 'MW'
                error('Attempt to modify read-only property MW');       
            otherwise
                s = index(1).subs;
                if double(s(1)) == 107 %Interaction coefficients
                    this = SetInterCoef(this,index,varargin);
                else
                    error(['Reference to non-existent field ' index(1).subs '.']);
                end
        end
        
    case '()' %Handles vectors
        if isempty(this)
            this = cMixture;
        end
        if length(index) == 1
            this = builtin('subsasgn', this, index, varargin{end:-1:1});
        else
            this_subset = this(index(1).subs{:});
            this_subset = subsasgn (this_subset, index(2:end), varargin{end:-1:1});
            this(index(1).subs{:}) = this_subset;
        end
        
     otherwise
        error (['Unexpected operator ' index(1).type]);
end
       
