function varargout = subsref(this, index)
%Accesor for cMixture class

%Copyright (c) 2011 Angel Martin, University of Valladolid (Spain)
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
                if length(index) > 1 %Handles sentences of the type mixture.x(i)
                    if isempty (this)
                        varargout = {};
                    else
                        molVector = this.mMolFrac;
                        indx = index(2).subs;
                        indx = indx{1};
                        varargout = molVector(indx);
                        varargout = {varargout};
                    end
                else %Handles sentences of the type mixture.x
                    if isempty (this)
                        varargout = {};
                    else
                        varargout = this.mMolFrac;
                        varargout = {varargout};
                    end
                end
                
            case 'molFrac' %Molar fraction (alternative denomination)
                if length(index) > 1  %Handles sentences of the type mixture.molFrac(i)
                    if isempty (this)
                        varargout = {};
                    else
                        molVector = this.mMolFrac;
                        varargout = subsref(molVector,index(2));
                        varargout = {varargout};
                    end
                else %Handles sentences of the type mixture.molFrac
                    if isempty (this)
                        varargout = {};
                    else
                        varargout = this.mMolFrac;
                        varargout = {varargout};
                    end
                end
    
            case 'comp'%Components
                if length(index) == 2          
                    if isempty (this)
                        varargout = {};
                    else
                        switch index(2).type
                            case '.' %Handles sentences of the type mixture.component.property. If there is more than one component, returns property of first component
                                varargout = subsref(this.mComponents(1),index(2));
                                varargout = {varargout};
                            case '()' %Handles sentences of the type mixture.component(i)
                                indx = index(2).subs;
                                indx = indx{1};
                                varargout = this.mComponents(indx);
                                varargout = {varargout};
                            otherwise
                                 error (['Unexpected index.type of ' index(2).type]);
                        end
                    end
                elseif length(index) == 3  
                   if isempty (this)
                        varargout = {};
                   else
                       if strcmp(index(2).type, '()') == 1 %Handles sentences of the type mixture.component(i).property
                            indx = index(2).subs;
                            indx = indx{1};
                            comp = this.mComponents(indx);
                            varargout = subsref(comp,index(3));
                            varargout = {varargout};
                       elseif index(2).type == '.' %Handles sentences of the type mixture.component.property(i)
                            comp = this.mComponents(1);
                            varargout = subsref(comp,index(2:3));
                            varargout = {varargout};
                       end
                   end 
                elseif length(index) == 4 %Handles sentences of the type mixture.component(i).property(j)
                    if isempty (this)
                        varargout = {};
                   else
                        indx = index(2).subs;
                        indx = indx{1};
                        comp = this.mComponents(indx);
                        varargout = subsref(comp,index(3:4));
                        varargout = {varargout};
                   end     
                else %Handles sentences of the type mixuture.component
                    if isempty (this)
                        varargout = {};
                    else
                        varargout = this.mComponents;
                        varargout = {varargout};
                    end
                end
                
            case 'numC' %Number of components
                if isempty (this)
                    varargout = {};
                else
                    varargout = max(size(this.mComponents));
                    varargout = {varargout};
                end

            case 'massFrac' %Mass fraction composition
                if isempty (this)
                    varargout = {};
                else
                    w = massFrac(this);
                    if length(index) > 1
                        indx = index(2).subs;
                        indx = indx{1};
                        varargout = w(indx);
                        varargout = {varargout};   
                    else
                        varargout = w;
                        varargout = {varargout};      
                    end
                end
            case 'MW' %Molecular weight of the mixture
                if isempty (this)
                    varargout = {};
                else
                    varargout = MW(this);
                    varargout = {varargout};
                end
            otherwise
                s = index(1).subs;
                if double(s(1)) == 107 %Interaction coefficients
                    varargout = GetInterCoef(this,index);
                    varargout = {varargout};
                else
                    error(['Reference to non-existent field ' index(1).subs '.']);
                end
        end
        
    case '()' %Handles vectors
        this_subset = this(index(1).subs{:});
        if length(index) == 1
            varargout = {this_subset};
        else
            varargout = cell(size(this_subset));
            [varargout{:}] = subsref(this_subset, index(2:end));
        end
        
    otherwise
        error (['Unexpected operator ' index(1).type]);
              
end