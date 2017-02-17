function display(this)
%Displays the properties of cSubstance objects in the command window

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

%handle empty inputname
display_name = inputname(1);
if isempty(display_name)
    display_name = 'ans';
end

%handle scalar vs. vector this

if length(this) == 1 %scalar case
    disp([inputname(1) ' = cSubstance object']);

    s = sprintf('     name: %s',this.mName);
    disp(s);
    s = sprintf('     MW:   %f g/mol', this.mMW);
    disp(s);
    s = sprintf('     Tc:   %f K', this.mTc);
    disp(s);
    s = sprintf('     Pc:   %f Pa', this.mPc);
    disp(s);
    s = sprintf('     w:    %f', this.mw);
    disp(s);
    s = sprintf('     AntA: %f', this.mAntA);
    disp(s);
    s = sprintf('     AntB: %f', this.mAntB);
    disp(s);
    s = sprintf('     AntC: %f', this.mAntC);
    disp(s);
    s = sprintf('     Tf: %f K', this.mTf);
    disp(s);
    s = sprintf('     Hf: %f J/mol', this.mHf);
    disp(s);
    
    numP = length(this.mEoSParam);
    if numP > 0
        s1 = '     EoS-specific parameter(s): ';
        for i = 1:numP
            param = this.mEoSParam(i);
            param = param{1};
            if isempty(param)
                s = ' 0;';
            else
                param = param{1};
                s = sprintf(' %f;',param);
            end
            s1 = strcat(s1,s);
        end
        disp(s1);   
    end
    
else %array case
    s = sprintf('cSubstance object: %d by %d', size(this,1), size(this,2));
	disp(s);
end