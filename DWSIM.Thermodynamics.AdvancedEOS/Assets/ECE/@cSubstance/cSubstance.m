function this = cSubstance(varargin)
%Constructor of the cSubstance class
%Parameters (optional):
%name - Name of the substance
%MW - Molecular weight (g/mol)
%Tc - Critical temperature (K)
%Pc - Critical pressure (Pa)
%w - Acentric factor
%AntA - Parameter A of Antoine equation, lnP(Pa) = A-B/(T(K)+C)
%AntB - Parameter B of Antoine equation, lnP(Pa) = A-B/(T(K)+C)
%AntC - Parameter C of Antoine equation, lnP(Pa) = A-B/(T(K)+C)
%Tf - Melting temperature at the triple point (K)
%Hf - Melting enthalpy at the triple point (J/mol)
%EoSParam - EoS-specific parameter(s)

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

%Defalut constructor
this = struct( ...
    'mName', [],...         %Name 
    'mMW', [],...            %Molecular weight (g/mol)
    'mTc', [],...            %Critical temperature (K)
    'mPc', [],...            %Critical pressure (Pa)
    'mw', [],...             %Acentric factor
    'mAntA', [],...          %Parameter A of Antoine equation, lnP(Pa) = A-B/(T(K)+C)
    'mAntB', [],...          %Parameter B of Antoine equation, lnP(Pa) = A-B/(T(K)+C)
    'mAntC', [],...          %Parameter C of Antoine equation, lnP(Pa) = A-B/(T(K)+C)
    'mTf', [],...            %Melting temperature at the triple point (K)
    'mHf', [],...            %Melting enthalpy at the triple point (J/mol)
    'mEoSParam', []);        %EoS-specific parameter(s)

%Initialize parameters
this.mName = ' ';
this.mMW = 0;
this.mTc = 0;
this.mPc = 0;
this.mw = 0;
this.mAntA = 0;
this.mAntB = 0;
this.mAntC = 0;
this.mTf = 0;
this.mHf = 0;
this.mEoSParam = {};


%Parameters assignment
if nargin > 0
    this.mName = varargin{1};
end
if nargin > 1
    this.mMW = varargin{2};
end
if nargin > 2
    this.mTc = varargin{3};
end
if nargin > 3
    this.mPc = varargin{4};
end
if nargin > 4
    this.mw = varargin{5};
end
if nargin > 5
    this.mAntA = varargin{6};
end
if nargin > 6
    this.mAntB = varargin{7};
end
if nargin > 7
    this.mAntC = varargin{8};
end
if nargin > 8
    this.mTf = varargin{9};
end
if nargin > 9
    this.mHf = varargin{10};
end
if nargin > 10
    this.mEoSParam = varargin(11);
end

%Create object
this = class(this,'cSubstance');