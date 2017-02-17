function this = SetHighMWParam(this)
%Corrects GCEoS parameters for mixtures containing high molecular weight
%compounds
%
%Reference: T Fornari, Fluid Phase Eq. 262 (2007) 187-209, Table 4

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

%CH=C pure component parameters
this = SetGroup(this,18,'CH=C',600,0.676,421650,-1.3756,0);

%H2/CH3-CH2 interaction parameters
this = SetkG(this,1,40,'k1',0.845);
this = SetkG(this,40,1,'k1',0.845);
this = SetkG(this,2,40,'k1',0.845);
this = SetkG(this,40,2,'k1',0.845);
this = SetkG(this,1,40,'k2',-0.115);
this = SetkG(this,40,1,'k2',-0.115);
this = SetkG(this,2,40,'k2',-0.115);
this = SetkG(this,40,2,'k2',-0.115);
this = SetkG(this,1,40,'alfa',-1);
this = SetkG(this,40,1,'alfa',-1);
this = SetkG(this,2,40,'alfa',-1);
this = SetkG(this,40,2,'alfa',-1);

%CO2/CH3 interaction parameters
this = SetkG(this,1,46,'k1',0.898);
this = SetkG(this,46,1,'k1',0.898);
this = SetkG(this,1,46,'k2',0);
this = SetkG(this,46,1,'k2',0);
this = SetkG(this,1,46,'alfa',4.683);
this = SetkG(this,46,1,'alfa',4.683);

%CO2/CH2 interaction parameters
this = SetkG(this,2,46,'k1',0.874);
this = SetkG(this,46,2,'k1',0.874);
this = SetkG(this,2,46,'k2',0);
this = SetkG(this,46,2,'k2',0);
this = SetkG(this,2,46,'alfa',4.683);
this = SetkG(this,46,2,'alfa',4.683);

%CO2/CH=C interaction parameters
this = SetkG(this,18,46,'k1',0.882);
this = SetkG(this,46,18,'k1',0.882);
this = SetkG(this,18,46,'k2',0.022);
this = SetkG(this,46,18,'k2',0.022);
this = SetkG(this,18,46,'alfa',-14.247);
this = SetkG(this,46,18,'alfa',-14.247);

%AC/CH3-CH2 interaction parameters
this = SetkG(this,1,20,'k1',0.854);
this = SetkG(this,20,1,'k1',0.854);
this = SetkG(this,2,20,'k1',0.854);
this = SetkG(this,20,2,'k1',0.854);
this = SetkG(this,1,20,'k2',0);
this = SetkG(this,20,1,'k2',0);
this = SetkG(this,2,20,'k2',0);
this = SetkG(this,20,2,'k2',0);
this = SetkG(this,1,20,'alfa',0);
this = SetkG(this,20,1,'alfa',0);
this = SetkG(this,2,20,'alfa',0);
this = SetkG(this,20,2,'alfa',0);

%AC/CH=CH interaction parameters
this = SetkG(this,20,15,'k1',1.1);
this = SetkG(this,15,20,'k1',1.1);
this = SetkG(this,20,15,'k2',0);
this = SetkG(this,15,20,'k2',0);
this = SetkG(this,20,15,'alfa',0);
this = SetkG(this,15,20,'alfa',0);

%AC/CH3 interaction parameters
this = SetkG(this,20,1,'k1',0.937);
this = SetkG(this,1,20,'k1',0.937);
this = SetkG(this,20,1,'k2',0.02);
this = SetkG(this,1,20,'k2',0.02);
this = SetkG(this,20,1,'alfa',-0.794);
this = SetkG(this,1,20,'alfa',-0.794);

%AC/CH2 interaction parameters
this = SetkG(this,20,2,'k1',1.136);
this = SetkG(this,2,20,'k1',1.136);
this = SetkG(this,20,2,'k2',0.02);
this = SetkG(this,2,20,'k2',0.02);
this = SetkG(this,20,2,'alfa',-2.678);
this = SetkG(this,2,20,'alfa',-2.678);

%AC/CH=CH interaction parameters
this = SetkG(this,20,2,'k1',1.1);
this = SetkG(this,2,20,'k1',1.1);
this = SetkG(this,20,2,'k2',0);
this = SetkG(this,2,20,'k2',0);
this = SetkG(this,20,2,'alfa',0);
this = SetkG(this,2,20,'alfa',0);