﻿'    DWSIM Interface definitions
'    Copyright 2020 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Public Interface IDynamicsSchedule

    Property ID As String

    Property Description As String

    Property CurrentIntegrator As String

    Property UsesCauseAndEffectMatrix As Boolean

    Property UsesEventList As Boolean

    Property CurrentCauseAndEffectMatrix As String

    Property CurrentEventList As String

    Property InitialFlowsheetStateID As String

    Property UseCurrentStateAsInitial As Boolean

    Property ResetContentsOfAllObjects As Boolean

End Interface
