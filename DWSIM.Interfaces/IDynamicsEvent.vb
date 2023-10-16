'    DWSIM Interface definitions
'    Copyright 2020 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Public Interface IDynamicsEvent

    Property ID As String

    Property Description As String

    Property Enabled As Boolean

    Property TimeStamp As DateTime

    Property EventType As Enums.Dynamics.DynamicsEventType

    Property SimulationObjectID As String

    Property SimulationObjectProperty As String

    Property SimulationObjectPropertyValue As String

    Property SimulationObjectPropertyUnits As String

    Property ScriptID As String

    Property TransitionType As Enums.Dynamics.DynamicsEventTransitionType

    Property TransitionReference As Enums.Dynamics.DynamicsEventTransitionReferenceType

End Interface
