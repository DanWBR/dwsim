'    DWSIM Dynamics Library
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

Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums

Public Class DynamicEvent

    Implements IDynamicsEvent, ICustomXMLSerialization

    Public Property ID As String = "" Implements IDynamicsEvent.ID

    Public Property Description As String = "" Implements IDynamicsEvent.Description

    Public Property TimeStamp As Date = DateTime.MinValue Implements IDynamicsEvent.TimeStamp

    Public Property EventType As Dynamics.DynamicsEventType = Dynamics.DynamicsEventType.ChangeProperty Implements IDynamicsEvent.EventType

    Public Property SimulationObjectID As String = "" Implements IDynamicsEvent.SimulationObjectID

    Public Property SimulationObjectProperty As String = "" Implements IDynamicsEvent.SimulationObjectProperty

    Public Property SimulationObjectPropertyValue As String = "" Implements IDynamicsEvent.SimulationObjectPropertyValue

    Public Property SimulationObjectPropertyUnits As String = "" Implements IDynamicsEvent.SimulationObjectPropertyUnits

    Public Property ScriptID As String = "" Implements IDynamicsEvent.ScriptID

    Public Property Enabled As Boolean = True Implements IDynamicsEvent.Enabled

    Public Property TransitionType As Dynamics.DynamicsEventTransitionType = Dynamics.DynamicsEventTransitionType.StepChange Implements IDynamicsEvent.TransitionType

    Public Property TransitionReference As Dynamics.DynamicsEventTransitionReferenceType = Dynamics.DynamicsEventTransitionReferenceType.PreviousEvent Implements IDynamicsEvent.TransitionReference

    Public Property TransitionReferenceEventID As String = "" Implements IDynamicsEvent.TransitionReferenceEventID

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

End Class
