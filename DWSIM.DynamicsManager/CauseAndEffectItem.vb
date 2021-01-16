'    DWSIM Dynamics Library
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

Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums

Public Class CauseAndEffectItem

    Implements IDynamicsCauseAndEffectItem, ICustomXMLSerialization

    Public Property ID As String = "" Implements IDynamicsCauseAndEffectItem.ID

    Public Property Description As String = "" Implements IDynamicsCauseAndEffectItem.Description

    Public Property Enabled As Boolean Implements IDynamicsCauseAndEffectItem.Enabled

    Public Property AssociatedIndicator As String = "" Implements IDynamicsCauseAndEffectItem.AssociatedIndicator

    Public Property AssociatedIndicatorAlarm As Dynamics.DynamicsAlarmType Implements IDynamicsCauseAndEffectItem.AssociatedIndicatorAlarm

    Public Property SimulationObjectID As String = "" Implements IDynamicsCauseAndEffectItem.SimulationObjectID

    Public Property SimulationObjectProperty As String = "" Implements IDynamicsCauseAndEffectItem.SimulationObjectProperty

    Public Property SimulationObjectPropertyValue As String = "" Implements IDynamicsCauseAndEffectItem.SimulationObjectPropertyValue

    Public Property SimulationObjectPropertyUnits As String = "" Implements IDynamicsCauseAndEffectItem.SimulationObjectPropertyUnits

    Public Property ScriptID As String = "" Implements IDynamicsCauseAndEffectItem.ScriptID

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

End Class
