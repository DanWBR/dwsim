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

Public Class MonitoredVariable

    Implements IDynamicsMonitoredVariable, ICustomXMLSerialization, ICloneable

    Public Property ID As String = "" Implements IDynamicsMonitoredVariable.ID

    Public Property Description As String = "" Implements IDynamicsMonitoredVariable.Description

    Public Property TimeStamp As Date = New Date Implements IDynamicsMonitoredVariable.TimeStamp

    Public Property ObjectID As String = "" Implements IDynamicsMonitoredVariable.ObjectID

    Public Property PropertyID As String = "" Implements IDynamicsMonitoredVariable.PropertyID

    Public Property PropertyValue As String = "" Implements IDynamicsMonitoredVariable.PropertyValue

    Public Property PropertyUnits As String = "" Implements IDynamicsMonitoredVariable.PropertyUnits

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

    Public Function Clone() As Object Implements ICloneable.Clone
        Dim mv As New MonitoredVariable()
        mv.LoadData(Me.SaveData)
        Return mv
    End Function
End Class
