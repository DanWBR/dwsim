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

Public Class Schedule

    Implements IDynamicsSchedule, ICustomXMLSerialization

    Public Property ID As String = "" Implements IDynamicsSchedule.ID

    Public Property Description As String = "" Implements IDynamicsSchedule.Description

    Public Property CurrentIntegrator As String = "" Implements IDynamicsSchedule.CurrentIntegrator

    Public Property UsesCauseAndEffectMatrix As Boolean Implements IDynamicsSchedule.UsesCauseAndEffectMatrix

    Public Property UsesEventList As Boolean Implements IDynamicsSchedule.UsesEventList

    Public Property CurrentCauseAndEffectMatrix As String = "" Implements IDynamicsSchedule.CurrentCauseAndEffectMatrix

    Public Property CurrentEventList As String = "" Implements IDynamicsSchedule.CurrentEventList

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

End Class
