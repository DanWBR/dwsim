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

Public Class Manager

    Implements Interfaces.IDynamicsManager, ICustomXMLSerialization

    Public Property Description As String = "" Implements IDynamicsManager.Description

    Public Property ScheduleList As Dictionary(Of String, IDynamicsSchedule) = New Dictionary(Of String, IDynamicsSchedule) Implements IDynamicsManager.ScheduleList

    Public Property CurrentSchedule As String = "" Implements IDynamicsManager.CurrentSchedule

    Public Property CauseAndEffectMatrixList As Dictionary(Of String, IDynamicsCauseAndEffectMatrix) = New Dictionary(Of String, IDynamicsCauseAndEffectMatrix) Implements IDynamicsManager.CauseAndEffectMatrixList

    Public Property EventSetList As Dictionary(Of String, IDynamicsEventSet) = New Dictionary(Of String, IDynamicsEventSet) Implements IDynamicsManager.EventSetList

    Public Property IntegratorList As Dictionary(Of String, IDynamicsIntegrator) = New Dictionary(Of String, IDynamicsIntegrator) Implements IDynamicsManager.IntegratorList

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Dim data = XMLSerializer.XMLSerializer.Serialize(Me)
        Dim e1 = New XElement("ScheduleList")
        For Each kvp As KeyValuePair(Of String, IDynamicsSchedule) In ScheduleList
            e1.Add(New XElement("Schedule",
                                DirectCast(kvp.Value, ICustomXMLSerialization).SaveData))
        Next
        data.Add(e1)
        Dim e2 = New XElement("EventSetList")
        For Each kvp As KeyValuePair(Of String, IDynamicsEventSet) In EventSetList
            e2.Add(New XElement("EventSet",
                                DirectCast(kvp.Value, ICustomXMLSerialization).SaveData))
        Next
        data.Add(e2)
        Dim e3 = New XElement("IntegratorList")
        For Each kvp As KeyValuePair(Of String, IDynamicsIntegrator) In IntegratorList
            e3.Add(New XElement("Integrator",
                                DirectCast(kvp.Value, ICustomXMLSerialization).SaveData))
        Next
        data.Add(e2)
        Return data
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Dim elm As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ScheduleList").LastOrDefault
        If Not elm Is Nothing Then
            ScheduleList = New Dictionary(Of String, IDynamicsSchedule)
            For Each xel2 As XElement In elm.Elements
                Dim sch = New Schedule()
                DirectCast(sch, ICustomXMLSerialization).LoadData(xel2.Elements.ToList)
                ScheduleList.Add(sch.ID, sch)
            Next
        End If
        Dim elm2 As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "EventSetList").LastOrDefault
        If Not elm2 Is Nothing Then
            EventSetList = New Dictionary(Of String, IDynamicsEventSet)
            For Each xel2 As XElement In elm2.Elements
                Dim es = New EventSet
                DirectCast(es, ICustomXMLSerialization).LoadData(xel2.Elements.ToList)
                EventSetList.Add(es.ID, es)
            Next
        End If
        Dim elm3 As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "IntegratorList").LastOrDefault
        If Not elm3 Is Nothing Then
            IntegratorList = New Dictionary(Of String, IDynamicsIntegrator)
            For Each xel2 As XElement In elm3.Elements
                Dim intg = New Integrator
                DirectCast(intg, ICustomXMLSerialization).LoadData(xel2.Elements.ToList)
                IntegratorList.Add(intg.ID, intg)
            Next
        End If
        Return True
    End Function

End Class
