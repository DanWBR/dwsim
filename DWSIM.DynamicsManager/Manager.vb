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

Imports DWSIM.ExtensionMethods
Imports DWSIM.Interfaces
Imports OxyPlot
Imports OxyPlot.Axes

Public Class Manager

    Implements Interfaces.IDynamicsManager, ICustomXMLSerialization

    Public Property Description As String = "" Implements IDynamicsManager.Description

    Public Property ScheduleList As Dictionary(Of String, IDynamicsSchedule) = New Dictionary(Of String, IDynamicsSchedule) Implements IDynamicsManager.ScheduleList

    Public Property CurrentSchedule As String = "" Implements IDynamicsManager.CurrentSchedule

    Public Property CauseAndEffectMatrixList As Dictionary(Of String, IDynamicsCauseAndEffectMatrix) = New Dictionary(Of String, IDynamicsCauseAndEffectMatrix) Implements IDynamicsManager.CauseAndEffectMatrixList

    Public Property EventSetList As Dictionary(Of String, IDynamicsEventSet) = New Dictionary(Of String, IDynamicsEventSet) Implements IDynamicsManager.EventSetList

    Public Property IntegratorList As Dictionary(Of String, IDynamicsIntegrator) = New Dictionary(Of String, IDynamicsIntegrator) Implements IDynamicsManager.IntegratorList

    Public Property ToggleDynamicMode As Action(Of Boolean) Implements IDynamicsManager.ToggleDynamicMode

    Public Property RunSchedule As Func(Of String, Task) Implements IDynamicsManager.RunSchedule

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
        data.Add(e3)
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

    Public Function GetChartModel(IntegratorID As String) As Object Implements IDynamicsManager.GetChartModel

        Dim integrator = IntegratorList(IntegratorID)

        Dim model = New PlotModel() With {.Title = integrator.Description}

        Dim i, j As Integer

        Dim xavals = New List(Of Double)
        i = 1
        For Each item In integrator.MonitoredVariableValues
            If integrator.RealTime Then
                xavals.Add(item.Key * integrator.RealTimeStepMs / 1000)
            Else
                xavals.Add(item.Key * integrator.IntegrationStep.TotalMilliseconds / 1000)
            End If
            i += 1
        Next

        model.TitleFontSize = 12

        model.Axes.Add(New LinearAxis() With {
            .MajorGridlineStyle = LineStyle.Dash,
            .MinorGridlineStyle = LineStyle.Dot,
            .Position = AxisPosition.Bottom,
            .FontSize = 10,
            .Title = "Time (s)"
        })

        model.Axes.Add(New LinearAxis() With {
            .MajorGridlineStyle = LineStyle.Dash,
            .MinorGridlineStyle = LineStyle.Dot,
            .Position = AxisPosition.Left,
            .FontSize = 10,
            .Title = "",
            .Key = "0"
        })

        model.LegendFontSize = 10
        model.LegendPlacement = LegendPlacement.Outside
        model.LegendOrientation = LegendOrientation.Horizontal
        model.LegendPosition = LegendPosition.BottomCenter
        model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView

        If integrator.MonitoredVariableValues.Count = 0 Then
            Return model
        End If

        Dim values As New List(Of List(Of Double))
        Dim names As New List(Of String)
        For Each var In integrator.MonitoredVariables
            values.Add(New List(Of Double))
            If var.PropertyUnits <> "" Then
                names.Add(var.Description & " (" & var.PropertyUnits & ")")
            Else
                names.Add(var.Description)
            End If
        Next

        For Each item In integrator.MonitoredVariableValues
            i = 0
            For Each var In item.Value
                values(i).Add(var.PropertyValue.ToDoubleFromInvariant)
                i += 1
            Next
        Next

        i = 0
        For Each l In values
            model.AddLineSeries(xavals, l, names(i))
            i += 1
        Next

        Return model

    End Function

    Public Function GetSchedule(name As String) As IDynamicsSchedule Implements IDynamicsManager.GetSchedule
        Return ScheduleList.Values.Where(Function(s) s.Description = name).FirstOrDefault()
    End Function

    Public Function GetIntegrator(name As String) As IDynamicsIntegrator Implements IDynamicsManager.GetIntegrator
        Return IntegratorList.Values.Where(Function(s) s.Description = name).FirstOrDefault()
    End Function

    Public Function GetEventSet(name As String) As IDynamicsEventSet Implements IDynamicsManager.GetEventSet
        Return EventSetList.Values.Where(Function(s) s.Description = name).FirstOrDefault()
    End Function

    Public Function GetCauseAndEffectMatrix(name As String) As IDynamicsCauseAndEffectMatrix Implements IDynamicsManager.GetCauseAndEffectMatrix
        Return CauseAndEffectMatrixList.Values.Where(Function(s) s.Description = name).FirstOrDefault()
    End Function

End Class
