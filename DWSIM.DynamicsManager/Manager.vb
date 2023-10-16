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
Imports DWSIM.SharedClasses.SystemsOfUnits
Imports interp = DWSIM.MathOps.MathEx.Interpolation

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

    Public Function GetPropertyValuesFromEvents(fs As IFlowsheet, currenttime As DateTime, history As Dictionary(Of DateTime, XDocument), eventset As IDynamicsEventSet) As List(Of Tuple(Of String, String, Double)) Implements IDynamicsManager.GetPropertyValuesFromEvents

        Dim props As New List(Of Tuple(Of String, String, Double))

        Dim i As Integer

        Dim events = eventset.Events.Values.OrderBy(Function(e) e.TimeStamp).ToList()

        For i = 0 To events.Count - 1

            Dim current = events(i)

            If currenttime <= current.TimeStamp And current.EventType = Enums.Dynamics.DynamicsEventType.ChangeProperty Then

                If current.TransitionType <> Enums.Dynamics.DynamicsEventTransitionType.StepChange Then

                    Dim obj = fs.SimulationObjects(current.SimulationObjectID)
                    Dim values = current.SimulationObjectPropertyValue
                    Dim units = current.SimulationObjectPropertyUnits

                    Dim value = Converter.ConvertToSI(units, values.ToDoubleFromInvariant())

                    Dim state As XDocument = Nothing

                    Dim active As Boolean = False

                    Dim refevent As IDynamicsEvent = Nothing

                    Select Case current.TransitionReference

                        Case Enums.Dynamics.DynamicsEventTransitionReferenceType.InitialState

                            state = history.Values.First()

                            active = True

                        Case Enums.Dynamics.DynamicsEventTransitionReferenceType.PreviousEvent

                            If i = 0 Then

                                state = history.Values.First()

                                active = True

                            Else

                                refevent = events(i - 1)

                                If refevent.TimeStamp <= currenttime Then active = True

                            End If

                            state = history.Where(Function(h) h.Key <= currenttime).OrderByDescending(Function(h) h.Key).FirstOrDefault().Value

                        Case Enums.Dynamics.DynamicsEventTransitionReferenceType.SpecificEvent

                            If Not eventset.Events.ContainsKey(current.TransitionReferenceEventID) Then

                                Throw New Exception(String.Format("could not find reference event for transition in event '{0}'", current.Description))

                            End If

                            refevent = eventset.Events(current.TransitionReferenceEventID)

                            state = history.Where(Function(h) h.Key <= refevent.TimeStamp).OrderByDescending(Function(h) h.Key).FirstOrDefault().Value

                            If refevent.TimeStamp <= currenttime Then active = True

                    End Select

                    If active Then

                        fs.RestoreSnapshot(state, Enums.SnapshotType.ObjectData)

                        Dim value0 = Convert.ToDouble(fs.SimulationObjects(current.SimulationObjectID).GetPropertyValue(current.SimulationObjectProperty))

                        Dim span, dt As Double

                        If refevent Is Nothing Then

                            span = (current.TimeStamp - Date.MinValue).TotalMilliseconds
                            dt = (currenttime - Date.MinValue).TotalMilliseconds

                        Else

                            span = (current.TimeStamp - refevent.TimeStamp).TotalMilliseconds
                            dt = (currenttime - refevent.TimeStamp).TotalMilliseconds

                        End If

                        Dim xt = dt / span
                        Dim y0 = value0
                        Dim y1 = value
                        Dim yt As Double

                        Select Case current.TransitionType

                            Case Enums.Dynamics.DynamicsEventTransitionType.LinearChange

                                yt = interp.LinearInterpolation.Interpolate({0.0, 1.0}, {y0, y1}, xt)

                            Case Enums.Dynamics.DynamicsEventTransitionType.LogChange

                                yt = interp.LogLinearInterpolation.Interpolate({0.0, 1.0}, {y0, y1}, xt)

                            Case Enums.Dynamics.DynamicsEventTransitionType.CubicSplineChange

                                yt = interp.CubicSplineInterpolation.Interpolate({0.0, 1.0}, {y0, y1}, xt)

                            Case Enums.Dynamics.DynamicsEventTransitionType.RandomChange

                                yt = y0 + Math.Sign(y1 - y0) * New Random().NextDouble() * Math.Abs(y1 - y0)

                        End Select

                        props.Add(New Tuple(Of String, String, Double)(obj.Name, current.SimulationObjectProperty, yt))

                    End If

                End If

            End If

        Next

        Return props

    End Function

End Class
