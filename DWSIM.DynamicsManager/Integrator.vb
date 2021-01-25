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

Public Class Integrator

    Implements Interfaces.IDynamicsIntegrator, ICustomXMLSerialization

    Public Property ID As String = "" Implements IDynamicsIntegrator.ID

    Public Property Description As String = "" Implements IDynamicsIntegrator.Description

    Public Property ShouldCalculateEquilibrium As Boolean Implements IDynamicsIntegrator.ShouldCalculateEquilibrium

    Public Property ShouldCalculatePressureFlow As Boolean Implements IDynamicsIntegrator.ShouldCalculatePressureFlow

    Public Property ShouldCalculateControl As Boolean Implements IDynamicsIntegrator.ShouldCalculateControl

    Public Property IntegrationStep As TimeSpan = New TimeSpan(0, 0, 5) Implements IDynamicsIntegrator.IntegrationStep

    Public Property Duration As TimeSpan = New TimeSpan(0, 10, 0) Implements IDynamicsIntegrator.Duration

    Public Property CurrentTime As Date = New Date() Implements IDynamicsIntegrator.CurrentTime

    Public Property CalculationRateEquilibrium As Integer = 1 Implements IDynamicsIntegrator.CalculationRateEquilibrium

    Public Property CalculationRatePressureFlow As Integer = 1 Implements IDynamicsIntegrator.CalculationRatePressureFlow

    Public Property CalculationRateControl As Integer = 1 Implements IDynamicsIntegrator.CalculationRateControl

    Public Property RealTime As Boolean = False Implements IDynamicsIntegrator.RealTime

    Public Property MonitoredVariableValues As Dictionary(Of Integer, List(Of IDynamicsMonitoredVariable)) = New Dictionary(Of Integer, List(Of IDynamicsMonitoredVariable)) Implements IDynamicsIntegrator.MonitoredVariableValues

    Public Property MonitoredVariables As List(Of IDynamicsMonitoredVariable) = New List(Of IDynamicsMonitoredVariable) Implements IDynamicsIntegrator.MonitoredVariables

    Public Property RealTimeStepMs As Integer = 1000 Implements IDynamicsIntegrator.RealTimeStepMs

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Dim data = XMLSerializer.XMLSerializer.Serialize(Me)
        'Dim e1 = New XElement("MonitoredVariableValues")
        'For Each kvp As KeyValuePair(Of Integer, List(Of IDynamicsMonitoredVariable)) In MonitoredVariableValues
        '    Dim e2 = New XElement("Step" + "_" + kvp.Key.ToString(Globalization.CultureInfo.InvariantCulture))
        '    For Each item As ICustomXMLSerialization In kvp.Value
        '        e2.Add(item.SaveData)
        '    Next
        '    e1.Add(e2)
        'Next
        'data.Add(e1)
        Dim e3 = New XElement("MonitoredVariables")
        For Each item As ICustomXMLSerialization In MonitoredVariables
            Dim e4 = New XElement("MonitoredVariable")
            e4.Add(item.SaveData)
            e3.Add(e4)
        Next
        data.Add(e3)
        Return data
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        'Dim elm As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "MonitoredVariableValues").LastOrDefault
        'If Not elm Is Nothing Then
        '    MonitoredVariableValues = New Dictionary(Of Integer, List(Of IDynamicsMonitoredVariable))
        '    For Each xel2 As XElement In elm.Elements
        '        Try
        '            Dim l As New List(Of IDynamicsMonitoredVariable)
        '            For Each el In xel2.Elements
        '                Dim item As New MonitoredVariable
        '                item.LoadData(el.Elements.ToList)
        '                l.Add(item)
        '            Next
        '            MonitoredVariableValues.Add(Integer.Parse(xel2.Name.LocalName.Split("_")(1), Globalization.CultureInfo.InvariantCulture), l)
        '        Catch ex As Exception
        '        End Try
        '    Next
        'End If
        Dim elm2 As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "MonitoredVariables").LastOrDefault
        If Not elm2 Is Nothing Then
            MonitoredVariables = New List(Of IDynamicsMonitoredVariable)
            For Each el In elm2.Elements
                Dim item As New MonitoredVariable
                item.LoadData(el.Elements.ToList)
                MonitoredVariables.Add(item)
            Next
        End If
        Return True
    End Function

End Class
