'    Splitter Calculation Routines 
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports System.Linq
Imports DWSIM.Interfaces.Enums

Namespace UnitOperations

    <System.Serializable()> Public Class Splitter

        Inherits UnitOperations.UnitOpBaseClass

        <NonSerialized> <Xml.Serialization.XmlIgnore> Dim f As EditingForm_Splitter

        Public Enum OpMode
            SplitRatios = 0
            StreamMassFlowSpec = 1
            StreamMoleFlowSpec = 2
        End Enum

        Protected m_ratios As New System.Collections.ArrayList(3)
        Public OutCount As Integer = 0

        Public Property StreamFlowSpec As Double = 0.0#
        Public Property Stream2FlowSpec As Double = 0.0#

        Public Property OperationMode As OpMode = OpMode.SplitRatios

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            MyBase.LoadData(data)

            Me.m_ratios = New ArrayList

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "SplitRatios").SingleOrDefault.Elements.ToList
                m_ratios.Add(Double.Parse(xel.Value, ci))
            Next

            OutCount = 0
            For Each cp In GraphicObject.OutputConnectors
                If cp.IsAttached Then OutCount += 1
            Next

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("SplitRatios"))
                For Each d As Double In m_ratios
                    .Item(.Count - 1).Add(New XElement("SplitRatio", d.ToString(ci)))
                Next
            End With

            Return elements

        End Function

        Public ReadOnly Property Ratios() As System.Collections.ArrayList
            Get
                Return Me.m_ratios
            End Get
        End Property

        Public Sub New(ByVal Name As String, ByVal Description As String)

            MyBase.CreateNew()
            Me.ComponentName = Name
            Me.ComponentDescription = Description
            Me.m_ratios.Add(1.0#)
            Me.m_ratios.Add(0.0#)
            Me.m_ratios.Add(0.0#)


        End Sub

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac8"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            OutCount = 0
            For Each cp In GraphicObject.OutputConnectors
                If cp.IsAttached Then OutCount += 1
            Next

            Dim ems As Thermodynamics.Streams.MaterialStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            ems.Validate()
            Dim W As Double = ems.Phases(0).Properties.massflow.GetValueOrDefault
            Dim M As Double = ems.Phases(0).Properties.molarflow.GetValueOrDefault

            Dim i As Integer = 0
            Dim j As Integer = 0

            Dim ms As MaterialStream

            Select Case Me.OperationMode

                Case OpMode.SplitRatios

                    Dim cp As ConnectionPoint
                    For Each cp In Me.GraphicObject.OutputConnectors
                        If cp.IsAttached Then
                            ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                            With ms
                                .Phases(0).Properties.temperature = ems.Phases(0).Properties.temperature
                                .Phases(0).Properties.pressure = ems.Phases(0).Properties.pressure
                                .Phases(0).Properties.enthalpy = ems.Phases(0).Properties.enthalpy
                                Dim comp As BaseClasses.Compound
                                j = 0
                                For Each comp In .Phases(0).Compounds.Values
                                    comp.MoleFraction = ems.Phases(0).Compounds(comp.Name).MoleFraction
                                    comp.MassFraction = DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Phases(0).Compounds(comp.Name).MassFraction
                                    j += 1
                                Next
                                .Phases(0).Properties.massflow = W * Me.Ratios(i)
                                .Phases(0).Properties.massfraction = 1
                                .Phases(0).Properties.molarfraction = 1
                                .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
                            End With
                        End If
                        i += 1
                    Next

                Case OpMode.StreamMassFlowSpec

                    Dim cp As ConnectionPoint
                    Dim w1, w2 As Double

                    Dim wn(OutCount) As Double

                    Select Case OutCount
                        Case 1
                            w1 = Me.StreamFlowSpec
                            wn(0) = w1
                        Case 2
                            If W >= Me.StreamFlowSpec Then
                                w1 = Me.StreamFlowSpec
                                wn(0) = w1
                                wn(1) = W - w1
                            Else
                                Throw New Exception(FlowSheet.GetTranslatedString("Ovalorinformadonovli"))
                            End If
                        Case 3
                            If W >= Me.StreamFlowSpec + Me.Stream2FlowSpec Then
                                w1 = Me.StreamFlowSpec
                                w2 = Me.Stream2FlowSpec
                                wn(0) = w1
                                wn(1) = w2
                                wn(2) = W - w1 - w2
                            Else
                                Throw New Exception(FlowSheet.GetTranslatedString("Ovalorinformadonovli"))
                            End If
                    End Select

                    i = 0
                    For Each cp In Me.GraphicObject.OutputConnectors
                        If cp.IsAttached Then
                            ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                            With ms
                                .Phases(0).Properties.temperature = ems.Phases(0).Properties.temperature
                                .Phases(0).Properties.pressure = ems.Phases(0).Properties.pressure
                                .Phases(0).Properties.enthalpy = ems.Phases(0).Properties.enthalpy
                                Dim comp As BaseClasses.Compound
                                j = 0
                                For Each comp In .Phases(0).Compounds.Values
                                    comp.MoleFraction = ems.Phases(0).Compounds(comp.Name).MoleFraction
                                    comp.MassFraction = DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Phases(0).Compounds(comp.Name).MassFraction
                                    j += 1
                                Next
                                .Phases(0).Properties.massflow = wn(i)
                                .Phases(0).Properties.massfraction = 1
                                .Phases(0).Properties.molarfraction = 1
                                .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
                            End With
                        End If
                        i += 1
                    Next

                Case OpMode.StreamMoleFlowSpec

                    Dim cp As ConnectionPoint
                    Dim m1, m2 As Double

                    Dim mn(OutCount) As Double

                    Select Case OutCount
                        Case 1
                            m1 = m1
                            mn(0) = m1
                        Case 2
                            If M >= Me.StreamFlowSpec Then
                                m1 = Me.StreamFlowSpec
                                mn(0) = m1
                                mn(1) = M - m1
                            Else
                                Throw New Exception(FlowSheet.GetTranslatedString("Ovalorinformadonovli"))
                            End If
                        Case 3
                            If M >= Me.StreamFlowSpec + Me.Stream2FlowSpec Then
                                m1 = Me.StreamFlowSpec
                                m2 = Me.Stream2FlowSpec
                                mn(0) = m1
                                mn(1) = m2
                                mn(2) = M - m1 - m2
                            Else
                                Throw New Exception(FlowSheet.GetTranslatedString("Ovalorinformadonovli"))
                            End If

                    End Select

                    i = 0
                    For Each cp In Me.GraphicObject.OutputConnectors
                        If cp.IsAttached Then
                            ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                            With ms
                                .Phases(0).Properties.temperature = ems.Phases(0).Properties.temperature
                                .Phases(0).Properties.pressure = ems.Phases(0).Properties.pressure
                                .Phases(0).Properties.enthalpy = ems.Phases(0).Properties.enthalpy
                                Dim comp As BaseClasses.Compound
                                j = 0
                                For Each comp In .Phases(0).Compounds.Values
                                    comp.MoleFraction = ems.Phases(0).Compounds(comp.Name).MoleFraction
                                    comp.MassFraction = DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Phases(0).Compounds(comp.Name).MassFraction
                                    j += 1
                                Next
                                .Phases(0).Properties.massflow = Nothing
                                .Phases(0).Properties.molarflow = mn(i)
                                .Phases(0).Properties.massfraction = 1
                                .Phases(0).Properties.molarfraction = 1
                                .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
                            End With
                        End If
                        i += 1
                    Next

            End Select

        End Sub

        Public Overrides Sub DeCalculate()

            Dim i As Integer = 0
            Dim j As Integer = 0

            Dim ms As MaterialStream
            Dim cp As ConnectionPoint
            For Each cp In Me.GraphicObject.OutputConnectors
                If cp.IsAttached Then
                    ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                    j = 0
                    With ms
                        .Phases(0).Properties.temperature = Nothing
                        .Phases(0).Properties.pressure = Nothing
                        .Phases(0).Properties.enthalpy = Nothing
                        Dim comp As BaseClasses.Compound
                        For Each comp In .Phases(0).Compounds.Values
                            comp.MoleFraction = 0
                            comp.MassFraction = 0
                            j += 1
                        Next
                        .Phases(0).Properties.massflow = Nothing
                        .Phases(0).Properties.massfraction = 1
                        .Phases(0).Properties.molarfraction = 1
                    End With
                End If
                i += 1
            Next

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Select Case prop
                Case "PROP_SP_1"
                    If Me.OperationMode = OpMode.StreamMassFlowSpec Then
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.StreamFlowSpec)
                    Else
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.StreamFlowSpec)
                    End If
                Case "PROP_SP_2"
                    If Me.OperationMode = OpMode.StreamMassFlowSpec Then
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Stream2FlowSpec)
                    Else
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Stream2FlowSpec)
                    End If
                Case "SR1"
                    If Me.Ratios.Count > 0 Then value = Me.Ratios(0)
                Case "SR2"
                    If Me.Ratios.Count > 1 Then value = Me.Ratios(1)
                Case "SR3"
                    If Me.Ratios.Count > 2 Then value = Me.Ratios(2)
            End Select
            Return value
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim proplist As New ArrayList

            proplist.Add("PROP_SP_1")
            proplist.Add("PROP_SP_2")

            Select Case proptype
                Case PropertyType.RW
                    For i = 1 To OutCount - 1
                        proplist.Add("SR" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 1 To OutCount - 1
                        proplist.Add("SR" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 1 To OutCount
                        proplist.Add("SR" + CStr(i))
                    Next
                Case PropertyType.RO
                    proplist.Add("SR" + CStr(OutCount))
            End Select

            Return proplist.ToArray(GetType(System.String))
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Select Case prop
                Case "PROP_SP_1"
                    If Me.OperationMode = OpMode.StreamMassFlowSpec Then
                        Me.StreamFlowSpec = SystemsOfUnits.Converter.ConvertToSI(su.massflow, propval)
                    Else
                        Me.StreamFlowSpec = SystemsOfUnits.Converter.ConvertToSI(su.molarflow, propval)
                    End If
                Case "PROP_SP_2"
                    If Me.OperationMode = OpMode.StreamMassFlowSpec Then
                        Me.Stream2FlowSpec = SystemsOfUnits.Converter.ConvertToSI(su.massflow, propval)
                    Else
                        Me.Stream2FlowSpec = SystemsOfUnits.Converter.ConvertToSI(su.molarflow, propval)
                    End If
                Case "SR1"
                    If propval >= 0 And propval <= 1 Then
                        Me.Ratios(0) = propval
                        If OutCount = 2 Then Me.Ratios(1) = 1 - propval
                        If OutCount = 3 And Ratios(0) + Ratios(1) <= 1 Then Me.Ratios(2) = 1 - Me.Ratios(0) - Me.Ratios(1)
                    End If
                Case "SR2"
                    If propval >= 0 And propval <= 1 And Me.Ratios(0) + Me.Ratios(1) + propval <= 1 And OutCount = 3 Then
                        Me.Ratios(1) = propval
                        Me.Ratios(2) = 1 - Me.Ratios(0) - Me.Ratios(1)
                    End If
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim value As String = ""
            If prop.StartsWith("P") Then
                Select Case Me.OperationMode
                    Case OpMode.StreamMassFlowSpec
                        value = su.massflow
                    Case OpMode.StreamMoleFlowSpec
                        value = su.molarflow
                End Select
            Else
                value = ""
            End If
            Return value
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_Splitter With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Splitter With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.uo_split_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return FlowSheet.GetTranslatedString("SPLIT_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return FlowSheet.GetTranslatedString("SPLIT_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property
    End Class

End Namespace
