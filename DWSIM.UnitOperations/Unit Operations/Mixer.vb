'    Mixer Calculation Routines 
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

Namespace DWSIM.SimulationObjects.UnitOperations

    <System.Serializable()> Public Class Mixer

        Inherits SharedClasses.UnitOperations.UnitOpBaseClass

        Public Enum PressureBehavior
            Average
            Maximum
            Minimum
        End Enum

        Protected m_pressurebehavior As PressureBehavior = PressureBehavior.Minimum

        Public Property PressureCalculation() As PressureBehavior
            Get
                Return Me.m_pressurebehavior
            End Get
            Set(ByVal value As PressureBehavior)
                Me.m_pressurebehavior = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description




        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac6"))
            End If

            Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)

            Dim H, Hs, T, W, We, P As Double
            H = 0
            Hs = 0
            T = 0
            W = 0
            We = 0
            P = 0
            Dim i As Integer = 1
            Dim ms As MaterialStream
            Dim cp As ConnectionPoint
            For Each cp In Me.GraphicObject.InputConnectors
                If cp.IsAttached Then
                    If cp.AttachedConnector.AttachedFrom.Calculated = False Then Throw New Exception(FlowSheet.GetTranslatedString("Umaoumaiscorrentesna"))
                    ms = Me.FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedFrom.Name)
                    ms.Validate()
                    If Me.PressureCalculation = PressureBehavior.Minimum Then
                        If ms.Phases(0).Properties.pressure.GetValueOrDefault < P Then
                            P = ms.Phases(0).Properties.pressure
                        ElseIf P = 0 Then
                            P = ms.Phases(0).Properties.pressure
                        End If
                    ElseIf Me.PressureCalculation = PressureBehavior.Maximum Then
                        If ms.Phases(0).Properties.pressure.GetValueOrDefault > P Then
                            P = ms.Phases(0).Properties.pressure
                        ElseIf P = 0 Then
                            P = ms.Phases(0).Properties.pressure
                        End If
                    Else
                        P = P + ms.Phases(0).Properties.pressure.GetValueOrDefault
                        i += 1
                    End If

                    We = ms.Phases(0).Properties.massflow.GetValueOrDefault
                    W += We
                    If Not Double.IsNaN(ms.Phases(0).Properties.enthalpy.GetValueOrDefault) Then H += We * ms.Phases(0).Properties.enthalpy.GetValueOrDefault
                End If
            Next

            If W <> 0.0# Then Hs = H / W Else Hs = 0.0#

            If Me.PressureCalculation = PressureBehavior.Average Then P = P / (i - 1)

            T = 0

            Dim n As Integer = DirectCast(Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name), MaterialStream).Phases(0).Compounds.Count
            Dim Vw As New Dictionary(Of String, Double)
            For Each cp In Me.GraphicObject.InputConnectors
                If cp.IsAttached Then
                    ms = Me.FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedFrom.Name)
                    Dim comp As BaseClasses.Compound
                    For Each comp In ms.Phases(0).Compounds.Values
                        If Not Vw.ContainsKey(comp.Name) Then
                            Vw.Add(comp.Name, 0)
                        End If
                        Vw(comp.Name) += comp.MassFraction.GetValueOrDefault * ms.Phases(0).Properties.massflow.GetValueOrDefault
                    Next
                    If W <> 0.0# Then T += ms.Phases(0).Properties.massflow.GetValueOrDefault / W * ms.Phases(0).Properties.temperature.GetValueOrDefault
                End If
            Next

            If W = 0.0# Then T = 273.15

            Dim omstr As MaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
            With omstr
                If W <> 0.0# Then .Phases(0).Properties.enthalpy = Hs
                .Phases(0).Properties.pressure = P
                .Phases(0).Properties.massflow = W
                .Phases(0).Properties.molarfraction = 1
                .Phases(0).Properties.massfraction = 1
                Dim comp As BaseClasses.Compound
                For Each comp In .Phases(0).Compounds.Values
                    If W <> 0.0# Then comp.MassFraction = Vw(comp.Name) / W
                Next
                Dim mass_div_mm As Double = 0
                Dim sub1 As BaseClasses.Compound
                For Each sub1 In .Phases(0).Compounds.Values
                    mass_div_mm += sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight
                Next
                For Each sub1 In .Phases(0).Compounds.Values
                    If W <> 0.0# Then
                        sub1.MoleFraction = sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight / mass_div_mm
                    Else
                        sub1.MoleFraction = 0.0#
                    End If
                Next
                Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                If W <> 0.0# Then
                    Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(Enums.FlashCalculationType.PressureEnthalpy, P, Hs, T)
                    T = tmp.CalculatedTemperature
                End If
                .Phases(0).Properties.temperature = T
                .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
            End With

        End Sub

        Public Overrides Sub DeCalculate()

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then
                With DirectCast(Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name), Thermodynamics.Streams.MaterialStream)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.molarflow = Nothing
                    .GraphicObject.Calculated = False
                End With
            End If

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Return 0

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            Return 0

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Return 0

        End Function

        Public Overrides Sub DisplayEditForm()

            Dim f As New EditingForm() With {.Text = Me.GraphicObject.Tag}

            Dim gb1 As New GroupBox() With {.Width = f.TotalWidth, .Text = "Connections", .FlatStyle = FlatStyle.Flat}
            Dim fl1 As New FlowLayoutPanel() With {.Dock = DockStyle.Fill, .FlowDirection = FlowDirection.TopDown}

            'get a list of available material streams in the flowsheet

            Dim streams = Me.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream).ToArray.Select(Function(ms) ms.GraphicObject.Tag).ToArray

            Dim height As Integer = 0
            For i As Integer = 1 To 6
                Dim p As New FlowLayoutPanel() With {.AutoSize = True, .FlowDirection = FlowDirection.LeftToRight}
                p.Controls.Add(New Label() With {.Width = f.DefaultLabelWidth, .Text = "Inlet Stream #" & i, .TextAlign = Drawing.ContentAlignment.MiddleLeft})
                Dim c As New ComboBox() With {.Width = f.DefaultEditorWidth, .DropDownStyle = ComboBoxStyle.DropDown}
                c.Items.AddRange(streams)
                AddHandler c.SelectedIndexChanged, Sub()
                                                       MsgBox("OK")
                                                   End Sub
                p.Controls.Add(c)
                p.Margin = New Padding(0)
                fl1.Controls.Add(p)
            Next

            gb1.Controls.Add(fl1)
            gb1.Height = 6 * f.DefaultRowHeight + 24
            f.Contents.Controls.Add(gb1)

            Dim gb2 As New GroupBox() With {.Width = f.TotalWidth, .Text = "Properties"}

            Dim fl2 As New FlowLayoutPanel() With {.Dock = DockStyle.Fill, .FlowDirection = FlowDirection.TopDown}

            Dim p2 As New FlowLayoutPanel() With {.AutoSize = True, .FlowDirection = FlowDirection.LeftToRight}
            p2.Controls.Add(New Label() With {.Width = f.DefaultLabelWidth, .Text = "Pressure Calculation Mode", .TextAlign = Drawing.ContentAlignment.MiddleLeft})
            Dim c2 As New ComboBox() With {.Width = f.DefaultEditorWidth, .DropDownStyle = ComboBoxStyle.DropDownList}
            c2.Items.AddRange([Enum].GetNames(Me.PressureCalculation.GetType))
            AddHandler c2.SelectedIndexChanged, Sub()
                                                    Select Case c2.SelectedIndex
                                                        Case 0
                                                            Me.PressureCalculation = PressureBehavior.Average
                                                        Case 1
                                                            Me.PressureCalculation = PressureBehavior.Maximum
                                                        Case 2
                                                            Me.PressureCalculation = PressureBehavior.Minimum
                                                    End Select
                                                End Sub

            p2.Controls.Add(c2)
            p2.Margin = New Padding(0)
            fl2.Controls.Add(p2)

            gb2.Controls.Add(fl2)
            gb2.Height = f.DefaultRowHeight + 24
            f.Contents.Controls.Add(gb2)

            Me.FlowSheet.DisplayForm(f)

        End Sub

    End Class

End Namespace
