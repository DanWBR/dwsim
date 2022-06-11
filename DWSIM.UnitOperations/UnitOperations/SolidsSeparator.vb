'    Solids Separator Calculation Routines 
'    Copyright 2013 Daniel Wagner O. de Medeiros
'    Copyright 2021 Gregor Reichert
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


Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums

Namespace UnitOperations

    <System.Serializable()> Public Class SolidsSeparator

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Solids

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_SolidsSep

        Public Property EmbeddedImageData As String = ""

        Public Property UseEmbeddedImage As Boolean = False

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean
            Return MyBase.LoadData(data)
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        Public Property SeparationEfficiency() As Double = 100.0#
        Public Property LiquidSeparationEfficiency() As Double = 100.0#

        Public Overrides Sub PerformPostCalcValidation()

        End Sub

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New SolidsSeparator()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of SolidsSeparator)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The solids separator is used to separate solids from a liquid phase in a mixed material stream. 
                                  <br><br>Liquid and vapor phases are sent into outlet 1 and solid phase into outlet 2. 
                                  <br>The solid and liquid phases are split between both outlets according to specified efficiencies. The vapor phase is always sent to outlet 1 completely.")

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim instr, outstr1, outstr2 As MaterialStream
            instr = GetInletMaterialStream(0)
            outstr1 = GetOutletMaterialStream(0)
            outstr2 = GetOutletMaterialStream(1)

            Dim W As Double = instr.Phases(0).Properties.massflow.GetValueOrDefault
            Dim Wsin As Double = instr.Phases(7).Properties.massflow.GetValueOrDefault
            Dim Wlin As Double = instr.Phases(1).Properties.massflow.GetValueOrDefault
            Dim Wvin As Double = instr.Phases(2).Properties.massflow.GetValueOrDefault
            Dim HVin As Double = instr.Phases(2).Properties.enthalpy.GetValueOrDefault
            Dim HLin As Double = instr.Phases(1).Properties.enthalpy.GetValueOrDefault
            Dim HSin As Double = instr.Phases(7).Properties.enthalpy.GetValueOrDefault

            Dim sse, lse As Double
            sse = Me.SeparationEfficiency / 100
            lse = Me.LiquidSeparationEfficiency / 100
            Dim Wsout As Double = sse * Wsin + (1 - lse) * Wlin
            Dim Wlvout As Double = (1 - sse) * Wsin + lse * Wlin + Wvin

            IObj?.Paragraphs.Add("<hr><h3>Input Variables</h3>")
            IObj?.Paragraphs.Add(String.Format("<b><i>Solid separation efficiency:</i></b> {0} <br><b><i>Liquid separation efficiency:</i></b> {1}", sse, lse))
            IObj?.Paragraphs.Add(String.Format("<b><i>Solid mass flow:</i></b> {0} Kg/s <br><b><i>Solid phase enthalpy:</i></b> {1} KJ/Kg", Wsin, HSin))
            IObj?.Paragraphs.Add(String.Format("<b><i>Liquid mass flow:</i></b> {0} Kg/s <br><b><i>Liquid phase enthalpy:</i></b> {1} KJ/Kg", Wlin, HLin))
            IObj?.Paragraphs.Add(String.Format("<b><i>Vapor mass flow:</i></b> {0} Kg/s <br><b><i>Vapor phase enthalpy:</i></b> {1} KJ/Kg", Wvin, HVin))

            Dim mw As Double

            Dim cp As IConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                With outstr1
                    .ClearAllProps()
                    .Phases(0).Properties.massflow = Wlvout
                    .DefinedFlow = FlowSpec.Mass
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MassFlow = (1 - sse) * instr.Phases(7).Compounds(comp.Name).MassFlow + instr.Phases(2).Compounds(comp.Name).MassFlow
                        comp.MassFlow += lse * (instr.Phases(3).Compounds(comp.Name).MassFlow + instr.Phases(4).Compounds(comp.Name).MassFlow)
                        comp.MassFraction = comp.MassFlow / Wlvout
                    Next
                    mw = 0.0#
                    For Each comp In .Phases(0).Compounds.Values
                        mw += comp.MassFraction / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = comp.MassFraction / comp.ConstantProperties.Molar_Weight / mw
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight / 1000
                    Next
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                With outstr2
                    .ClearAllProps()
                    .Phases(0).Properties.massflow = Wsout
                    .DefinedFlow = FlowSpec.Mass
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MassFlow = sse * instr.Phases(7).Compounds(comp.Name).MassFlow.GetValueOrDefault + (1 - lse) * (instr.Phases(3).Compounds(comp.Name).MassFlow + instr.Phases(4).Compounds(comp.Name).MassFlow)
                        comp.MassFraction = If(Wsout > 0.0#, comp.MassFlow / Wsout, 0.0#)
                    Next
                    mw = 0.0#
                    For Each comp In .Phases(0).Compounds.Values
                        mw += comp.MassFraction / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = If(mw > 0.0#, comp.MassFraction / comp.ConstantProperties.Molar_Weight / mw, 0.0#)
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight / 1000
                    Next
                End With
            End If

            'pass conditions

            outstr1.Phases(0).Properties.temperature = instr.Phases(0).Properties.temperature.GetValueOrDefault
            outstr1.Phases(0).Properties.pressure = instr.Phases(0).Properties.pressure.GetValueOrDefault
            outstr2.Phases(0).Properties.temperature = instr.Phases(0).Properties.temperature.GetValueOrDefault
            outstr2.Phases(0).Properties.pressure = instr.Phases(0).Properties.pressure.GetValueOrDefault

            outstr1.Phases(0).Properties.enthalpy = (HVin * Wvin + HLin * Wlin * lse + HSin * Wsin * (1 - sse)) / Wlvout
            outstr2.Phases(0).Properties.enthalpy = (HSin * Wsin * sse + HLin * Wlin * (1 - lse)) / Wsout
            outstr1.SpecType = StreamSpec.Pressure_and_Enthalpy
            outstr2.SpecType = StreamSpec.Pressure_and_Enthalpy

            IObj?.Paragraphs.Add("<hr><h3>Results</h3>")
            IObj?.Paragraphs.Add(String.Format("Flash specs of outlet streams are set to PH. Enthalpies are defined to maintain phase fractions."))
            IObj?.Paragraphs.Add(String.Format("<b><i>Massflow Outlet 1</i></b>: {0} Kg/s <br><b><i>Enthalpy Outlet 1</i></b>: {1} KJ/Kg", Wlvout, outstr1.Phases(0).Properties.enthalpy))
            IObj?.Paragraphs.Add(String.Format("<b><i>Massflow Outlet 2</i></b>: {0} Kg/s <br><b><i>Enthalpy Outlet 2</i></b>: {1} KJ/Kg", Wsout, outstr2.Phases(0).Properties.enthalpy))

            IObj?.Close()

        End Sub

        Public Overrides Sub DeCalculate()

            Dim j As Integer = 0

            Dim cp As IConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                With GetOutletMaterialStream(0)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                With GetOutletMaterialStream(1)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As Double = 0
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx
                    Case 1
                        value = Me.SeparationEfficiency
                    Case 2
                        value = Me.LiquidSeparationEfficiency
                End Select

                Return value
            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            Select Case proptype
                Case PropertyType.RW
                    'For i = 0 To 0
                    '    proplist.Add("PROP_SS_" + CStr(i))
                    'Next
                Case PropertyType.WR
                    For i = 1 To 2
                        proplist.Add("PROP_SS_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 1 To 2
                        proplist.Add("PROP_SS_" + CStr(i))
                    Next
                Case PropertyType.RO
                    'For i = 0 To 0
                    '    proplist.Add("PROP_SS_" + CStr(i))
                    'Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 1
                    'PROP_SS_1	Solid Separation Efficiency
                    Me.SeparationEfficiency = propval
                Case 2
                    'PROP_SS_2	Liquid Separation Efficiency
                    Me.LiquidSeparationEfficiency = propval
            End Select

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then
                Return u0
            Else
                'If su Is Nothing Then su = New SystemsOfUnits.SI
                'Dim cv As New SystemsOfUnits.Converter
                Dim value As String = "%"
                'Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Return value
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_SolidsSep With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_SolidsSep With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
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
            Return My.Resources.uo_solidsep_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("SSEP_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("SSEP_Name")
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
                Return False
            End Get
        End Property
    End Class

End Namespace
