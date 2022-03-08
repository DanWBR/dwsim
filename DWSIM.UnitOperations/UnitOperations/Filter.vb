'    Continuous Cake Filter Unit Operation Calculation Routines 
'
'    Model based on the Cake Filter equations of Chapter 29 - 
'    "Mechanical Separations" from the "Unit Operations of Chemical Engineering" 
'    book by McCabe, Smith and Harriott, Seventh Edition. 
'
'    Copyright 2013 Daniel Wagner O. de Medeiros
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

    <System.Serializable()> Public Class Filter

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Solids

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_Filter

        Protected m_ei As Double

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean
            Return MyBase.LoadData(data)
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        Public Enum CalculationMode
            Design = 0
            Simulation = 1
        End Enum

        Public Property EnergyImb As Double = 0.0#
        Public Property PressureDrop As Double = 0.0#
        Public Property TotalFilterArea As Double = 1.0#
        Public Property SubmergedAreaFraction As Double = 0.3#
        Public Property SpecificCakeResistance As Double = 10000000000.0
        Public Property FilterMediumResistance As Double = 0.000000001
        Public Property FilterCycleTime As Double = 300.0#
        Public Property CakeRelativeHumidity As Double = 0.0#
        Public Property CalcMode As CalculationMode = CalculationMode.Simulation

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Filter()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Filter)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim instr, outstr1, outstr2 As MaterialStream
            instr = Me.GetInletMaterialStream(0)
            outstr1 = Me.GetOutletMaterialStream(0)
            outstr2 = Me.GetOutletMaterialStream(1)

            'the filter doesn't support a vapor phase in the inlet stream.
            If instr.Phases(2).Properties.massflow.GetValueOrDefault > 0.0# Then
                Throw New Exception(FlowSheet.GetTranslatedString("FilterVaporPhaseNotSupported"))
            End If

            Dim W As Double = instr.Phases(0).Properties.massflow.GetValueOrDefault
            Dim Wsin As Double = instr.Phases(7).Properties.massflow.GetValueOrDefault
            Dim Wlin As Double = W - Wsin

            Dim n, At, c, alpha, Rm, f, tc, mf_mc, dp As Double

            tc = Me.FilterCycleTime
            n = 1 / tc
            f = Me.SubmergedAreaFraction
            alpha = Me.SpecificCakeResistance
            Rm = Me.FilterMediumResistance
            mf_mc = 100 / (100 - Me.CakeRelativeHumidity)

            Dim rho, mu, cf, frh, crh As Double

            rho = instr.Phases(1).Properties.density.GetValueOrDefault
            mu = instr.Phases(1).Properties.viscosity.GetValueOrDefault
            cf = instr.Phases(7).Properties.massflow.GetValueOrDefault / instr.Phases(0).Properties.volumetric_flow.GetValueOrDefault
            frh = instr.Phases(1).Properties.massflow.GetValueOrDefault / (instr.Phases(1).Properties.massflow.GetValueOrDefault + instr.Phases(7).Properties.massflow.GetValueOrDefault)
            crh = Me.CakeRelativeHumidity / 100

            If crh > frh Then
                Throw New Exception(FlowSheet.GetTranslatedString("FilterInvalidCakeHumidity"))
            End If

            c = cf / (1 - (mf_mc - 1) * cf / rho)

            Select Case CalcMode
                Case CalculationMode.Design
                    dp = Me.PressureDrop
                    At = Wsin * alpha / ((2 * c * alpha * dp * f * n / mu + (n * Rm) ^ 2) ^ 0.5 - n * Rm)
                    Me.TotalFilterArea = At
                Case CalculationMode.Simulation
                    At = Me.TotalFilterArea
                    dp = ((n * Rm) ^ 2 + (n * Rm + Wsin * alpha / At) ^ 2) / (2 * c * alpha * f * n / mu)
                    Me.PressureDrop = dp
            End Select

            Dim Wsout As Double = Wsin / (1 - crh)
            Dim Wlout As Double = W - Wsout

            Dim mw As Double

            Dim cp As IConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                With outstr1
                    .ClearAllProps()
                    .Phases(0).Properties.massflow = Wlout
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MassFlow = instr.Phases(1).Compounds(comp.Name).MassFlow * Wlout / Wlin
                        comp.MassFraction = comp.MassFlow / Wlout
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
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MassFlow = instr.Phases(1).Compounds(comp.Name).MassFlow * (Wlin - Wlout) / Wlin + instr.Phases(7).Compounds(comp.Name).MassFlow
                        comp.MassFraction = comp.MassFlow / Wsout
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

            'pass conditions

            outstr1.Phases(0).Properties.temperature = instr.Phases(0).Properties.temperature.GetValueOrDefault
            outstr1.Phases(0).Properties.pressure = instr.Phases(0).Properties.pressure.GetValueOrDefault - dp
            outstr2.Phases(0).Properties.temperature = instr.Phases(0).Properties.temperature.GetValueOrDefault
            outstr2.Phases(0).Properties.pressure = instr.Phases(0).Properties.pressure.GetValueOrDefault - dp

            'do a flash calculation on streams to calculate energy imbalance

            outstr1.PropertyPackage.CurrentMaterialStream = outstr1
            IObj?.SetCurrent()
            outstr1.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
            outstr2.PropertyPackage.CurrentMaterialStream = outstr2
            IObj?.SetCurrent()
            outstr2.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)

            Dim Hi, Ho1, Ho2, Wi, Wo1, Wo2 As Double

            Hi = instr.Phases(0).Properties.enthalpy.GetValueOrDefault
            Wi = instr.Phases(0).Properties.massflow.GetValueOrDefault
            Ho1 = outstr1.Phases(0).Properties.enthalpy.GetValueOrDefault
            Wo1 = outstr1.Phases(0).Properties.massflow.GetValueOrDefault
            Ho2 = outstr2.Phases(0).Properties.enthalpy.GetValueOrDefault
            Wo2 = outstr2.Phases(0).Properties.massflow.GetValueOrDefault

            'calculate imbalance

            Me.EnergyImb = Hi * Wi - Ho1 * Wo1 - Ho2 * Wo2

            'update energy stream power value

            If GetEnergyStream() IsNot Nothing Then
                With Me.GetEnergyStream
                    .EnergyFlow = Me.EnergyImb
                    .GraphicObject.Calculated = True
                End With
            End If

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

            'energy stream - update energy flow value (kW)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With GetEnergyStream()
                    .EnergyFlow = Nothing
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
                    Case 0
                        'PROP_FT_0	Energy Balance	
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.EnergyImb)
                    Case 1
                        'PROP_FT_1	Total Filter Area	
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.TotalFilterArea)
                    Case 2
                        'PROP_FT_2	Cake Relative Humidity (%)	
                        value = Me.CakeRelativeHumidity
                    Case 3
                        'PROP_FT_3	Cycle Time	
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.time, Me.FilterCycleTime)
                    Case 4
                        'PROP_FT_4	Filter Medium Resistance	
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.mediumresistance, Me.FilterMediumResistance)
                    Case 5
                        'PROP_FT_5	Specific Cake Resistance	
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.cakeresistance, Me.SpecificCakeResistance)
                    Case 6
                        'PROP_FT_6	Submerged Area Fraction	
                        value = Me.SubmergedAreaFraction
                    Case 7
                        'PROP_FT_7	Total Pressure Drop	
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.PressureDrop)
                End Select

                Return value

            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            For i = 0 To 7
                proplist.Add("PROP_FT_" + CStr(i))
            Next
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    'PROP_FT_0	Energy Balance	
                Case 1
                    'PROP_FT_1	Total Filter Area	
                    Me.TotalFilterArea = SystemsOfUnits.Converter.ConvertToSI(su.area, propval)
                Case 2
                    'PROP_FT_2	Cake Relative Humidity (%)	
                    Me.CakeRelativeHumidity = propval
                Case 3
                    'PROP_FT_3	Cycle Time	
                    Me.FilterCycleTime = SystemsOfUnits.Converter.ConvertToSI(su.time, propval)
                Case 4
                    'PROP_FT_4	Filter Medium Resistance	
                    Me.FilterMediumResistance = SystemsOfUnits.Converter.ConvertToSI(su.mediumresistance, propval)
                Case 5
                    'PROP_FT_5	Specific Cake Resistance	
                    Me.SpecificCakeResistance = SystemsOfUnits.Converter.ConvertToSI(su.cakeresistance, propval)
                Case 6
                    'PROP_FT_6	Submerged Area Fraction	
                    Me.SubmergedAreaFraction = propval
                Case 7
                    'PROP_FT_7	Total Pressure Drop	
                    Me.PressureDrop = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
            End Select

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String

            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then

                Return u0

            Else

                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As String = ""
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx
                    Case 0
                        'PROP_FT_0	Energy Balance	
                        value = su.heatflow
                    Case 1
                        'PROP_FT_1	Total Filter Area	
                        value = su.area
                    Case 2
                        'PROP_FT_2	Cake Relative Humidity (%)	
                        value = "%"
                    Case 3
                        'PROP_FT_3	Cycle Time	
                        value = su.time
                    Case 4
                        'PROP_FT_4	Filter Medium Resistance	
                        value = su.mediumresistance
                    Case 5
                        'PROP_FT_5	Specific Cake Resistance	
                        value = su.cakeresistance
                    Case 6
                        'PROP_FT_6	Submerged Area Fraction	
                        value = ""
                    Case 7
                        'PROP_FT_7	Total Pressure Drop	
                        value = su.deltaP
                End Select

                Return value

            End If

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_Filter With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Filter With {.SimObject = Me}
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
            Return My.Resources.uo_filter_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("FILTER_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("FILTER_Name")
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

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            Dim istr, ostr As MaterialStream
            istr = Me.GetInletMaterialStream(0)
            ostr = Me.GetOutletMaterialStream(0)

            istr.PropertyPackage.CurrentMaterialStream = istr

            str.AppendLine("Solids Filter: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Inlet conditions")
            str.AppendLine()
            str.AppendLine("    Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, istr.Phases(0).Properties.temperature.GetValueOrDefault).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, istr.Phases(0).Properties.pressure.GetValueOrDefault).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Mass flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.massflow, istr.Phases(0).Properties.massflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.massflow)
            str.AppendLine("    Mole flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, istr.Phases(0).Properties.molarflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.molarflow)
            str.AppendLine("    Volumetric flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, istr.Phases(0).Properties.volumetric_flow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.volumetricFlow)
            str.AppendLine("    Vapor fraction: " & istr.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine("    Compounds: " & istr.PropertyPackage.RET_VNAMES.ToArrayString)
            str.AppendLine("    Molar composition: " & istr.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString(ci))
            str.AppendLine()
            str.AppendLine("Parameters")
            str.AppendLine()
            str.AppendLine("    Calculation mode: " & CalcMode.ToString)
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            str.AppendLine("    Energy Balance: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.EnergyImb).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine("    Total Filter Area: " & SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.TotalFilterArea).ToString(numberformat, ci) & " " & su.area)
            str.AppendLine("    Cake Relative Humidity (%): " & Me.CakeRelativeHumidity.ToString(numberformat, ci))
            str.AppendLine("    Cycle Time: " & SystemsOfUnits.Converter.ConvertFromSI(su.time, Me.FilterCycleTime).ToString(numberformat, ci) & " " & su.time)
            str.AppendLine("    Filter Medium Resistance: " & SystemsOfUnits.Converter.ConvertFromSI(su.mediumresistance, Me.FilterMediumResistance).ToString(numberformat, ci) & " " & su.mediumresistance)
            str.AppendLine("    Specific Cake Resistance: " & SystemsOfUnits.Converter.ConvertFromSI(su.cakeresistance, Me.SpecificCakeResistance).ToString(numberformat, ci) & " " & su.cakeresistance)
            str.AppendLine("    Submerged Area Fraction: " & Me.SubmergedAreaFraction.ToString(numberformat, ci))
            str.AppendLine("    Total Pressure Drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.PressureDrop).ToString(numberformat, ci) & " " & su.deltaP)

            Return str.ToString

        End Function


    End Class

End Namespace

