'    Valve Calculation Routines 
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


Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums

Namespace UnitOperations

    <System.Serializable()> Public Class Valve

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.PressureChangers

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_Valve

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)
        Protected m_Pout As Nullable(Of Double) = 101325.0#
        Protected m_cmode As CalculationMode = CalculationMode.DeltaP
        Public Property Hinlet As Double
        Public Property Houtlet As Double

        Public Property Kv As Double = 100.0#

        Public Property OpeningPct As Double = 50.0

        Public Property PercentOpeningVersusPercentKvExpression As String = "1.0*OP"

        Public Property EnableOpeningKvRelationship As Boolean = False

        Public Enum CalculationMode
            DeltaP = 0
            OutletPressure = 1
            Kv_Liquid = 2
            Kv_Gas = 3
            Kv_Steam = 4
        End Enum

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Valve()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Valve)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Property OutletPressure() As Nullable(Of Double)
            Get
                Return m_Pout
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_Pout = value
            End Set
        End Property

        Public Property CalcMode() As CalculationMode
            Get
                Return m_cmode
            End Get
            Set(ByVal value As CalculationMode)
                m_cmode = value
            End Set
        End Property

        Public Property DeltaP() As Nullable(Of Double)
            Get
                Return m_dp
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dp = value
            End Set
        End Property

        Public Property DeltaT() As Nullable(Of Double)
            Get
                Return m_dt
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dt = value
            End Set
        End Property

        Public Property DeltaQ() As Nullable(Of Double)
            Get
                Return m_DQ
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_DQ = value
            End Set
        End Property

        Property OutletTemperature As Double

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The Valve works like a fixed pressure drop for the process, where 
                                the outlet material stream properties are calculated beginning 
                                from the principle that the expansion is an isenthalpic process.")

            IObj?.Paragraphs.Add("The outlet stream pressure is calculated from the inlet pressure 
                                and the pressure drop. The outlet stream temperature is found by 
                                doing a PH Flash. This way, in the majority of cases, the outlet 
                                temperature will be less than or equal to the inlet one.")

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim Ti, Pi, Hi, Wi, ei, ein, T2, P2, H2, H2c, rho, volf, rhog20, P2ant, v2, Kvc As Double
            Dim icount As Integer

            Dim ims As MaterialStream = Me.GetInletMaterialStream(0)

            Me.PropertyPackage.CurrentMaterialStream = ims
            Me.PropertyPackage.CurrentMaterialStream.Validate()
            Ti = ims.Phases(0).Properties.temperature.GetValueOrDefault.ToString
            Pi = ims.Phases(0).Properties.pressure.GetValueOrDefault.ToString
            Hi = ims.Phases(0).Properties.enthalpy.GetValueOrDefault.ToString
            Wi = ims.Phases(0).Properties.massflow.GetValueOrDefault.ToString
            ei = Hi * Wi
            ein = ei
            rho = ims.Phases(0).Properties.density.GetValueOrDefault
            volf = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault

            H2 = Hi '- Me.DeltaP.GetValueOrDefault / (rho_li * 1000)

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, W = {3} kg/s", Ti, Pi, Hi, Wi))

            If EnableOpeningKvRelationship Then
                IObj?.Paragraphs.Add("<h2>Opening/Kv relationship</h2>")
                IObj?.Paragraphs.Add("When this feature is enabled, you can enter an expression that relates the valve stem opening with the maximum flow value (Kvmax).")
                IObj?.Paragraphs.Add("The relationship between control valve capacity and valve stem travel is known as the Flow Characteristic of the 
                                    Control Valve. Trim design of the valve affects how the control valve capacity changes as the valve moves through 
                                    its complete travel. Because of the variation in trim design, many valves are not linear in nature. Valve trims 
                                    are instead designed, or characterized, in order to meet the large variety of control application needs. Many 
                                    control loops have inherent non linearity's, which may be possible to compensate selecting the control valve trim.")
                IObj?.Paragraphs.Add("<img src='https://www.engineeringtoolbox.com/docs/documents/485/Control_Valve_Flow_Characteristics.gif'></img>")
                Try
                    Dim ExpContext As New Ciloci.Flee.ExpressionContext
                    ExpContext.Imports.AddType(GetType(System.Math))
                    ExpContext.Variables.Clear()
                    ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                    ExpContext.Variables.Add("OP", OpeningPct)
                    IObj?.Paragraphs.Add("Current Opening (%): " & OpeningPct)
                    IObj?.Paragraphs.Add("Opening/Kvmax relationship expression: " & PercentOpeningVersusPercentKvExpression)
                    Dim Expr = ExpContext.CompileGeneric(Of Double)(PercentOpeningVersusPercentKvExpression)
                    Kvc = Kv * Expr.Evaluate() / 100
                    IObj?.Paragraphs.Add("Calculated Kv/Kvmax (%): " & Kvc / Kv * 100)
                    IObj?.Paragraphs.Add("Calculated Kv: " & Kvc)
                Catch ex As Exception
                    Throw New Exception("Invalid expression for Kv/Opening relationship.")
                End Try
            Else
                Kvc = Kv
            End If

            'reference: https://www.samson.de/document/t00050en.pdf

            If CalcMode = CalculationMode.Kv_Gas Or CalcMode = CalculationMode.Kv_Liquid Or CalcMode = CalculationMode.Kv_Steam Then
                IObj?.Paragraphs.Add("<h2>Kv Calculation Mode</h2>")
                IObj?.Paragraphs.Add("Kv flow equations in DWSIM are implemented as per IEC 60534 for non-critical flow (P2 > 0.5*P1).")
                IObj?.Paragraphs.Add("For more information, see <a href='https://www.samson.de/document/t00050en.pdf'>this document</a>.")
                IObj?.Paragraphs.Add(String.Format("Kv = {0}", Kvc))
            End If

            If CalcMode = CalculationMode.Kv_Liquid Then
                P2 = Pi / 100000.0 - 1 / (1000.0 * rho) * (Wi * 3600 / Kvc) ^ 2
                P2 = P2 * 100000.0
                IObj?.Paragraphs.Add(String.Format("Calculated Outlet Pressure P2 = {0} Pa", P2))
            ElseIf CalcMode = CalculationMode.Kv_Gas Then
                ims.PropertyPackage.CurrentMaterialStream = ims
                rhog20 = ims.PropertyPackage.AUX_VAPDENS(273.15, 101325)
                P2 = Pi * 0.7 / 100000.0
                icount = 0
                Do
                    P2ant = P2
                    P2 = Pi / 100000.0 - Ti / rhog20 / P2ant * (519 * Kvc / (Wi * 3600)) ^ -2
                    icount += 1
                    If icount > 1000 Then Throw New Exception("P2 did not converge in 1000 iterations.")
                Loop Until Math.Abs(P2 - P2ant) < 0.0001
                P2 = P2 * 100000.0
                IObj?.Paragraphs.Add(String.Format("Calculated Outlet Pressure P2 = {0} Pa", P2))
            ElseIf CalcMode = CalculationMode.Kv_Steam Then
                P2 = Pi * 0.7 / 100000.0
                icount = 0
                Do
                    v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P2)
                    P2ant = P2
                    P2 = Pi / 100000.0 - v2 * (31.62 * Kvc / (Wi * 3600)) ^ -2
                    icount += 1
                    If icount > 1000 Then Throw New Exception("P2 did not converge in 1000 iterations.")
                Loop Until Math.Abs(P2 - P2ant) < 0.0001
                P2 = P2 * 100000.0
                IObj?.Paragraphs.Add(String.Format("Calculated Outlet Pressure P2 = {0} Pa", P2))
            End If

            If Me.CalcMode = CalculationMode.DeltaP Then
                P2 = Pi - Me.DeltaP.GetValueOrDefault
            ElseIf CalcMode = CalculationMode.OutletPressure Then
                P2 = Me.OutletPressure.GetValueOrDefault
                Me.DeltaP = Pi - P2
            Else
                DeltaP = Pi - P2
                OutletPressure = P2
            End If

            CheckSpec(P2, True, "outlet pressure")

            If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

            IObj?.Paragraphs.Add(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

            IObj?.Paragraphs.Add(String.Format("Inlet Stream Enthalpy = {0} kJ/kg", Hi))

            IObj?.SetCurrent()
            Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)
            T2 = tmp.CalculatedTemperature
            CheckSpec(T2, True, "outlet temperature")
            H2c = tmp.CalculatedEnthalpy
            CheckSpec(H2c, False, "outlet enthalpy")

            If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

            IObj?.Paragraphs.Add(String.Format("Outlet Stream Enthalpy = {0} kJ/kg", H2c))
            IObj?.Paragraphs.Add(String.Format("Calculated Outlet Temperature T2 = {0} K", T2))

            Houtlet = H2c
            Hinlet = Hi

            'Dim htol As Double = Me.PropertyPackage.Parameters("PP_PHFELT")
            'Dim herr As Double = Math.Abs((H2c - H2) / H2)

            'If herr > 0.01 Then Throw New Exception("The enthalpy of inlet and outlet streams doesn't match. Result is invalid.")

            Me.DeltaT = T2 - Ti
            Me.DeltaQ = 0

            OutletTemperature = T2

            If Not DebugMode Then

                With Me.GetOutletMaterialStream(0)
                    .Phases(0).Properties.temperature = T2
                    .Phases(0).Properties.pressure = P2
                    .Phases(0).Properties.enthalpy = H2
                    Dim comp As BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = ims.Phases(0).Compounds(comp.Name).MoleFraction
                        comp.MassFraction = ims.Phases(0).Compounds(comp.Name).MassFraction
                        i += 1
                    Next
                    .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
                    .Phases(0).Properties.massflow = ims.Phases(0).Properties.massflow.GetValueOrDefault
                End With

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If

            IObj?.Close()

        End Sub


        Public Overrides Sub DeCalculate()

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de materia conectada a jusante
                With Me.GetOutletMaterialStream(0)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.molarflow = Nothing
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
                        'PROP_VA_0	Calculation Mode
                        value = Me.CalcMode
                    Case 1
                        'PROP_VA_1	Pressure Drop
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                    Case 2
                        'PROP_VA_2	Outlet Pressure
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure.GetValueOrDefault)
                    Case 3
                        'PROP_VA_3	Temperature Drop
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
                    Case 5
                        value = Kv
                    Case 6
                        value = OpeningPct
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
                Case PropertyType.RO
                    For i = 3 To 3
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 5
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 5
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 5
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
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
                Case 0
                    'PROP_VA_0	Calculation Mode
                    Me.CalcMode = propval
                Case 1
                    'PROP_VA_1	Pressure Drop
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 2
                    'PROP_VA_2	Outlet Pressure
                    Me.OutletPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                Case 4
                    Me.Kv = propval
                Case 5
                    Me.OpeningPct = propval
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String

            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 = "NF" Then

                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim value As String = ""
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx

                    Case 0, 4, 5
                        'PROP_VA_0	Calculation Mode
                        value = ""
                    Case 1
                        'PROP_VA_1	Pressure Drop
                        value = su.deltaP
                    Case 2
                        'PROP_VA_2	Outlet Pressure
                        value = su.pressure
                    Case 3
                        'PROP_VA_3	Temperature Drop
                        value = su.deltaT

                End Select

                Return value

            Else

                Return u0

            End If

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_Valve With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Valve With {.SimObject = Me}
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
            Return My.Resources.uo_valve_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("VALVE_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("VALVE_Name")
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

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            Dim istr, ostr As MaterialStream
            istr = Me.GetInletMaterialStream(0)
            ostr = Me.GetOutletMaterialStream(0)

            istr.PropertyPackage.CurrentMaterialStream = istr

            str.AppendLine("Adiabatic Valve: " & Me.GraphicObject.Tag)
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
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Calculation mode: " & CalcMode.ToString)
            Select Case Me.CalcMode
                Case CalculationMode.DeltaP
                    str.AppendLine("    Pressure decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure).ToString(numberformat, ci) & " " & su.pressure)
                Case Else
                    str.AppendLine("    Kv(max): " & Kv)
            End Select
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case Me.CalcMode
                Case CalculationMode.DeltaP
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure).ToString(numberformat, ci) & " " & su.pressure)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Pressure decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
                Case Else
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure).ToString(numberformat, ci) & " " & su.pressure)
                    str.AppendLine("    Pressure decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
            End Select
            str.AppendLine("    Inlet enthalpy: " & SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Hinlet).ToString(numberformat, ci) & " " & su.enthalpy)
            str.AppendLine("    Outlet enthalpy: " & SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Houtlet).ToString(numberformat, ci) & " " & su.enthalpy)
            str.AppendLine("    Temperature decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT).ToString(numberformat, ci) & " " & su.deltaT)

            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Adiabatic Valve '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Calculation Mode",
                    CalcMode.ToString}))

            Select Case CalcMode
                Case CalculationMode.DeltaP
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Increase",
                            Me.DeltaP.GetValueOrDefault.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
                Case CalculationMode.OutletPressure
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.OutletPressure.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                Case Else
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Kv (max)",
                            Me.Kv.ToString(nf),
                            ""}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            Select Case CalcMode
                Case CalculationMode.DeltaP
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.OutletPressure.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                Case CalculationMode.OutletPressure
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Increase",
                            Me.DeltaP.GetValueOrDefault.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Temperature Change",
                            Me.DeltaT.GetValueOrDefault.ConvertFromSI(su.deltaT).ToString(nf),
                            su.deltaT}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Inlet Enthalpy",
                            Me.Hinlet.ConvertFromSI(su.enthalpy).ToString(nf),
                            su.enthalpy}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Enthalpy",
                            Me.Houtlet.ConvertFromSI(su.enthalpy).ToString(nf),
                            su.enthalpy}))

            Return list

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Calculation Mode") Then
                Return "Select the calculation mode of this valve."
            ElseIf p.Equals("Pressure Drop") Then
                Return "If you chose 'Pressure Drop' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            ElseIf p.Equals("Outlet Pressure") Then
                Return "If you chose 'Outlet Pressure' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace

