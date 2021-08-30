'    ISO 5167 Orifice Plate Calculation Routines 
'    Copyright 2010 Daniel Wagner O. de Medeiros
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

    <System.Serializable()> Public Class OrificePlate

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.PressureChangers

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_OrificePlate

        Public Enum CalcMethod
            Homogeneous = 0
            Slip = 1
        End Enum

        Public Enum OrificeType
            CornerTaps = 0
            FlangeTaps = 1
            RadiusTaps = 2
        End Enum

        Protected m_dt As Nullable(Of Double)
        Protected _orificeDP As Double = 0.0
        Protected _fluidDP As Double = 0.0
        Protected _beta As Double = 0.5
        Protected _orificediameter As Double = 100.0# / 1000.0
        Protected _internaldiameter As Double = 200.0# / 1000.0
        Protected _orificetype As OrificeType = OrificeType.FlangeTaps
        Protected _calcmethod As CalcMethod
        Protected _corrfactor As Double = 1.0

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New OrificePlate()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of OrificePlate)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Property OrifType() As OrificeType
            Get
                Return _orificetype
            End Get
            Set(ByVal value As OrificeType)
                _orificetype = value
            End Set
        End Property

        Public Property OverallPressureDrop() As Double
            Get
                Return _fluidDP
            End Get
            Set(ByVal value As Double)
                _fluidDP = value
            End Set
        End Property

        Public Property OrificePressureDrop() As Double
            Get
                Return _orificeDP
            End Get
            Set(ByVal value As Double)
                _orificeDP = value
            End Set
        End Property

        Public Property CorrectionFactor() As Double
            Get
                Return _corrfactor
            End Get
            Set(ByVal value As Double)
                _corrfactor = value
            End Set
        End Property

        Public Property CalculationMethod() As CalcMethod
            Get
                Return _calcmethod
            End Get
            Set(ByVal value As CalcMethod)
                _calcmethod = value
            End Set
        End Property

        Public Property Beta() As Double
            Get
                Return _beta
            End Get
            Set(ByVal value As Double)
                _beta = value
            End Set
        End Property

        Public Property OrificeDiameter() As Double
            Get
                Return _orificediameter
            End Get
            Set(ByVal value As Double)
                _orificediameter = value
            End Set
        End Property
        Public Property InternalPipeDiameter() As Double
            Get
                Return _internaldiameter
            End Get
            Set(ByVal value As Double)
                _internaldiameter = value
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

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Sub RunDynamicModel()

            Calculate()

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim Ti, Pi, Wi, T2, P2, H2, H1, xv, xl, wv, wl As Double
            Dim rhom, mum, rhov, rhol, muv, mul As Double

            Dim instr, outstr As MaterialStream

            instr = Me.GetInletMaterialStream(0)
            outstr = Me.GetOutletMaterialStream(0)

            Me.PropertyPackage.CurrentMaterialStream = instr
            Ti = instr.Phases(0).Properties.temperature.GetValueOrDefault
            Pi = instr.Phases(0).Properties.pressure.GetValueOrDefault
            Wi = instr.Phases(0).Properties.massflow.GetValueOrDefault
            H1 = instr.Phases(0).Properties.enthalpy.GetValueOrDefault
            rhom = instr.Phases(0).Properties.density.GetValueOrDefault
            rhov = instr.Phases(2).Properties.density.GetValueOrDefault
            rhol = instr.Phases(1).Properties.density.GetValueOrDefault
            muv = instr.Phases(2).Properties.viscosity.GetValueOrDefault
            mul = instr.Phases(1).Properties.viscosity.GetValueOrDefault
            xv = instr.Phases(2).Properties.molarfraction.GetValueOrDefault
            xl = instr.Phases(1).Properties.molarfraction.GetValueOrDefault
            wv = instr.Phases(2).Properties.massfraction.GetValueOrDefault
            wl = instr.Phases(1).Properties.massfraction.GetValueOrDefault
            If xv > 0 And xl > 0 Then
                mum = (xv / muv + xl / mul) ^ -1
            ElseIf xl = 0 Then
                mum = muv
            ElseIf xv = 0 Then
                mum = mul
            End If

            Dim beta, A1, A2, s2_s1, L1, L2 As Double

            beta = _beta
            A1 = 3.1416 * _internaldiameter ^ 2 / 4
            A2 = 3.1416 * _orificediameter ^ 2 / 4

            Select Case _orificetype

                Case OrificeType.CornerTaps

                    'placa de orificio corner taps

                    s2_s1 = 0
                    L1 = 0
                    L2 = 0

                Case OrificeType.FlangeTaps

                    'placa de orificio flange taps

                    s2_s1 = 0.0508
                    L1 = 1 / (_orificediameter / 0.0254)
                    L2 = 1 / (_orificediameter / 0.0254)

                Case OrificeType.RadiusTaps

                    'placa de orificio radius taps

                    s2_s1 = 1.5 * _orificediameter
                    L1 = 1

                    L2 = 0.47

            End Select

            Dim ReD, Cd, DP, c1, c2, c3, M2, A As Double

            ReD = Wi * _internaldiameter / (A1 * mum)

            M2 = 2 * L2 / (1 - beta)
            A = (19000 * beta / ReD) ^ 0.8

            c1 = 0.5961 + 0.0261 * beta ^ 2 - 0.216 * beta ^ 8 + 0.000521 * (10 ^ 6 * beta / ReD) ^ 0.7 + (0.0188 + 0.0063 * A) * beta ^ 3.5 * (10 ^ 6 / ReD) ^ 0.3
            c2 = (0.043 + 0.08 * Math.Exp(-10 * L1) - 0.123 * Math.Exp(-7 * L1)) * (1 - 0.11 * A) * beta ^ 4 / (1 - beta ^ 4) - 0.031 * (M2 - 0.8 * M2 ^ 1.1) * beta ^ 1.3
            If _internaldiameter * 1000 < 71.12 Then
                c3 = 0.011 * (0.75 - beta) * (2.8 - (_internaldiameter * 1000) / 25.4)
            Else
                c3 = 0
            End If
            Cd = c1 + c2 + c3

            'DP = (Wi / (_corrfactor * Cd * A2)) ^ 2 * (1 - beta ^ 4) / (2 * rhom)
            DP = rhom / 2 * (Wi / rhom / (_corrfactor * Cd / (1 - beta ^ 4) ^ 0.5 * A2)) ^ 2
            DP = DP + (rhom * 9.8 * (s2_s1))

            _orificeDP = DP
            If beta >= 1.0# Then
                _fluidDP = DP
            Else
                _fluidDP = DP * ((1 - beta ^ 4 * (1 - Cd ^ 2)) ^ 0.5 - Cd * beta ^ 2) / ((1 - beta ^ 4 * (1 - Cd ^ 2)) ^ 0.5 + Cd * beta ^ 2)
            End If

            P2 = Pi - _fluidDP
            H2 = H1

            If P2 <= 0 Then
                Throw New Exception("Error: Negative Pressure! Orifice Plate to small for inlet stream.")
            End If

            IObj?.SetCurrent()
            Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)
            T2 = tmp.CalculatedTemperature

            Me.DeltaT = T2 - Ti

            'Atribuir valores a corrente de materia conectada a jusante
            With outstr
                .Phases(0).Properties.temperature = T2
                .Phases(0).Properties.pressure = P2
                .Phases(0).Properties.enthalpy = H2
                Dim comp As BaseClasses.Compound
                Dim i As Integer = 0
                For Each comp In .Phases(0).Compounds.Values
                    comp.MoleFraction = instr.Phases(0).Compounds(comp.Name).MoleFraction
                    comp.MassFraction = instr.Phases(0).Compounds(comp.Name).MassFraction
                    i += 1
                Next
                .Phases(0).Properties.massflow = instr.Phases(0).Properties.massflow.GetValueOrDefault
                .SpecType = StreamSpec.Pressure_and_Enthalpy
            End With

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
                        'PROP_OP_0	Orifice Type	1
                        value = Me.OrifType
                    Case 1
                        'PROP_OP_1	Orifice Diameter	1
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.OrificeDiameter)
                    Case 2
                        'PROP_OP_2	Internal Pipe Diameter	1
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.InternalPipeDiameter)
                    Case 3
                        'PROP_OP_3	Correction Factor	1
                        value = Me.CorrectionFactor
                    Case 4
                        'PROP_OP_4	Beta (d/D)	1
                        value = Me.Beta
                    Case 5
                        'PROP_OP_5	Overall Pressure Drop	0
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.OverallPressureDrop)
                    Case 6
                        'PROP_OP_6	Orifice Pressure Drop	0
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.OrificePressureDrop)
                    Case 7
                        'PROP_OP_7	Delta T	0
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
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
                    For i = 4 To 7
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 7
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 3
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 7
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
            'PROP_OP_0	Orifice Type	1
            'PROP_OP_1	Orifice Diameter	1
            'PROP_OP_2	Internal Pipe Diameter	1
            'PROP_OP_3	Correction Factor	1
            'PROP_OP_4	Beta (d/D)	1
            'PROP_OP_5	Overall Pressure Drop	0
            'PROP_OP_6	Orifice Pressure Drop	0
            'PROP_OP_7	Delta T	0

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    'PROP_OP_0	Orifice Type	1
                    Me.OrifType = propval
                Case 1
                    'PROP_OP_1	Orifice Diameter	1
                    Me.OrificeDiameter = SystemsOfUnits.Converter.ConvertToSI(su.diameter, propval)
                    Me.Beta = Me.OrificeDiameter / Me.InternalPipeDiameter
                Case 2
                    'PROP_OP_2	Internal Pipe Diameter	1
                    Me.InternalPipeDiameter = SystemsOfUnits.Converter.ConvertToSI(su.diameter, propval)
                    Me.Beta = Me.OrificeDiameter / Me.InternalPipeDiameter
                Case 3
                    'PROP_OP_3	Correction Factor	1
                    Me.CorrectionFactor = propval
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then
                Return u0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim value As String = ""
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))
                Select Case propidx
                    Case 0
                        'PROP_OP_0	Orifice Type	1
                        value = ""
                    Case 1
                        'PROP_OP_1	Orifice Diameter	1
                        value = su.diameter
                    Case 2
                        'PROP_OP_2	Internal Pipe Diameter
                        value = su.diameter
                    Case 3
                        'PROP_OP_3	Correction Factor	1
                        value = ""
                    Case 4
                        'PROP_OP_4	Beta (d/D)	1
                        value = ""
                    Case 5
                        'PROP_OP_4	Overall Pressure Drop	0
                        value = su.deltaP
                    Case 6
                        'PROP_OP_5	Orifice Pressure Drop	0
                        value = su.deltaP
                    Case 7
                        'PROP_OP_6	Delta T	0
                        value = su.deltaT
                End Select
                Return value
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_OrificePlate With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_OrificePlate With {.SimObject = Me}
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
            Return My.Resources.uo_orifice_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("ORIF_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("ORIF_Name")
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

            str.AppendLine("Pump: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Inlet conditions")
            str.AppendLine()
            str.AppendLine("    Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, istr.Phases(0).Properties.temperature.GetValueOrDefault).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, istr.Phases(0).Properties.pressure.GetValueOrDefault).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Mass flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.massflow, istr.Phases(0).Properties.massflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.massflow)
            str.AppendLine("    Volumetric flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, istr.Phases(0).Properties.volumetric_flow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.volumetricFlow)
            str.AppendLine("    Vapor fraction: " & istr.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine("    Compounds: " & istr.PropertyPackage.RET_VNAMES.ToArrayString)
            str.AppendLine("    Molar composition: " & istr.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString(ci))
            str.AppendLine()
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Pressure Tappings: " & OrifType.ToString)
            str.AppendLine("    Orifice Diameter: " & SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.OrificeDiameter).ToString(numberformat, ci) & " " & su.diameter)
            str.AppendLine("    Internal Pipe Diameter: " & SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.InternalPipeDiameter).ToString(numberformat, ci) & " " & su.diameter)
            str.AppendLine("    Correction Factor: " & CorrectionFactor.ToString(numberformat))
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            str.AppendLine("    Orifice Pressure Drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.OrificePressureDrop).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine("    Overall Pressure Drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.OverallPressureDrop).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine("    Temperature change: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT).ToString(numberformat, ci) & " " & su.deltaT)
            str.AppendLine("    Temperature change: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT).ToString(numberformat, ci) & " " & su.deltaT)
            str.AppendLine("    Orifice Beta (d/D): " & Beta.ToString(numberformat))

            Return str.ToString

        End Function



    End Class

End Namespace


