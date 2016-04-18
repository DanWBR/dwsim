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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums

Namespace UnitOperations

    <System.Serializable()> Public Class OrificePlate

        Inherits SharedClasses.UnitOperations.UnitOpBaseClass

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
        Protected _orificeDP As Double = 0
        Protected _fluidDP As Double = 0
        Protected _beta As Double = 0
        Protected _orificediameter As Double = 0
        Protected _orificetype As OrificeType = OrificeType.FlangeTaps
        Protected _calcmethod As CalcMethod
        Protected _corrfactor As Double = 1

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

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

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

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
            A1 = 3.1416 * (_orificediameter / _beta) ^ 2 / 4
            A2 = 3.1416 * (_orificediameter) ^ 2 / 4

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

            Dim ReD, Cd, DP As Double

            ReD = Wi * _orificediameter / (A1 * mum)
            If L1 < 0.4333 Then
                Cd = 0.5959 + 0.312 * beta ^ 2.1 - 0.184 * beta ^ 8 + 0.0029 * beta ^ 2.5 * (10 ^ 6 / ReD) ^ 0.75 + 0.09 * L1 * (beta ^ 4 / (1 - beta ^ 4)) - 0.0337 * L2 * beta ^ 3
            Else
                Cd = 0.5959 + 0.312 * beta ^ 2.1 - 0.184 * beta ^ 8 + 0.0029 * beta ^ 2.5 * (10 ^ 6 / ReD) ^ 0.75 + 0.039 * L1 * (beta ^ 4 / (1 - beta ^ 4)) - 0.0337 * L2 * beta ^ 3
            End If

            DP = (Wi / (_corrfactor * Cd * A2)) ^ 2 * (1 - beta ^ 4) / (2 * rhom)
            DP = DP + (rhom * 9.8 * (s2_s1))

            _orificeDP = DP
            _fluidDP = DP * ((1 - beta ^ 4 * (1 - Cd ^ 2)) ^ 0.5 - Cd * beta ^ 2) / ((1 - beta ^ 4 * (1 - Cd ^ 2)) ^ 0.5 + Cd * beta ^ 2)

            P2 = Pi - _fluidDP
            H2 = H1

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
            End With

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
                    'PROP_OP_2	Beta (d/D)	1
                    value = Me.Beta
                Case 3
                    'PROP_OP_3	Correction Factor	1
                    value = Me.CorrectionFactor
                Case 4
                    'PROP_OP_4	Overall Pressure Drop	0
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.OverallPressureDrop)
                Case 5
                    'PROP_OP_5	Orifice Pressure Drop	0
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.OrificePressureDrop)
                Case 6
                    'PROP_OP_6	Delta T	0
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 4 To 6
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 6
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 3
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 6
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
            'PROP_OP_0	Orifice Type	1
            'PROP_OP_1	Orifice Diameter	1
            'PROP_OP_2	Beta (d/D)	1
            'PROP_OP_3	Correction Factor	1
            'PROP_OP_4	Overall Pressure Drop	0
            'PROP_OP_5	Orifice Pressure Drop	0
            'PROP_OP_6	Delta T	0

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
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
                Case 2
                    'PROP_OP_2	Beta (d/D)	1
                    Me.Beta = propval
                Case 3
                    'PROP_OP_3	Correction Factor	1
                    Me.CorrectionFactor = propval
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
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
                    'PROP_OP_2	Beta (d/D)	1
                    value = ""
                Case 3
                    'PROP_OP_3	Correction Factor	1
                    value = ""
                Case 4
                    'PROP_OP_4	Overall Pressure Drop	0
                    value = su.deltaP
                Case 5
                    'PROP_OP_5	Orifice Pressure Drop	0
                    value = su.deltaP
                Case 6
                    'PROP_OP_6	Delta T	0
                    value = su.deltaT
            End Select

            Return value
        End Function

        Public Overrides Sub DisplayEditForm()

        End Sub
    End Class

End Namespace


