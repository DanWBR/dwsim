'    Shortcut Column Calculation Routines 
'    Copyright 2008-2013 Daniel Wagner O. de Medeiros
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
'

Imports System.Collections.Generic

Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums
Imports System.Math
Imports DWSIM.MathOps.MathEx
Imports IronPython.Zlib

Namespace UnitOperations

    <System.Serializable()> Public Class ShortcutColumn

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Columns

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_ShortcutColumn

        Public Enum CondenserType
            TotalCond = 0
            PartialCond = 1
        End Enum

        Public m_lightkey As String = ""
        Public m_heavykey As String = ""
        Public m_lightkeymolarfrac As Double = 0.01
        Public m_heavykeymolarfrac As Double = 0.01
        Public m_refluxratio As Double = 1.5
        Public m_boilerpressure As Double = 101325.0#
        Public m_condenserpressure As Double = 101325.0#

        Public m_N, m_Nmin, m_Rmin, m_Tc, m_Tb, m_Qc, m_Qb, L, V, L_, V_, ofs As Double

        Public condtype As CondenserType = CondenserType.TotalCond

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            ComponentName = name
            ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New ShortcutColumn()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of ShortcutColumn)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean
            MyBase.LoadData(data)
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
            Return True
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)
            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            elements.AddRange(XMLSerializer.XMLSerializer.Serialize(Me, True))
            Return elements
        End Function

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The shortcut column is used to calculate the minimum reflux and 
                                distribution of products in a distillation column by the method 
                                of Fenske-Underwood-Gilliland. The column should have a 
                                single feed stage, two products (top and bottom), condenser 
                                (partial or total) and reboiler. The results are the minimum 
                                reflux, thermal loads and temperature of the condenser and 
                                reboiler for a fixed reflux ratio, in addition to determining the 
                                optimum feed stage and the minimum number of stages.")

            'Validate unitop status.
            Me.Validate()

            'streams

            Dim feed, distillate, bottoms As MaterialStream
            Dim cduty, rduty As Streams.EnergyStream

            feed = GetInletMaterialStream(0)
            distillate = GetOutletMaterialStream(0)
            bottoms = GetOutletMaterialStream(1)
            cduty = GetEnergyStream()
            rduty = GetInletEnergyStream(1)

            feed.Validate()

            'classify components by relative volatility

            Dim n As Integer = feed.Phases(0).Compounds.Count - 1
            Dim i As Integer = 0

            Dim lnk, dnk, hnk As New ArrayList
            Dim hki, lki As Integer
            Dim K(n), alpha(n), z(n), xb(n), xd(n), F, D, Dant, B, R, q, T, P As Double
            Dim id(n) As String

            F = feed.Phases(0).Properties.molarflow.GetValueOrDefault
            q = feed.Phases(3).Properties.molarfraction.GetValueOrDefault
            T = feed.Phases(0).Properties.temperature.GetValueOrDefault
            P = feed.Phases(0).Properties.pressure.GetValueOrDefault

            If q = 0.0 Or q = 1.0 Then

                Dim H = feed.Phases(0).Properties.enthalpy.GetValueOrDefault()

                feed.PropertyPackage.CurrentMaterialStream = feed
                Dim bpoint = feed.PropertyPackage.CalculateEquilibrium(FlashCalculationType.PressureVaporFraction, P, 0.0, feed.GetOverallComposition(), Nothing, T)
                Dim Tbub = bpoint.CalculatedTemperature.GetValueOrDefault()
                feed.PropertyPackage.CurrentMaterialStream = feed
                Dim dpoint = feed.PropertyPackage.CalculateEquilibrium(FlashCalculationType.PressureVaporFraction, P, 1.0, feed.GetOverallComposition(), Nothing, T)
                Dim Tdew = dpoint.CalculatedTemperature.GetValueOrDefault()
                feed.PropertyPackage.CurrentMaterialStream = feed
                Dim Hbub = feed.PropertyPackage.DW_CalcEnthalpy(feed.GetOverallComposition(), Tbub, P, PropertyPackages.State.Liquid)
                Dim Hdew = feed.PropertyPackage.DW_CalcEnthalpy(feed.GetOverallComposition(), Tdew, P, PropertyPackages.State.Vapor)

                q = 1.0 + (Hbub - H) / (Hdew - Hbub)

            End If

            i = 0
            For Each comp As BaseClasses.Compound In feed.Phases(0).Compounds.Values
                z(i) = comp.MoleFraction.GetValueOrDefault()
                id(i) = comp.Name
                If Me.m_lightkey = comp.Name Then lki = i
                If Me.m_heavykey = comp.Name Then hki = i
                i = i + 1
            Next

            feed.PropertyPackage.CurrentMaterialStream = feed

            IObj?.SetCurrent()
            K = feed.PropertyPackage.DW_CalcKvalue(z, T, P)

            For i = 0 To n
                If Double.IsInfinity(K(i)) Then K(i) = Double.MaxValue
            Next

            i = 0
            Do
                alpha(i) = K(i) / K(hki)
                If K(i) > K(lki) Then
                    lnk.Add(i)
                ElseIf K(i) < K(lki) And K(i) > K(hki) Then
                    dnk.Add(i)
                ElseIf K(i) < K(hki) Then
                    hnk.Add(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            'first D estimate
            i = 0
            D = F * z(lki)
            If lnk.Count > 0 Then
                Do
                    D += F * (z(lnk(i)))
                    i = i + 1
                Loop Until i >= lnk.Count
            End If

restart:    B = F - D

            xd(hki) = Me.m_heavykeymolarfrac
            xb(lki) = Me.m_lightkeymolarfrac

            xb(hki) = (F * z(hki) - D * xd(hki)) / (F - D)
            xd(lki) = (F * z(lki) - (F - D) * xb(lki)) / D

            'Nmin calculation (Fenske)

            Dim S As Double

            S = (xd(lki) / xd(hki)) * (xb(hki) / xb(lki))
            m_Nmin = Math.Log(S) / Math.Log(alpha(lki) / alpha(hki))

            'calculate nonkeys distribution

            Dim C, cte(n) As Double

            C = (Log10(alpha(lki)) * Log10(xd(hki) / xb(hki)) - Log10(alpha(hki)) * Log10(xd(lki) / xb(lki))) / (Log10(alpha(lki)) - Log10(alpha(hki)))

            i = 0
            Do
                If i <> lki And i <> hki Then
                    cte(i) = 10 ^ (m_Nmin * Math.Log10(alpha(i)) + C)
                    xb(i) = F * z(i) / (B + D * cte(i))
                    xd(i) = xb(i) * cte(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            Dant = D

            i = 0
            D = 0
            Do
                If z(i) <> 0 Then D += Dant * xd(i)
                i = i + 1
            Loop Until i = n + 1

            If Double.IsNaN(D) Or D = 0.0# Then Throw New ArgumentOutOfRangeException("D", "Invalid value for Distillate Rate: " & D)

            If Not Math.Abs((D - Dant) / D) < 0.0001 Then GoTo restart

            R = m_refluxratio
            L = R * D
            L_ = L + q * F
            V_ = L_ - B
            V = D + L

            If L_ < 0.0 Or V_ < 0 Then
                Throw New Exception("Invalid Reflux Ratio")
            End If

            'calculate minimum reflux by Underwood's method

            Dim brentsolver As New BrentOpt.BrentMinimize

            Dim mode2 As Boolean = False
            Dim count As Integer = 0
            Dim indexes As New ArrayList
            Dim Dr(n) As Double
            i = 0
            Do
                Dr(i) = (alpha(i) - 1) / (alpha(lki) - 1) * D * xd(lki) / (F * z(lki)) + (alpha(lki) - alpha(i)) / (alpha(lki) - 1) * D * xd(hki) / (F * z(hki))
                If Dr(i) > 0 And Dr(i) < 1 And z(i) <> 0 And i <> lki And i <> hki Then
                    mode2 = True
                    count += 1
                    indexes.Add(i)
                End If
                i = i + 1
            Loop Until i = n + 1

            If mode2 = False Then

                Dim teta, L_Dmin, sum As Double

                teta = brentsolver.brentoptimize2(alpha(hki) * 1.01, alpha(lki), 0.0000001,
                                                Function(x)
                                                    Return rminfunc(x, alpha, z, q, n)
                                                End Function)

                sum = 0
                i = 0
                Do
                    If z(i) <> 0 Then sum += alpha(i) * xd(i) / (alpha(i) - teta)
                    i = i + 1
                Loop Until i = n + 1

                L_Dmin = sum - 1

                m_Rmin = L_Dmin

            Else

                Dim teta(count), xdm(count - 1) As Double

                i = 0
                Do
                    If i = 0 Then
                        If alpha(lki) < alpha(indexes(i)) Then
                            teta(i) = brentsolver.brentoptimize2(alpha(lki) * 1.01, alpha(indexes(i)) * 0.99, 0.0000001,
                                                Function(x)
                                                    Return rminfunc(x, alpha, z, q, n)
                                                End Function)
                        Else
                            teta(i) = brentsolver.brentoptimize2(alpha(indexes(i)) * 1.01, alpha(lki) * 0.99, 0.0000001,
                                                Function(x)
                                                    Return rminfunc(x, alpha, z, q, n)
                                                End Function)
                        End If
                    ElseIf i = count Then
                        If alpha(indexes(i - 1)) < alpha(hki) Then
                            teta(i) = brentsolver.brentoptimize2(alpha(indexes(i - 1)) * 1.01, alpha(hki) * 0.99, 0.0000001,
                                                Function(x)
                                                    Return rminfunc(x, alpha, z, q, n)
                                                End Function)
                        Else
                            teta(i) = brentsolver.brentoptimize2(alpha(hki) * 1.01, alpha(indexes(i - 1)) * 0.99, 0.0000001,
                                                Function(x)
                                                    Return rminfunc(x, alpha, z, q, n)
                                                End Function)
                        End If
                    Else
                        If alpha(indexes(i - 1)) < alpha(indexes(i)) Then
                            teta(i) = brentsolver.brentoptimize2(alpha(indexes(i - 1)) * 1.01, alpha(indexes(i)) * 0.99, 0.0000001,
                                                Function(x)
                                                    Return rminfunc(x, alpha, z, q, n)
                                                End Function)
                        Else
                            teta(i) = brentsolver.brentoptimize2(alpha(indexes(i - 1)) * 1.01, alpha(indexes(i)) * 0.99, 0.0000001,
                                                Function(x)
                                                    Return rminfunc(x, alpha, z, q, n)
                                                End Function)
                        End If
                    End If
                    i = i + 1
                Loop Until i > count

                Dim MA As New Mapack.Matrix(count + 1, count + 1)
                Dim MB As New Mapack.Matrix(count + 1, 1)
                Dim MX As New Mapack.Matrix(1, count + 1)

                Dim j, j2 As Integer
                i = 0
                Do
                    MB(i, 0) = -1.0
                    MA(i, 0) = 1.0 'L/D min
                    j2 = 0
                    For j = 0 To alpha.Count - 1
                        If Not indexes.Contains(j) Then
                            MB(i, 0) += alpha(j) * xd(j) / (alpha(j) - teta(i))
                        Else
                            MA(i, j2 + 1) = -alpha(j) / (alpha(j) - teta(i))
                            j2 += 1
                        End If
                    Next
                    i = i + 1
                Loop Until i > count

                MX = MA.Solve(MB)

                m_Rmin = MX(0, 0)

                'Dim sum As Double = 0.0
                'For j = 0 To teta.Count - 1
                '    For i = 0 To n
                '        If z(i) > 0.0 Then sum += alpha(i) * xd(i) / (alpha(i) - teta(j))
                '    Next
                'Next

                'm_Rmin = sum / teta.Count - 1

            End If

            If m_Rmin > m_refluxratio Then
                Throw New Exception(String.Format("Defined Reflux Ratio ({0}) lower than calculated minimum ({1})", m_refluxratio, m_Rmin))
            End If

            'actual number of stages by Gilliland's method

            Dim xx, yy As Double
            xx = (R - m_Rmin) / (R + 1)
            yy = 0.75 * (1 - xx ^ 0.5668)
            m_N = (yy + m_Nmin) / (1 - yy)

            'temperatures and heat duties - copy compositions

            Dim Dmw, Bmw As Double

            i = 0
            Dmw = 0
            For Each comp As BaseClasses.Compound In distillate.Phases(0).Compounds.Values
                If Double.IsNaN(xd(i)) = False Then comp.MoleFraction = xd(i) Else comp.MoleFraction = 0
                Dmw += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                i = i + 1
            Next
            With distillate.Phases(0)
                .Properties.pressure = Me.m_condenserpressure
                .Properties.molarflow = D
                .Properties.massflow = Dmw * D / 1000
            End With

            i = 0
            Bmw = 0
            For Each comp As BaseClasses.Compound In bottoms.Phases(0).Compounds.Values
                If Double.IsNaN(xb(i)) = False Then comp.MoleFraction = xb(i) Else comp.MoleFraction = 0
                Bmw += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                i = i + 1
            Next
            With bottoms.Phases(0)
                .Properties.pressure = Me.m_boilerpressure
                .Properties.molarflow = B
                .Properties.massflow = Bmw * B / 1000
            End With

            Dim result As IFlashCalculationResult
            Dim TD, TB, TF, HF, HD, HD0, HB, HL As Double
            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage

            TF = feed.Phases(0).Properties.temperature
            HF = feed.Phases(0).Properties.enthalpy.GetValueOrDefault * feed.Phases(0).Properties.molecularWeight.GetValueOrDefault

            pp.CurrentMaterialStream = distillate

            For Each comp As BaseClasses.Compound In distillate.Phases(0).Compounds.Values
                comp.MassFraction = pp.AUX_CONVERT_MOL_TO_MASS(comp.Name, 0)
            Next

            IObj?.SetCurrent()
            If Me.condtype = CondenserType.PartialCond Then
                result = pp.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, m_condenserpressure, 1, 0)
                TD = result.CalculatedTemperature
            ElseIf Me.condtype = CondenserType.TotalCond Then
                result = pp.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, m_condenserpressure, 0, 0)
                TD = result.CalculatedTemperature
            End If
            With distillate.Phases(0)
                .Properties.temperature = TD
            End With
            IObj?.SetCurrent()
            distillate.Calculate(True, True)
            distillate.AtEquilibrium = True

            HD = distillate.Phases(0).Properties.enthalpy.GetValueOrDefault * distillate.Phases(0).Properties.molecularWeight.GetValueOrDefault

            pp.CurrentMaterialStream = bottoms

            For Each comp As BaseClasses.Compound In bottoms.Phases(0).Compounds.Values
                comp.MassFraction = pp.AUX_CONVERT_MOL_TO_MASS(comp.Name, 0)
            Next

            IObj?.SetCurrent()
            result = pp.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, m_boilerpressure, 0.0, 0)
            TB = result.CalculatedTemperature
            With bottoms.Phases(0)
                .Properties.temperature = TB
            End With
            IObj?.SetCurrent()
            bottoms.Calculate(True, True)
            bottoms.AtEquilibrium = True

            HB = bottoms.Phases(0).Properties.enthalpy.GetValueOrDefault * bottoms.Phases(0).Properties.molecularWeight.GetValueOrDefault

            pp.CurrentMaterialStream = distillate
            If Me.condtype = CondenserType.PartialCond Then
                IObj?.SetCurrent()
                result = pp.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, m_condenserpressure, 0, 0)
                HL = result.CalculatedEnthalpy * distillate.Phases(0).Properties.molecularWeight.GetValueOrDefault
                m_Qc = -(HL - HD) * L / 1000
            ElseIf Me.condtype = CondenserType.TotalCond Then
                IObj?.SetCurrent()
                result = pp.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, m_condenserpressure, 1, 0)
                HD0 = result.CalculatedEnthalpy * distillate.Phases(0).Properties.molecularWeight.GetValueOrDefault
                IObj?.SetCurrent()
                result = pp.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, m_condenserpressure, 0, 0)
                HL = result.CalculatedEnthalpy * distillate.Phases(0).Properties.molecularWeight.GetValueOrDefault
                m_Qc = -(HL - HD0) * (L + D) / 1000
            End If

            m_Qb = D / 1000 * HD + B / 1000 * HB + m_Qc - F / 1000 * HF

            'optimum feed stage by Fenske's method

            Dim NminS, Ss, Ns As Double

            Ss = z(lki) / z(hki) * xb(hki) / xb(lki)
            NminS = Log(Ss) / Log(alpha(lki))
            Ns = NminS * m_N / m_Nmin
            ofs = Ns

            'update exchanger duties

            If cduty IsNot Nothing Then
                With cduty
                    .EnergyFlow = m_Qc
                    .GraphicObject.Calculated = True
                End With
            End If

            If rduty IsNot Nothing Then
                With rduty
                    .EnergyFlow = m_Qb
                    .GraphicObject.Calculated = True
                End With
            End If

            IObj?.Close()

        End Sub

        Function rminfunc(ByVal x As Double, alpha As Object, z As Object, q As Double, n As Integer) As Double

            Dim value As Double = 0.0
            Dim j As Integer = 0
            Do
                If z(j) <> 0.0 Then
                    value += alpha(j) * z(j) / (alpha(j) - x)
                End If
                j = j + 1
            Loop Until j = n + 1

            Return (value - 1 + q) ^ 2

        End Function

        Public Overrides Sub DeCalculate()

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de materia conectada a jusante
                GetOutletMaterialStream(0).Clear()

            End If

            If Me.GraphicObject.OutputConnectors(1).IsAttached Then

                'Zerar valores da corrente de materia conectada a jusante
                GetOutletMaterialStream(1).Clear()

            End If

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
                        'PROP_SC_0	Reflux Ratio
                        value = Me.m_refluxratio
                    Case 1
                        'PROP_SC_1	Heavy Key Molar Fraction
                        value = Me.m_heavykeymolarfrac
                    Case 2
                        'PROP_SC_2	Light Key Molar Fraction
                        value = Me.m_lightkeymolarfrac
                    Case 3
                        'PROP_SC_3	Condenser Pressure
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.m_condenserpressure)
                    Case 4
                        'PROP_SC_4	Reboiler Pressure
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.m_boilerpressure)
                    Case 5
                        'PROP_SC_5	Minimun Reflux Ratio
                        value = Me.m_Rmin
                    Case 6
                        'PROP_SC_6	Minimum Stages
                        value = Me.m_Nmin
                    Case 7
                        'PROP_SC_7	Optimal Feed Stage
                        value = Me.ofs
                    Case 8
                        'PROP_SC_8	Stripping Liquid Molar Flow
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.L_)
                    Case 9
                        'PROP_SC_9	Rectify Liquid Molar Flow
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.L)
                    Case 10
                        'PROP_SC_10	Stripping Vapor Molar Flow
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.V_)
                    Case 11
                        'PROP_SC_11	Rectify Vapor Molar Flow
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.V)
                    Case 12
                        'PROP_SC_12	Condenser Duty
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.m_Qc)
                    Case 13
                        'PROP_SC_13	Reboiler Duty
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.m_Qb)
                    Case 14
                        value = m_N
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
                    For i = 5 To 14
                        proplist.Add("PROP_SC_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 14
                        proplist.Add("PROP_SC_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 4
                        proplist.Add("PROP_SC_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 14
                        proplist.Add("PROP_SC_" + CStr(i))
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
                    'PROP_SC_0	Reflux Ratio
                    Me.m_refluxratio = propval
                Case 1
                    'PROP_SC_1	Heavy Key Molar Fraction
                    Me.m_heavykeymolarfrac = propval
                Case 2
                    'PROP_SC_2	Light Key Molar Fraction
                    Me.m_lightkeymolarfrac = propval
                Case 3
                    'PROP_SC_3	Condenser Pressure
                    Me.m_condenserpressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                Case 4
                    'PROP_SC_4	Reboiler Pressure
                    Me.m_boilerpressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)

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
                        'PROP_SC_0	Reflux Ratio
                        value = ""
                    Case 1
                        'PROP_SC_1	Heavy Key Molar Fraction
                        value = ""
                    Case 2
                        'PROP_SC_2	Light Key Molar Fraction
                        value = ""
                    Case 3
                        'PROP_SC_3	Condenser Pressure
                        value = su.pressure
                    Case 4
                        'PROP_SC_4	Reboiler Pressure
                        value = su.pressure
                    Case 5
                        'PROP_SC_5	Minimun Reflux Ratio
                        value = ""
                    Case 6
                        'PROP_SC_6	Minimum Stages
                        value = ""
                    Case 7
                        'PROP_SC_7	Optimal Feed Stage
                        value = ""
                    Case 8
                        'PROP_SC_8	Stripping Liquid Molar Flow
                        value = su.molarflow
                    Case 9
                        'PROP_SC_9	Rectify Liquid Molar Flow
                        value = su.molarflow
                    Case 10
                        'PROP_SC_10	Stripping Vapor Molar Flow
                        value = su.molarflow
                    Case 11
                        'PROP_SC_11	Rectify Vapor Molar Flow
                        value = su.molarflow
                    Case 12
                        'PROP_SC_12	Condenser Duty
                        value = su.heatflow
                    Case 13
                        'PROP_SC_13	Reboiler Duty
                        value = su.heatflow
                    Case 14
                        value = ""
                End Select

                Return value
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_ShortcutColumn With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ShortcutColumn With {.SimObject = Me}
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
            Return My.Resources.col_sc_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CSC_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CSC_Name")
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

            str.AppendLine("Shortcut Column: " & Me.GraphicObject.Tag)
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
            str.AppendLine("    Condenser type: " & Me.condtype.ToString)
            str.AppendLine("    Reflux ratio: " & Me.m_refluxratio.ToString(numberformat, ci))
            str.AppendLine("    Light key: " & Me.m_lightkey.ToString)
            str.AppendLine("    Light key mole fraction: " & Me.m_lightkeymolarfrac.ToString(numberformat, ci))
            str.AppendLine("    Heavy key: " & Me.m_heavykey.ToString)
            str.AppendLine("    Heavy key mole fraction: " & Me.m_heavykeymolarfrac.ToString(numberformat, ci))
            str.AppendLine("    Condenser pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.m_condenserpressure).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Reboiler pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.m_condenserpressure).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            str.AppendLine("    Minimum reflux ratio: " & Me.m_Rmin.ToString(numberformat, ci))
            str.AppendLine("    Minimum number of stages: " & Me.m_Nmin.ToString(numberformat, ci))
            str.AppendLine("    Actual number of stages: " & Me.m_N.ToString(numberformat, ci))
            str.AppendLine("    Optimum feed stage: " & Me.ofs.ToString(numberformat, ci))
            str.AppendLine("    Condenser heat duty: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.m_Qc).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine("    Reboiler heat duty: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.m_Qb).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine("    Stripping liquid mole flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.L_).ToString(numberformat, ci) & " " & su.molarflow)
            str.AppendLine("    Rectifying liquid mole flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.L).ToString(numberformat, ci) & " " & su.molarflow)
            str.AppendLine("    Stripping vapor mole flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.V_).ToString(numberformat, ci) & " " & su.molarflow)
            str.AppendLine("    Rectifying liquid mole flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.V).ToString(numberformat, ci) & " " & su.molarflow)

            Return str.ToString

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Light Key Compound") Then
                Return "Select a compound that will be considered as the lightest one for specification purposes."
            ElseIf p.Equals("Heavy Key Compound") Then
                Return "Select a compound that will be considered as the heaviest one for specification purposes."
            ElseIf p.Equals("LK Mole Fraction in Bottoms") Then
                Return "Enter the mole fraction of the Light Key compound in the bottoms stream."
            ElseIf p.Equals("HK Mole Fraction in Distillate") Then
                Return "Enter the mole fraction of the Heavy Key compound in the distillate stream."
            ElseIf p.Equals("Reflux Ratio") Then
                Return "Specify the reflux ratio of this column."
            ElseIf p.Equals("Condenser Pressure") Then
                Return "Enter the pressure of the condenser on this column."
            ElseIf p.Equals("Reboiler Pressure") Then
                Return "Enter the pressure of the reboiler on this column."
            ElseIf p.Equals("Condenser Type") Then
                Return "Select the type of the condenser on this column."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace



