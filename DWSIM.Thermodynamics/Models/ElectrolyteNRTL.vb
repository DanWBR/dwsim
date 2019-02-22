'    Electrolyte NRTL Model
'    Copyright 2019 Daniel Wagner O. de Medeiros
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

Imports FileHelpers
Imports System.Math
Imports DWSIM.ExtensionMethods

Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(";")> <IgnoreFirst()> <System.Serializable()>
    Public Class ElectrolyteNRTL_IPData

        Implements ICloneable

        Public ID1 As String = ""
        Public ID2 As String = ""
        Public A12 As Double = 0
        Public A21 As Double = 0
        Public alpha12 As Double = 0.2

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New NRTL_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .A12 = Me.A12
                .A21 = Me.A21
                .alpha12 = Me.alpha12
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class ElectrolyteNRTL

        Implements IActivityCoefficientBase

        Private _ip As Dictionary(Of String, Dictionary(Of String, ElectrolyteNRTL_IPData))
        Private _dc As Dictionary(Of String, DielectricConstants)

        Public Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, ElectrolyteNRTL_IPData))
            Get
                Return _ip
            End Get
            Set(value As Dictionary(Of String, Dictionary(Of String, ElectrolyteNRTL_IPData)))
                _ip = value
            End Set
        End Property

        Public ReadOnly Property DielectricConstants() As Dictionary(Of String, DielectricConstants)
            Get
                Return _dc
            End Get
        End Property

        Sub New()

            _ip = New Dictionary(Of String, Dictionary(Of String, ElectrolyteNRTL_IPData))

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Dim t1, t3, t6, t9a As String, i As Integer

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table1.txt")
                Using t As New IO.StreamReader(filestr)
                    t1 = t.ReadToEnd
                End Using
            End Using

            Dim lines As String()

            lines = t1.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            For i = 2 To lines.Count - 1
                Dim ip As New ElectrolyteNRTL_IPData
                ip.ID1 = lines(i).Split(vbTab)(0)
                ip.ID2 = "H2O"
                ip.A12 = Double.Parse(lines(i).Split(vbTab)(2), ci)
                ip.A21 = Double.Parse(lines(i).Split(vbTab)(3), ci)
                ip.alpha12 = 0.2
                If Not InteractionParameters.ContainsKey(ip.ID1) Then InteractionParameters.Add(ip.ID1, New Dictionary(Of String, ElectrolyteNRTL_IPData))
                InteractionParameters(ip.ID1).Add(ip.ID2, ip)
            Next

            'Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table2.txt")
            '    Using t As New IO.StreamReader(filestr)
            '        t2 = t.ReadToEnd
            '    End Using
            'End Using

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table3.txt")
                Using t As New IO.StreamReader(filestr)
                    t3 = t.ReadToEnd
                End Using
            End Using

            lines = t3.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            For i = 2 To lines.Count - 1
                Dim ip As New ElectrolyteNRTL_IPData
                ip.ID1 = lines(i).Split(vbTab)(0)
                ip.ID2 = lines(i).Split(vbTab)(1)
                ip.A12 = Double.Parse(lines(i).Split(vbTab)(2), ci)
                ip.A21 = Double.Parse(lines(i).Split(vbTab)(2), ci)
                ip.alpha12 = 0.2
                If Not InteractionParameters.ContainsKey(ip.ID1) Then InteractionParameters.Add(ip.ID1, New Dictionary(Of String, ElectrolyteNRTL_IPData))
                InteractionParameters(ip.ID1).Add(ip.ID2, ip)
            Next

            'Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table4.txt")
            '    Using t As New IO.StreamReader(filestr)
            '        t4 = t.ReadToEnd
            '    End Using
            'End Using

            'lines = t4.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            'For i = 2 To 2
            '    Dim ip As New ElectrolyteNRTL_IPData
            '    ip.ID1 = "NaOH"
            '    ip.ID2 = "H2O"
            '    ip.A12 = Double.Parse(lines(i).Split(vbTab)(1), ci)
            '    ip.A21 = Double.Parse(lines(i).Split(vbTab)(2), ci)
            '    ip.alpha12 = 0.2
            '    If Not InteractionParameters.ContainsKey(ip.ID1) Then InteractionParameters.Add(ip.ID1, New Dictionary(Of String, ElectrolyteNRTL_IPData))
            '    InteractionParameters(ip.ID1).Add(ip.ID2, ip)
            'Next

            'Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table5.txt")
            '    Using t As New IO.StreamReader(filestr)
            '        t5 = t.ReadToEnd
            '    End Using
            'End Using

            'lines = t5.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            'For i = 5 To 5
            '    Dim ip As New ElectrolyteNRTL_IPData
            '    ip.ID1 = "HCl"
            '    ip.ID2 = "H2O"
            '    ip.A12 = Double.Parse(lines(i).Split(vbTab)(1), ci)
            '    ip.A21 = Double.Parse(lines(i).Split(vbTab)(2), ci)
            '    ip.alpha12 = 0.2
            '    If Not InteractionParameters.ContainsKey(ip.ID1) Then InteractionParameters.Add(ip.ID1, New Dictionary(Of String, ElectrolyteNRTL_IPData))
            '    InteractionParameters(ip.ID1).Add(ip.ID2, ip)
            'Next

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table6.txt")
                Using t As New IO.StreamReader(filestr)
                    t6 = t.ReadToEnd
                End Using
            End Using

            lines = t6.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            For i = 2 To lines.Count - 1
                Dim ip As New ElectrolyteNRTL_IPData
                ip.ID1 = "CO2"
                ip.ID2 = lines(i).Split(vbTab)(0)
                ip.A12 = Double.Parse(lines(i).Split(vbTab)(2), ci)
                ip.A21 = Double.Parse(lines(i).Split(vbTab)(3), ci)
                ip.alpha12 = 0.2
                If Not InteractionParameters.ContainsKey(ip.ID1) Then InteractionParameters.Add(ip.ID1, New Dictionary(Of String, ElectrolyteNRTL_IPData))
                InteractionParameters(ip.ID1).Add(ip.ID2, ip)
            Next

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table9a.txt")
                Using t As New IO.StreamReader(filestr)
                    t9a = t.ReadToEnd
                End Using
            End Using

            lines = t6.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            For i = 2 To lines.Count - 1
                Dim ip As New ElectrolyteNRTL_IPData
                ip.ID1 = lines(i).Split(vbTab)(0)
                ip.ID2 = lines(i).Split(vbTab)(1)
                ip.A12 = Double.Parse(lines(i).Split(vbTab)(2), ci)
                ip.A21 = Double.Parse(lines(i).Split(vbTab)(2), ci)
                ip.alpha12 = 0.2
                If Not InteractionParameters.ContainsKey(ip.ID1) Then InteractionParameters.Add(ip.ID1, New Dictionary(Of String, ElectrolyteNRTL_IPData))
                InteractionParameters(ip.ID1).Add(ip.ID2, ip)
            Next

            'Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table9b.txt")
            '    Using t As New IO.StreamReader(filestr)
            '        t9b = t.ReadToEnd
            '    End Using
            'End Using

            _dc = New Dictionary(Of String, DielectricConstants)

            Dim exuniquacdc As DielectricConstants
            Dim exuniquacdcc() As DielectricConstants
            Dim fh4 As New FileHelperEngine(Of DielectricConstants)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.dielectricconstants.txt")
                Using t As New IO.StreamReader(filestr)
                    exuniquacdcc = fh4.ReadStream(t)
                End Using
            End Using

            For Each exuniquacdc In exuniquacdcc
                Me.DielectricConstants.Add(exuniquacdc.Name, exuniquacdc.Clone)
            Next

            exuniquacdc = Nothing
            exuniquacdcc = Nothing
            fh4 = Nothing

        End Sub

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double()

            Dim n As Integer = Vx.Length - 1

            Dim X(n) As Double

            Dim G As New Dictionary(Of String, Dictionary(Of String, Double))
            Dim tau As New Dictionary(Of String, Dictionary(Of String, Double))
            Dim alpha12 As New Dictionary(Of String, Dictionary(Of String, Double))

            Dim i, j, k, m1, a, a1, c, c1, wi As Integer

            Dim Vids(n) As String, vsolv(n), charge(n), molality(n), solvdensity(n), solvvfrac(n), solvmfrac(n) As Double
            Dim Msolv, DCsolv, dsolv, Xsolv, Im, A0 As Double
            Dim lnglr(n), lngb(n), lngsr(n), lng(n) As Double

            i = 0
            Do
                Vids(i) = cprops(i).Formula
                If Vids(i) = "HOH" Then Vids(i) = "H2O"
                If Vids(i) = "OCO" Then Vids(i) = "CO2"
                If Vids(i) = "HSH" Then Vids(i) = "H2S"
                If cprops(i).IsIon Then
                    charge(i) = cprops(i).Charge
                    If Vx(i) = 0.0# Then Vx(i) = 1.0E-20
                Else
                    charge(i) = 0.0#
                    solvdensity(i) = cprops(i).Molar_Weight / cprops(i).Chao_Seader_Liquid_Molar_Volume * 1000
                End If
                i = i + 1
            Loop Until i = n + 1

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    wi = i
                    wtotal += Vx(i) * cprops(i).Molar_Weight / 1000
                End If
                i += 1
            Loop Until i = n + 1

            Xsolv = 1

            i = 0
            Do
                molality(i) = Vx(i) / wtotal
                'salt-free mole fractions
                If cprops(i).Name <> "Water" And cprops(i).Name <> "Methanol" Then
                    Xsolv -= Vx(i)
                End If
                i += 1
            Loop Until i = n + 1

            Dim sumvfrac As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    sumvfrac = Vx(i) / Xsolv * cprops(i).Chao_Seader_Liquid_Molar_Volume / 1000000.0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    solvmfrac(i) = Vx(i) / Xsolv
                    solvvfrac(i) = Vx(i) / Xsolv * cprops(i).Chao_Seader_Liquid_Molar_Volume / 1000000.0 / sumvfrac
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Msolv = 0.0#
            DCsolv = 0.0#
            dsolv = 0.0#
            Do
                If cprops(i).Name = "Water" Then
                    Msolv += Vx(i) / Xsolv * cprops(i).Molar_Weight / 1000
                    With Me.DielectricConstants(cprops(i).Name)
                        DCsolv += solvvfrac(i) * (.a + .b * T + .c * T ^ 2 + .d * T ^ 3 + .e * T ^ 4)
                    End With
                    dsolv += solvvfrac(i) * solvdensity(i)
                End If
                i += 1
            Loop Until i = n + 1

            Im = 0.0#
            For i = 0 To n
                Im += 0.5 * cprops(i).Charge ^ 2 * Vx(i)
            Next

            A0 = -61.44534 * Exp((T - 273.15) / 273.15) + 2.864468 * (Exp((T - 273.15) / 273.15)) ^ 2 + 183.5379 * Log(T / 273.15) - 0.6820223 * (T - 273.15) + 0.0007875695 * (T ^ 2 - 273.15 ^ 2) + 58.95788 * 273.15 / T

            'long range term (Debye-Huckel)

            For i = 0 To n
                With cprops(i)
                    lnglr(i) = -A0 / (.Molar_Weight / 1000) ^ 0.5 * ((2 * .Charge ^ 2 / 14.9) * Log(1 + 14.9 * Im ^ 0.5) + (.Charge ^ 2 * Im ^ 0.5 - 2 * Im ^ 1.5) / (1 + 14.9 * Im ^ 0.5))
                End With
            Next

            'Born term

            For i = 0 To n
                With cprops(i)
                    lngb(i) = 0.0
                End With
            Next

            'short range term (eNRTL)

            'initialize tau and alpha12

            For i = 0 To n
                tau.Add(Vids(i), New Dictionary(Of String, Double))
                alpha12.Add(Vids(i), New Dictionary(Of String, Double))
                For j = 0 To n
                    tau(Vids(i)).Add(Vids(j), 0.0)
                    alpha12(Vids(i)).Add(Vids(j), 0.2)
                Next
            Next

            i = 0
            Do
                j = 0
                Do
                    If Me.InteractionParameters.ContainsKey(Vids(i)) Then
                        If Me.InteractionParameters(Vids(i)).ContainsKey(Vids(j)) Then
                            tau(Vids(i))(Vids(j)) = Me.InteractionParameters(Vids(i))(Vids(j)).A12
                            tau(Vids(j))(Vids(i)) = Me.InteractionParameters(Vids(i))(Vids(j)).A21
                            alpha12(Vids(i))(Vids(j)) = Me.InteractionParameters(Vids(i))(Vids(j)).alpha12
                        Else
                            If Me.InteractionParameters.ContainsKey(Vids(j)) Then
                                If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                                    tau(Vids(i))(Vids(j)) = Me.InteractionParameters(Vids(j))(Vids(i)).A21
                                    tau(Vids(j))(Vids(i)) = Me.InteractionParameters(Vids(j))(Vids(i)).A12
                                    alpha12(Vids(i))(Vids(j)) = Me.InteractionParameters(Vids(j))(Vids(i)).alpha12
                                End If
                            End If
                        End If
                    ElseIf Me.InteractionParameters.ContainsKey(Vids(j)) Then
                        If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                            tau(Vids(i))(Vids(j)) = Me.InteractionParameters(Vids(j))(Vids(i)).A21
                            tau(Vids(j))(Vids(i)) = Me.InteractionParameters(Vids(j))(Vids(i)).A12
                            alpha12(Vids(i))(Vids(j)) = Me.InteractionParameters(Vids(j))(Vids(i)).alpha12
                        End If
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            For i = 0 To n
                If cprops(i).IsIon Then
                    For m1 = 0 To n
                        If Not cprops(m1).IsSalt And cprops(m1).Charge = 0.0 Then
                            'i = ion
                            'm1 = molecule
                            For j = 0 To n
                                If cprops(j).PositiveIon <> "" Then
                                    If cprops(j).PositiveIon = cprops(i).Formula Or cprops(j).NegativeIon = cprops(i).Formula Then
                                        tau(Vids(i))(Vids(m1)) = tau(Vids(j))(Vids(m1))
                                        tau(Vids(m1))(Vids(i)) = tau(Vids(m1))(Vids(j))
                                    End If
                                End If
                            Next
                        End If
                    Next
                End If
            Next

            'local concentrations

            For i = 0 To n
                If cprops(i).Charge <> 0 Then
                    X(i) = Vx(i) * Abs(cprops(i).Charge)
                Else
                    X(i) = Vx(i)
                End If
            Next

            For i = 0 To n
                G.Add(Vids(i), New Dictionary(Of String, Double))
                For j = 0 To n
                    G(Vids(i)).Add(Vids(j), Exp(-alpha12(Vids(i))(Vids(j)) * tau(Vids(i))(Vids(j))))
                Next
            Next

            Dim sa, sc As Double

            sa = 0.0
            sc = 0.0
            For i = 0 To n
                If cprops(i).Charge > 0 Then
                    'cation
                    sc += X(i)
                ElseIf cprops(i).Charge < 0 Then
                    'anion
                    sa += X(i)
                End If
            Next

            Dim x0(n) As Double

            For i = 0 To n
                If cprops(i).Charge < 0 Then
                    'anion
                    x0(i) = X(i) / sa
                ElseIf cprops(i).Charge > 0 Then
                    'cation
                    x0(i) = X(i) / sc
                End If
            Next

            'summation terms

            Dim s0(n), s0t(n) As Double

            For m1 = 0 To n
                s0(m1) = 0
                s0t(m1) = 0
                For k = 0 To n
                    s0(m1) += X(k) * G(Vids(k))(Vids(m1))
                    s0t(m1) += X(k) * G(Vids(k))(Vids(m1)) * tau(Vids(k))(Vids(m1))
                Next
            Next

            Dim s1(n)(), s1t(n)() As Double

            For i = 0 To n
                Array.Resize(s1(i), n + 1)
                Array.Resize(s1t(i), n + 1)
            Next

            For c = 0 To n
                For a1 = 0 To n
                    s1(c)(a1) = 0
                    s1t(c)(a1) = 0
                    For k = 0 To n
                        If k <> c Then
                            s1(c)(a1) += X(k) * G(Vids(k))(Vids(c))
                            s1t(c)(a1) += X(k) * G(Vids(k))(Vids(c)) * tau(Vids(k))(Vids(c))
                        End If
                    Next
                Next
            Next

            Dim s2(n)(), s2t(n)() As Double

            For i = 0 To n
                Array.Resize(s2(i), n + 1)
                Array.Resize(s2t(i), n + 1)
            Next

            For a = 0 To n
                For c1 = 0 To n
                    s2(a)(c1) = 0
                    s2t(a)(c1) = 0
                    For k = 0 To n
                        If k <> a Then
                            s2(a)(c1) += X(k) * G(Vids(k))(Vids(a))
                            s2t(a)(c1) += X(k) * G(Vids(k))(Vids(a)) * tau(Vids(k))(Vids(a))
                        End If
                    Next
                Next
            Next

            Dim sm1(n), sm2(n), sm3(n), sma1(n), sma2(n), sma3(n), smc1(n), smc2(n), smc3(n) As Double

            For m = 0 To n
                sm1(m) = 0.0
                For m1 = 0 To n
                    If cprops(m1).Charge = 0 Then
                        sm1(m) += X(m1) * G(Vids(m))(Vids(m1)) / s0(m1) * (tau(Vids(m))(Vids(m1)) - s0t(m1) / s0(m1))
                    End If
                Next
                sma1(m) = 0.0
                For c1 = 0 To n
                    For k = 0 To n
                        If k <> m And cprops(m).Charge < 0 And cprops(c1).Charge > 0 Then
                            sma1(m) += x0(c1) * X(k) * G(Vids(k))(Vids(c1)) * tau(Vids(k))(Vids(c1)) / s2(m)(c1)
                        End If
                    Next
                Next
                smc1(m) = 0.0
                For a1 = 0 To n
                    For k = 0 To n
                        If k <> m And cprops(m).Charge > 0 And cprops(a1).Charge < 0 Then
                            smc1(m) += x0(a1) * X(k) * G(Vids(k))(Vids(a1)) * tau(Vids(k))(Vids(a1)) / s2(m)(a1)
                        End If
                    Next
                Next
                sm2(m) = 0.0
                For c = 0 To n
                    If cprops(c).Charge > 0 Then
                        For a1 = 0 To n
                            If cprops(a1).Charge < 0 Then
                                sm2(m) += x0(a1) * X(c) * G(Vids(m))(Vids(a1)) / s1(c)(a1) * (tau(Vids(m))(Vids(a1)) - s1t(c)(a1) / s1(c)(a1))
                            End If
                        Next
                    End If
                Next
                sma2(m) = 0.0
                For m1 = 0 To n
                    If cprops(m).Charge < 0 And cprops(m1).Charge = 0 Then
                        sma2(m) += X(m1) * G(Vids(m))(Vids(m1)) / s0(m1) * (tau(Vids(m))(Vids(m1)) - s0t(m1) / s0(m1))
                    End If
                Next
                smc2(m) = 0.0
                For m1 = 0 To n
                    If cprops(m).Charge > 0 And cprops(m1).Charge = 0 Then
                        smc2(m) += X(m1) * G(Vids(m))(Vids(m1)) / s0(m1) * (tau(Vids(m))(Vids(m1)) - s0t(m1) / s0(m1))
                    End If
                Next
                sm3(m) = 0.0
                For a = 0 To n
                    If cprops(a).Charge < 0 Then
                        For c1 = 0 To n
                            If cprops(c1).Charge > 0 Then
                                sm3(m) += x0(c1) * X(a) * G(Vids(m))(Vids(c1)) / s2(a)(c1) * (tau(Vids(m))(Vids(c1)) - s2t(a)(c1) / s2(a)(c1))
                            End If
                        Next
                    End If
                Next
                sma3(m) = 0.0
                For c = 0 To n
                    If cprops(c).Charge > 0 Then
                        For a1 = 0 To n
                            If cprops(a1).Charge < 0 Then
                                sma3(m) += x0(a1) * X(c) * G(Vids(c))(Vids(a1)) / s2(c)(a1) * (tau(Vids(c))(Vids(a1)) - s2t(c)(a1) / s2(c)(a1))
                            End If
                        Next
                    End If
                Next
                smc3(m) = 0.0
                For a = 0 To n
                    If cprops(a).Charge < 0 Then
                        For c1 = 0 To n
                            If cprops(c1).Charge > 0 Then
                                smc3(m) += x0(c1) * X(a) * G(Vids(a))(Vids(c1)) / s2(a)(c1) * (tau(Vids(a))(Vids(c1)) - s2t(a)(c1) / s2(a)(c1))
                            End If
                        Next
                    End If
                Next
            Next

            'short range contribution

            For i = 0 To n
                If cprops(i).IsIon Then
                    If cprops(i).Charge > 0 Then
                        'cation
                        lngsr(i) = smc1(i) + smc2(i) + smc3(i)
                        lngsr(i) *= Abs(cprops(i).Charge)
                    ElseIf cprops(i).Charge < 0 Then
                        'anion
                        lngsr(i) = sma1(i) + sma2(i) + smc3(i)
                        lngsr(i) *= Abs(cprops(i).Charge)
                    End If
                ElseIf cprops(i).IsSalt Then
                    'salt, will be calculated by mean molal average
                Else
                    'molecule
                    lngsr(i) = s0t(i) / s0(i) + sm1(i) + sm2(i) + sm3(i)
                End If
            Next

            'final activity coefficients

            For i = 0 To n
                lng(i) = lngsr(i) + lngb(i) + lnglr(i)
            Next

            'calculate mean molal activity coefficient

            Dim plng, nlng As Double

            For i = 0 To n
                With cprops(i)
                    If .IsSalt Or .IsHydratedSalt Then
                        For j = 0 To n
                            If .PositiveIon = Vids(j) Then
                                plng = lng(j)
                            End If
                            If .NegativeIon = Vids(j) Then
                                nlng = lng(j)
                            End If
                        Next
                        lng(i) = .PositiveIonStoichCoeff * plng / .StoichSum + .NegativeIonStoichCoeff * nlng / .StoichSum
                    End If
                End With
            Next

            Dim ac(n) As Double

            For i = 0 To n
                ac(i) = Math.Exp(lng(i))
            Next

            Return ac

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, cprops As List(Of Interfaces.ICompoundConstantProperties)) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, cprops)
            gamma2 = GAMMA_MR(T + epsilon, Vx, cprops)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, cprops)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, cprops)
            hex2 = HEX_MIX(T + epsilon, Vx, cprops)

            cpex = (hex2 - hex1) / epsilon

            Return cpex 'kJ/kmol.K

        End Function

        Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

            Return GAMMA_MR(T, Vx, otherargs)

        End Function

        Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

            Return HEX_MIX(T, Vx, otherargs)

        End Function

        Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

            Return CPEX_MIX(T, Vx, otherargs)

        End Function

    End Class

End Namespace
