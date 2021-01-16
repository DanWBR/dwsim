﻿'    Electrolyte NRTL Model
'    Copyright 2019 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
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
        Public B12 As Double = 0
        Public B21 As Double = 0
        Public alpha12 As Double = 0.2

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New NRTL_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .A12 = Me.A12
                .A21 = Me.A21
                .B12 = Me.B12
                .B21 = Me.B21
                .alpha12 = Me.alpha12
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class ElectrolyteNRTL

        Implements IActivityCoefficientBase

        Private _ip As Dictionary(Of String, Dictionary(Of String, ElectrolyteNRTL_IPData))
        Private _dc As Dictionary(Of String, DielectricConstants)

        Private T_NaOH As New List(Of Double)
        Private tau_NaOH_A12 As New List(Of Double)
        Private tau_NaOH_A21 As New List(Of Double)

        Private T_HCl As New List(Of Double)
        Private tau_HCl_A12 As New List(Of Double)
        Private tau_HCl_A21 As New List(Of Double)

        Public Class ENRTLData

            Property ConstantProperties As List(Of Interfaces.ICompoundConstantProperties)

            Property Vz As Double()

            Property Vids As String()

            Property T As Double

            Property X As Double()

        End Class

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

            Dim t1, t3, t4, t4a, t5, t6, t9a As String, i As Integer

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

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table4.txt")
                Using t As New IO.StreamReader(filestr)
                    t4 = t.ReadToEnd
                End Using
            End Using

            lines = t4.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            For i = 2 To 7
                T_NaOH.Add(Double.Parse(lines(i).Split(vbTab)(0), ci))
                tau_NaOH_A12.Add(Double.Parse(lines(i).Split(vbTab)(1), ci))
                tau_NaOH_A21.Add(Double.Parse(lines(i).Split(vbTab)(2), ci))
            Next

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table5.txt")
                Using t As New IO.StreamReader(filestr)
                    t5 = t.ReadToEnd
                End Using
            End Using

            lines = t5.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            For i = 2 To 8
                T_HCl.Add(Double.Parse(lines(i).Split(vbTab)(0), ci))
                tau_HCl_A12.Add(Double.Parse(lines(i).Split(vbTab)(1), ci))
                tau_HCl_A21.Add(Double.Parse(lines(i).Split(vbTab)(2), ci))
            Next

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

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.table4_amines.txt")
                Using t As New IO.StreamReader(filestr)
                    t4a = t.ReadToEnd
                End Using
            End Using

            lines = t4a.Split(New Char() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)

            For i = 2 To lines.Count - 1
                Dim ip As New ElectrolyteNRTL_IPData
                ip.ID1 = lines(i).Split(vbTab)(0)
                ip.ID2 = lines(i).Split(vbTab)(1)
                ip.A12 = Double.Parse(lines(i).Split(vbTab)(2), ci)
                ip.B12 = Double.Parse(lines(i).Split(vbTab)(3), ci)
                ip.alpha12 = 0.2
                If Not InteractionParameters.ContainsKey(ip.ID1) Then InteractionParameters.Add(ip.ID1, New Dictionary(Of String, ElectrolyteNRTL_IPData))
                InteractionParameters(ip.ID1).Add(ip.ID2, ip)
            Next

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

        Function GetSaltFormula(cprops As List(Of Interfaces.ICompoundConstantProperties), cation As String, anion As String) As String

            Dim formula = ""

            For i As Integer = 0 To cprops.Count - 1
                If cprops(i).PositiveIon = cation And cprops(i).NegativeIon = anion Then
                    formula = cprops(i).Formula
                    Exit For
                End If
            Next

            If formula = "" And cation = "H3O+" And anion = "OH-" Then formula = "H2O"
            If formula = "" And cation = "H+" And anion = "OH-" Then formula = "H2O"

            Return formula

        End Function

        Function IsSalt(data As ENRTLData, i As Integer) As Boolean

            Return data.ConstantProperties(i).IsSalt

        End Function

        Function IsIon(data As ENRTLData, i As Integer) As Boolean

            Return data.ConstantProperties(i).Charge <> 0

        End Function

        Function IsCation(data As ENRTLData, i As Integer) As Boolean

            Return data.ConstantProperties(i).Charge > 0

        End Function

        Function IsAnion(data As ENRTLData, i As Integer) As Boolean

            Return data.ConstantProperties(i).Charge < 0

        End Function

        Function IsMolecule(data As ENRTLData, i As Integer) As Boolean

            If Not data.ConstantProperties(i).IsSalt And
                Not data.ConstantProperties(i).IsHydratedSalt And
                Not data.ConstantProperties(i).IsIon Then
                Return True
            Else
                Return False
            End If

        End Function

        Function GetNaOH_A12(T As Double) As Double

            Dim ip As New MathNet.Numerics.Interpolation.NevillePolynomialInterpolation(T_NaOH.ToArray, tau_NaOH_A12.ToArray)
            Return ip.Interpolate(T)

        End Function

        Function GetNaOH_A21(T As Double) As Double

            Dim ip As New MathNet.Numerics.Interpolation.NevillePolynomialInterpolation(T_NaOH.ToArray, tau_NaOH_A21.ToArray)
            Return ip.Interpolate(T)

        End Function

        Function GetHCl_A12(T As Double) As Double

            Dim ip As New MathNet.Numerics.Interpolation.NevillePolynomialInterpolation(T_HCl.ToArray, tau_HCl_A12.ToArray)
            Return ip.Interpolate(T)

        End Function

        Function GetHCl_A21(T As Double) As Double

            Dim ip As New MathNet.Numerics.Interpolation.NevillePolynomialInterpolation(T_HCl.ToArray, tau_HCl_A21.ToArray)
            Return ip.Interpolate(T)

        End Function

        Function TAU_k_m(data As ENRTLData, k As String, m As String) As Double

            If k = m Then Return 0.0

            Dim tau As Double = 0.0

            Dim Vids As String() = data.Vids

            If Not Vids.ToList.Contains(k) Or Not Vids.ToList.Contains(m) Then Return tau

            Dim T As Double = data.T

            If IsIon(data, Vids.ToList.IndexOf(k)) And IsMolecule(data, Vids.ToList.IndexOf(m)) Then
                'calculate tau[a/c]m
                Dim i As Integer = 0
                Dim sum1 As Double = 0
                Dim sum2 As Double = 0
                For i = 0 To data.ConstantProperties.Count - 1
                    If IsCation(data, Vids.ToList.IndexOf(k)) Then
                        If IsAnion(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, k, Vids(i))
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey((m)) Then
                                    tau = Me.InteractionParameters(s1)((m)).A12 + Me.InteractionParameters(s1)((m)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey((m)) Then
                                        If Me.InteractionParameters((m)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters((m))(s1).A21 + Me.InteractionParameters((m))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey((m)) Then
                                If Me.InteractionParameters((m)).ContainsKey((k)) Then
                                    tau = Me.InteractionParameters((m))(s1).A21 + Me.InteractionParameters((m))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    ElseIf IsAnion(data, Vids.ToList.IndexOf(k)) Then
                        If IsCation(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, Vids(i), k)
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey((m)) Then
                                    tau = Me.InteractionParameters(s1)((m)).A12 + Me.InteractionParameters(s1)((m)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey((m)) Then
                                        If Me.InteractionParameters((m)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters((m))(s1).A21 + Me.InteractionParameters((m))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey((m)) Then
                                If Me.InteractionParameters((m)).ContainsKey((k)) Then
                                    tau = Me.InteractionParameters((m))(s1).A21 + Me.InteractionParameters((m))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    End If
                Next
                tau = sum1 / sum2
            ElseIf IsIon(data, Vids.ToList.IndexOf(m)) And IsMolecule(data, Vids.ToList.IndexOf(k)) Then
                'calculate tau[c/a]
                Dim i As Integer = 0
                Dim sum1 As Double = 0
                Dim sum2 As Double = 0
                For i = 0 To data.ConstantProperties.Count - 1
                    If IsCation(data, Vids.ToList.IndexOf(m)) Then
                        If IsAnion(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, m, i)
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey((k)) Then
                                    tau = Me.InteractionParameters(s1)((k)).A12 + Me.InteractionParameters(s1)((k)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey((k)) Then
                                        If Me.InteractionParameters((k)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters((k))(s1).A21 + Me.InteractionParameters((k))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey((k)) Then
                                If Me.InteractionParameters((k)).ContainsKey(s1) Then
                                    tau = Me.InteractionParameters((k))(s1).A21 + Me.InteractionParameters((k))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    ElseIf IsAnion(data, Vids.ToList.IndexOf(m)) Then
                        If IsCation(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, i, m)
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey((k)) Then
                                    tau = Me.InteractionParameters(s1)((k)).A12 + Me.InteractionParameters(s1)((k)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey((k)) Then
                                        If Me.InteractionParameters((k)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters((k))(s1).A21 + Me.InteractionParameters((k))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey((k)) Then
                                If Me.InteractionParameters((k)).ContainsKey(s1) Then
                                    tau = Me.InteractionParameters((k))(s1).A21 + Me.InteractionParameters((k))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    End If
                Next
                tau = sum1 / sum2
            Else
                If Me.InteractionParameters.ContainsKey((k)) Then
                    If Me.InteractionParameters((k)).ContainsKey((m)) Then
                        tau = Me.InteractionParameters((k))((m)).A12 + Me.InteractionParameters((k))((m)).B12 / T
                    Else
                        If Me.InteractionParameters.ContainsKey((m)) Then
                            If Me.InteractionParameters((m)).ContainsKey((k)) Then
                                tau = Me.InteractionParameters((m))((k)).A21 + Me.InteractionParameters((m))((k)).B21 / T
                            End If
                        End If
                    End If
                ElseIf Me.InteractionParameters.ContainsKey((m)) Then
                    If Me.InteractionParameters((m)).ContainsKey((k)) Then
                        tau = Me.InteractionParameters((m))((k)).A21 + Me.InteractionParameters((m))((k)).B21 / T
                    End If
                End If
            End If

            If (k) = "H2O" Then
                If (m) = "NaOH" Or (m) = "Na+" Or (m) = "OH-" Then
                    tau = GetNaOH_A12(T)
                End If
                If (m) = "HCl" Or (m) = "H+" Or (m) = "Cl-" Then
                    tau = GetHCl_A12(T)
                End If
            End If
            If (m) = "H2O" Then
                If (k) = "NaOH" Or (k) = "Na+" Or (k) = "OH-" Then
                    tau = GetNaOH_A21(T)
                End If
                If (k) = "HCl" Or (k) = "H+" Or (k) = "Cl-" Then
                    tau = GetHCl_A21(T)
                End If
            End If

            Return tau

        End Function

        Function TAU_k_m(data As ENRTLData, k As Integer, m As Integer) As Double

            If k = m Then Return 0.0

            Dim tau As Double = 0.0

            Dim Vids As String() = data.Vids

            Dim T As Double = data.T

            If IsIon(data, k) And IsMolecule(data, m) Then
                'calculate tau[a/c]m
                Dim i As Integer = 0
                Dim sum1 As Double = 0
                Dim sum2 As Double = 0
                For i = 0 To data.ConstantProperties.Count - 1
                    If IsCation(data, k) Then
                        If IsAnion(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(k), data.Vids(i))
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey(Vids(m)) Then
                                    tau = Me.InteractionParameters(s1)(Vids(m)).A12 + Me.InteractionParameters(s1)(Vids(m)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey(Vids(m)) Then
                                        If Me.InteractionParameters(Vids(m)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters(Vids(m))(s1).A21 + Me.InteractionParameters(Vids(m))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey(Vids(m)) Then
                                If Me.InteractionParameters(Vids(m)).ContainsKey(Vids(k)) Then
                                    tau = Me.InteractionParameters(Vids(m))(s1).A21 + Me.InteractionParameters(Vids(m))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    ElseIf IsAnion(data, k) Then
                        If IsCation(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(i), data.Vids(k))
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey(Vids(m)) Then
                                    tau = Me.InteractionParameters(s1)(Vids(m)).A12 + Me.InteractionParameters(s1)(Vids(m)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey(Vids(m)) Then
                                        If Me.InteractionParameters(Vids(m)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters(Vids(m))(s1).A21 + Me.InteractionParameters(Vids(m))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey(Vids(m)) Then
                                If Me.InteractionParameters(Vids(m)).ContainsKey(Vids(k)) Then
                                    tau = Me.InteractionParameters(Vids(m))(s1).A21 + Me.InteractionParameters(Vids(m))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    End If
                Next
                tau = sum1 / sum2
            ElseIf IsIon(data, m) And IsMolecule(data, k) Then
                'calculate tau[c/a]
                Dim i As Integer = 0
                Dim sum1 As Double = 0
                Dim sum2 As Double = 0
                For i = 0 To data.ConstantProperties.Count - 1
                    If IsCation(data, m) Then
                        If IsAnion(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(m), data.Vids(i))
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey(Vids(k)) Then
                                    tau = Me.InteractionParameters(s1)(Vids(k)).A12 + Me.InteractionParameters(s1)(Vids(k)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey(Vids(k)) Then
                                        If Me.InteractionParameters(Vids(k)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters(Vids(k))(s1).A21 + Me.InteractionParameters(Vids(k))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey(Vids(k)) Then
                                If Me.InteractionParameters(Vids(k)).ContainsKey(s1) Then
                                    tau = Me.InteractionParameters(Vids(k))(s1).A21 + Me.InteractionParameters(Vids(k))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    ElseIf IsAnion(data, m) Then
                        If IsCation(data, i) Then
                            Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(i), data.Vids(m))
                            If Me.InteractionParameters.ContainsKey(s1) Then
                                If Me.InteractionParameters(s1).ContainsKey(Vids(k)) Then
                                    tau = Me.InteractionParameters(s1)(Vids(k)).A12 + Me.InteractionParameters(s1)(Vids(k)).B12 / T
                                Else
                                    If Me.InteractionParameters.ContainsKey(Vids(k)) Then
                                        If Me.InteractionParameters(Vids(k)).ContainsKey(s1) Then
                                            tau = Me.InteractionParameters(Vids(k))(s1).A21 + Me.InteractionParameters(Vids(k))(s1).B21 / T
                                        End If
                                    End If
                                End If
                            ElseIf Me.InteractionParameters.ContainsKey(Vids(k)) Then
                                If Me.InteractionParameters(Vids(k)).ContainsKey(s1) Then
                                    tau = Me.InteractionParameters(Vids(k))(s1).A21 + Me.InteractionParameters(Vids(k))(s1).B21 / T
                                End If
                            End If
                            sum1 += data.X(i) * tau
                            sum2 += data.X(i)
                        End If
                    End If
                Next
                tau = sum1 / sum2
            Else
                If Me.InteractionParameters.ContainsKey(Vids(k)) Then
                    If Me.InteractionParameters(Vids(k)).ContainsKey(Vids(m)) Then
                        tau = Me.InteractionParameters(Vids(k))(Vids(m)).A12 + Me.InteractionParameters(Vids(k))(Vids(m)).B12 / T
                    Else
                        If Me.InteractionParameters.ContainsKey(Vids(m)) Then
                            If Me.InteractionParameters(Vids(m)).ContainsKey(Vids(k)) Then
                                tau = Me.InteractionParameters(Vids(m))(Vids(k)).A21 + Me.InteractionParameters(Vids(m))(Vids(k)).B21 / T
                            End If
                        End If
                    End If
                ElseIf Me.InteractionParameters.ContainsKey(Vids(m)) Then
                    If Me.InteractionParameters(Vids(m)).ContainsKey(Vids(k)) Then
                        tau = Me.InteractionParameters(Vids(m))(Vids(k)).A21 + Me.InteractionParameters(Vids(m))(Vids(k)).B21 / T
                    End If
                End If
            End If

            If Vids(k) = "H2O" And (Vids.Contains("NaOH") Or Vids.Contains("HCl")) Then
                If Vids(m) = "NaOH" Or Vids(m) = "Na+" Or Vids(m) = "OH-" Then
                    tau = GetNaOH_A12(T)
                End If
                If Vids(m) = "HCl" Or Vids(m) = "H+" Or Vids(m) = "Cl-" Then
                    tau = GetHCl_A12(T)
                End If
            End If
            If Vids(m) = "H2O" And (Vids.Contains("NaOH") Or Vids.Contains("HCl")) Then
                If Vids(k) = "NaOH" Or Vids(k) = "Na+" Or Vids(k) = "OH-" Then
                    tau = GetNaOH_A21(T)
                End If
                If Vids(k) = "HCl" Or Vids(k) = "H+" Or Vids(k) = "Cl-" Then
                    tau = GetHCl_A21(T)
                End If
            End If

            Return tau

        End Function

        Function TAU_ki_ji(data As ENRTLData, k As Integer, i As Integer, j As Integer) As Double

            Return TAU_k_m(data, i, k) - TAU_ca_m(data, k, j, i) + TAU_m_ca(data, k, j, i)

        End Function

        Function TAU_m_ca(data As ENRTLData, m As Integer, c As Integer, a As Integer) As Double

            If IsAnion(data, a) And IsCation(data, c) Then

                Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(c), data.Vids(a))

                If s1 = "" Then
                    Return TAU_k_m(data, data.Vids(m), data.Vids(c) + "," + data.Vids(a))
                Else
                    Return TAU_k_m(data, data.Vids(m), s1)
                End If

            ElseIf IsCation(data, a) And IsAnion(data, c) Then

                Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(a), data.Vids(c))

                If s1 = "" Then
                    Return TAU_k_m(data, data.Vids(m), data.Vids(a) + "," + data.Vids(c))
                Else
                    Return TAU_k_m(data, data.Vids(m), s1)
                End If

            Else

                'undefined

                Return 0.0

            End If

        End Function

        Function TAU_ca_m(data As ENRTLData, m As Integer, c As Integer, a As Integer) As Double

            If IsAnion(data, a) And IsCation(data, c) Then

                Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(c), data.Vids(a))

                If s1 = "" Then
                    Return TAU_k_m(data, data.Vids(c) + "," + data.Vids(a), data.Vids(m))
                Else
                    Return TAU_k_m(data, s1, data.Vids(m))
                End If

            ElseIf IsCation(data, a) And IsAnion(data, c) Then

                Dim s1 = GetSaltFormula(data.ConstantProperties, data.Vids(a), data.Vids(c))

                If s1 = "" Then
                    Return TAU_k_m(data, data.Vids(a) + "," + data.Vids(c), data.Vids(m))
                Else
                    Return TAU_k_m(data, s1, data.Vids(m))
                End If

            Else

                'undefined

                Return 0.0

            End If

        End Function

        Function alpha_k_m(data As ENRTLData, k As Integer, m As Integer) As Double

            If k = m Then Return 0.0

            Dim alpha12 As Double = 0.0

            Dim Vids As String() = data.Vids

            If Me.InteractionParameters.ContainsKey(Vids(k)) Then
                If Me.InteractionParameters(Vids(k)).ContainsKey(Vids(m)) Then
                    alpha12 = Me.InteractionParameters(Vids(k))(Vids(m)).alpha12
                Else
                    If Me.InteractionParameters.ContainsKey(Vids(m)) Then
                        If Me.InteractionParameters(Vids(m)).ContainsKey(Vids(k)) Then
                            alpha12 = Me.InteractionParameters(Vids(m))(Vids(k)).alpha12
                        End If
                    End If
                End If
            ElseIf Me.InteractionParameters.ContainsKey(Vids(m)) Then
                If Me.InteractionParameters(Vids(m)).ContainsKey(Vids(k)) Then
                    alpha12 = Me.InteractionParameters(Vids(m))(Vids(k)).alpha12
                End If
            End If

            If Vids(k) = "H2O" Then
                If Vids(m) = "NaOH" Or Vids(m) = "Na+" Or Vids(m) = "OH-" Then
                    alpha12 = 0.03
                End If
                If Vids(m) = "HCl" Or Vids(m) = "H+" Or Vids(m) = "Cl-" Then
                    alpha12 = 0.03
                End If
            End If
            If Vids(m) = "H2O" Then
                If Vids(k) = "NaOH" Or Vids(k) = "Na+" Or Vids(k) = "OH-" Then
                    alpha12 = 0.03
                End If
                If Vids(k) = "HCl" Or Vids(k) = "H+" Or Vids(k) = "Cl-" Then
                    alpha12 = 0.03
                End If
            End If

            Return alpha12

        End Function

        Function alpha_ki_ji(data As ENRTLData, k As Integer, i As Integer, j As Integer) As Double

            Return 0.2

        End Function

        Function alpha_m_ca(data As ENRTLData, m As Integer, c As Integer, a As Integer) As Double

            Return 0.2

        End Function

        Function alpha_ca_m(data As ENRTLData, m As Integer, c As Integer, a As Integer) As Double

            Return 0.2

        End Function

        Function G_k_m(data As ENRTLData, k As Integer, m As Integer) As Double

            If k = m Then Return 1.0

            If IsIon(data, k) And IsMolecule(data, m) Then
                'calculate G[a/c]m
                Dim i As Integer = 0
                Dim sum1 As Double = 0
                Dim sum2 As Double = 0
                For i = 0 To data.ConstantProperties.Count - 1
                    If IsCation(data, k) Then
                        If IsAnion(data, i) Then
                            sum1 += data.X(i) * G_ca_m(data, m, k, i)
                            sum2 += data.X(i)
                        End If
                    ElseIf IsAnion(data, k) Then
                        If IsCation(data, i) Then
                            sum1 += data.X(i) * G_ca_m(data, m, i, k)
                            sum2 += data.X(i)
                        End If
                    End If
                Next
                Return sum1 / sum2
            ElseIf IsIon(data, m) And IsMolecule(data, k) Then
                'calculate Gm[c/a]
                Dim i As Integer = 0
                Dim sum1 As Double = 0
                Dim sum2 As Double = 0
                For i = 0 To data.ConstantProperties.Count - 1
                    If IsCation(data, m) Then
                        If IsAnion(data, i) Then
                            sum1 += data.X(i) * G_ca_m(data, k, m, i)
                            sum2 += data.X(i)
                        End If
                    ElseIf IsAnion(data, m) Then
                        If IsCation(data, i) Then
                            sum1 += data.X(i) * G_ca_m(data, k, i, m)
                            sum2 += data.X(i)
                        End If
                    End If
                Next
                Return sum1 / sum2
            Else
                Return Exp(-alpha_k_m(data, k, m) * TAU_k_m(data, k, m))
            End If

        End Function

        Function G_ki_ji(data As ENRTLData, k As Integer, i As Integer, j As Integer) As Double

            Return Exp(-alpha_ki_ji(data, k, i, j) * TAU_ki_ji(data, k, i, j))

        End Function

        Function G_m_ca(data As ENRTLData, m As Integer, c As Integer, a As Integer) As Double

            Return Exp(-alpha_m_ca(data, m, c, a) * TAU_m_ca(data, m, c, a))

        End Function

        Function G_ca_m(data As ENRTLData, m As Integer, c As Integer, a As Integer) As Double

            Return Exp(-alpha_ca_m(data, m, c, a) * TAU_ca_m(data, m, c, a))

        End Function

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double()

            Dim n As Integer = Vx.Length - 1

            Dim X(n) As Double

            Dim i, j, wi As Integer
            'Dim k, m1, a, a1, c, c1 As Integer

            Dim Vids(n) As String, vsolv(n), charge(n), molality(n), solvdensity(n), solvvfrac(n), solvmfrac(n) As Double
            Dim Msolv, DCsolv, dsolv, Xsolv, Im, A0 As Double
            Dim lnglr(n), lngb(n), lngsr(n), lng(n) As Double

            i = 0
            Do
                Vids(i) = cprops(i).Formula
                If Vids(i) = "HOH" Then Vids(i) = "H2O"
                If Vids(i) = "OCO" Then Vids(i) = "CO2"
                If Vids(i) = "HSH" Then Vids(i) = "H2S"
                If Vids(i) = "HOCH2CH2NH2" Then Vids(i) = "RNH2" 'MEA
                If Vids(i) = "(HOCH2CH2)2NH" Then Vids(i) = "R2NH" 'DEA
                If Vids(i) = "HOCH2CH2NH3+" Then Vids(i) = "RNH3+" 'Protonated MEA
                If Vids(i) = "(HOCH2CH2)2NH2+" Then Vids(i) = "R2NH2+" 'Protonated DEA
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
                If cprops(i).Charge = 0 And Not cprops(i).IsSalt Then
                    'solvents
                    Xsolv -= Vx(i)
                End If
                i += 1
            Loop Until i = n + 1

            Dim sumvfrac As Double = 0

            i = 0
            Do
                If cprops(i).Charge = 0 And Not cprops(i).IsSalt Then
                    'solvents
                    sumvfrac += Vx(i) / Xsolv * cprops(i).Chao_Seader_Liquid_Molar_Volume / 1000000.0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If cprops(i).Charge = 0 And Not cprops(i).IsSalt Then
                    'solvents
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
                ElseIf cprops(i).Name = "Monoethanolamine" Then
                    Msolv += Vx(i) / Xsolv * cprops(i).Molar_Weight / 1000
                    dsolv += solvvfrac(i) * (35.76 + 14836 * (1 / T - 1 / 273.15))
                ElseIf cprops(i).Name = "Diethanolamine" Then
                    Msolv += Vx(i) / Xsolv * cprops(i).Molar_Weight / 1000
                    dsolv += solvvfrac(i) * (28.01 + 9277 * (1 / T - 1 / 273.15))
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

            'local concentrations

            For i = 0 To n
                If cprops(i).Charge <> 0 Then
                    X(i) = Vx(i) * Abs(cprops(i).Charge)
                Else
                    X(i) = Vx(i)
                End If
            Next

            'mixture data

            Dim edata As New ENRTLData With {.ConstantProperties = cprops, .Vz = Vx, .X = X, .Vids = Vids, .T = T}

            'c/a sums

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

            'Parallel.For(0, n + 1, Sub(m1P)
            '                           Dim kP As Integer
            '                           s0(m1P) = 0
            '                           s0t(m1P) = 0
            '                           For kp = 0 To n
            '                               s0(m1P) += X(kP) * G_k_m(edata, kP, m1P)
            '                               s0t(m1P) += X(kP) * G_k_m(edata, kP, m1P) * TAU_k_m(edata, kP, m1P)
            '                           Next
            '                       End Sub)

            For m1 = 0 To n
                s0(m1) = 0
                s0t(m1) = 0
                For k = 0 To n
                    s0(m1) += X(k) * G_k_m(edata, k, m1)
                    s0t(m1) += X(k) * G_k_m(edata, k, m1) * TAU_k_m(edata, k, m1)
                Next
            Next

            Dim s1(n)(), s1t(n)() As Double

            For i = 0 To n
                Array.Resize(s1(i), n + 1)
                Array.Resize(s1t(i), n + 1)
            Next

            Parallel.For(0, n + 1, Sub(cP2)
                                       Dim a1P, kP As Integer
                                       For a1P = 0 To n
                                           s1(cP2)(a1P) = 0
                                           s1t(cP2)(a1P) = 0
                                           For kP = 0 To n
                                               If kP <> cP2 Then
                                                   s1(cP2)(a1P) += X(kP) * G_ki_ji(edata, kP, cP2, a1P)
                                                   s1t(cP2)(a1P) += X(kP) * G_ki_ji(edata, kP, cP2, a1P) * TAU_ki_ji(edata, kP, cP2, a1P)
                                               End If
                                           Next
                                       Next
                                   End Sub)

            'For c = 0 To n
            '    For a1 = 0 To n
            '        s1(c)(a1) = 0
            '        s1t(c)(a1) = 0
            '        For k = 0 To n
            '            If k <> c Then
            '                s1(c)(a1) += X(k) * G_ki_ji(edata, k, c, a1)
            '                s1t(c)(a1) += X(k) * G_ki_ji(edata, k, c, a1) * TAU_ki_ji(edata, k, c, a1)
            '            End If
            '        Next
            '    Next
            'Next

            Dim s2(n)(), s2t(n)() As Double

            For i = 0 To n
                Array.Resize(s2(i), n + 1)
                Array.Resize(s2t(i), n + 1)
            Next

            Parallel.For(0, n + 1, Sub(aP)
                                       Dim c1P, kP As Integer
                                       For c1P = 0 To n
                                           s2(aP)(c1P) = 0
                                           s2t(aP)(c1P) = 0
                                           For kP = 0 To n
                                               If kP <> aP Then
                                                   s2(aP)(c1P) += X(kP) * G_ki_ji(edata, kP, aP, c1P)
                                                   s2t(aP)(c1P) += X(kP) * G_ki_ji(edata, kP, aP, c1P) * TAU_ki_ji(edata, kP, aP, c1P)
                                               End If
                                           Next
                                       Next
                                   End Sub)

            'For a = 0 To n
            '    For c1 = 0 To n
            '        s2(a)(c1) = 0
            '        s2t(a)(c1) = 0
            '        For k = 0 To n
            '            If k <> a Then
            '                s2(a)(c1) += X(k) * G_ki_ji(edata, k, a, c1)
            '                s2t(a)(c1) += X(k) * G_ki_ji(edata, k, a, c1) * TAU_ki_ji(edata, k, a, c1)
            '            End If
            '        Next
            '    Next
            'Next

            Dim sm1(n), sm2(n), sm3(n), sma1(n), sma2(n), sma3(n), smc1(n), smc2(n), smc3(n) As Double

            Parallel.For(0, n + 1, Sub(mP)
                                       Dim tasks As New List(Of Task)
                                       tasks.Add(New Task(Sub()
                                                              Dim m1P As Integer
                                                              sm1(mP) = 0.0
                                                              For m1P = 0 To n
                                                                  If cprops(m1P).Charge = 0 Then
                                                                      sm1(mP) += X(m1P) * G_k_m(edata, mP, m1P) / s0(m1P) * (TAU_k_m(edata, mP, m1P) - s0t(m1P) / s0(m1P))
                                                                  End If
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim c1P, kP As Integer
                                                              sma1(mP) = 0.0
                                                              For c1P = 0 To n
                                                                  For kP = 0 To n
                                                                      If kP <> mP And cprops(mP).Charge < 0 And cprops(c1P).Charge > 0 Then
                                                                          sma1(mP) += x0(c1P) * X(kP) * G_ki_ji(edata, kP, c1P, mP) * TAU_ki_ji(edata, kP, c1P, mP) / s2(mP)(c1P)
                                                                      End If
                                                                  Next
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim a1P, kP As Integer
                                                              smc1(mP) = 0.0
                                                              For a1P = 0 To n
                                                                  For kP = 0 To n
                                                                      If kP <> mP And cprops(mP).Charge > 0 And cprops(a1P).Charge < 0 Then
                                                                          smc1(mP) += x0(a1P) * X(kP) * G_ki_ji(edata, kP, a1P, mP) * TAU_ki_ji(edata, kP, a1P, mP) / s2(mP)(a1P)
                                                                      End If
                                                                  Next
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim cP2, a1P As Integer
                                                              sm2(mP) = 0.0
                                                              For cP2 = 0 To n
                                                                  If cprops(cP2).Charge > 0 Then
                                                                      For a1P = 0 To n
                                                                          If cprops(a1P).Charge < 0 Then
                                                                              sm2(mP) += x0(a1P) * X(cP2) * G_ki_ji(edata, mP, cP2, a1P) / s1(cP2)(a1P) * (TAU_ki_ji(edata, mP, cP2, a1P) - s1t(cP2)(a1P) / s1(cP2)(a1P))
                                                                          End If
                                                                      Next
                                                                  End If
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim m1P As Integer
                                                              sma2(mP) = 0.0
                                                              For m1P = 0 To n
                                                                  If cprops(mP).Charge < 0 And cprops(m1P).Charge = 0 Then
                                                                      sma2(mP) += X(m1P) * G_k_m(edata, mP, m1P) / s0(m1P) * (TAU_k_m(edata, mP, m1P) - s0t(m1P) / s0(m1P))
                                                                  End If
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim m1P As Integer
                                                              smc2(mP) = 0.0
                                                              For m1P = 0 To n
                                                                  If cprops(mP).Charge > 0 And cprops(m1P).Charge = 0 Then
                                                                      smc2(mP) += X(m1P) * G_k_m(edata, mP, m1P) / s0(m1P) * (TAU_k_m(edata, mP, m1P) - s0t(m1P) / s0(m1P))
                                                                  End If
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim aP, c1P As Integer
                                                              sm3(mP) = 0.0
                                                              For aP = 0 To n
                                                                  If cprops(aP).Charge < 0 Then
                                                                      For c1P = 0 To n
                                                                          If cprops(c1P).Charge > 0 Then
                                                                              sm3(mP) += x0(c1P) * X(aP) * G_ki_ji(edata, mP, c1P, aP) / s2(aP)(c1P) * (TAU_ki_ji(edata, mP, c1P, aP) - s2t(aP)(c1P) / s2(aP)(c1P))
                                                                          End If
                                                                      Next
                                                                  End If
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim a1P, c As Integer
                                                              sma3(mP) = 0.0
                                                              For c = 0 To n
                                                                  If cprops(c).Charge > 0 Then
                                                                      For a1P = 0 To n
                                                                          If cprops(a1P).Charge < 0 Then
                                                                              sma3(mP) += x0(a1P) * X(c) * G_ki_ji(edata, mP, c, a1P) / s2(c)(a1P) * (TAU_ki_ji(edata, mP, c, a1P) - s2t(c)(a1P) / s2(c)(a1P))
                                                                          End If
                                                                      Next
                                                                  End If
                                                              Next
                                                          End Sub))
                                       tasks.Add(New Task(Sub()
                                                              Dim aP, c1P As Integer
                                                              smc3(mP) = 0.0
                                                              For aP = 0 To n
                                                                  If cprops(aP).Charge < 0 Then
                                                                      For c1P = 0 To n
                                                                          If cprops(c1P).Charge > 0 Then
                                                                              smc3(mP) += x0(c1P) * X(aP) * G_ki_ji(edata, mP, c1P, aP) / s2(aP)(c1P) * (TAU_ki_ji(edata, mP, c1P, aP) - s2t(aP)(c1P) / s2(aP)(c1P))
                                                                          End If
                                                                      Next
                                                                  End If
                                                              Next
                                                          End Sub))
                                       Parallel.ForEach(tasks, Sub(tk) tk.Start())
                                       Task.WaitAll(tasks.ToArray)
                                   End Sub)

            'For m = 0 To n
            '    sm1(m) = 0.0
            '    For m1 = 0 To n
            '        If cprops(m1).Charge = 0 Then
            '            sm1(m) += X(m1) * G_k_m(edata, m, m1) / s0(m1) * (TAU_k_m(edata, m, m1) - s0t(m1) / s0(m1))
            '        End If
            '    Next
            '    sma1(m) = 0.0
            '    For c1 = 0 To n
            '        For k = 0 To n
            '            If k <> m And cprops(m).Charge < 0 And cprops(c1).Charge > 0 Then
            '                sma1(m) += x0(c1) * X(k) * G_ki_ji(edata, k, c1, m) * TAU_ki_ji(edata, k, c1, m) / s2(m)(c1)
            '            End If
            '        Next
            '    Next
            '    smc1(m) = 0.0
            '    For a1 = 0 To n
            '        For k = 0 To n
            '            If k <> m And cprops(m).Charge > 0 And cprops(a1).Charge < 0 Then
            '                smc1(m) += x0(a1) * X(k) * G_ki_ji(edata, k, a1, m) * TAU_ki_ji(edata, k, a1, m) / s2(m)(a1)
            '            End If
            '        Next
            '    Next
            '    sm2(m) = 0.0
            '    For c = 0 To n
            '        If cprops(c).Charge > 0 Then
            '            For a1 = 0 To n
            '                If cprops(a1).Charge < 0 Then
            '                    sm2(m) += x0(a1) * X(c) * G_ki_ji(edata, m, c, a1) / s1(c)(a1) * (TAU_ki_ji(edata, m, c, a1) - s1t(c)(a1) / s1(c)(a1))
            '                End If
            '            Next
            '        End If
            '    Next
            '    sma2(m) = 0.0
            '    For m1 = 0 To n
            '        If cprops(m).Charge < 0 And cprops(m1).Charge = 0 Then
            '            sma2(m) += X(m1) * G_k_m(edata, m, m1) / s0(m1) * (TAU_k_m(edata, m, m1) - s0t(m1) / s0(m1))
            '        End If
            '    Next
            '    smc2(m) = 0.0
            '    For m1 = 0 To n
            '        If cprops(m).Charge > 0 And cprops(m1).Charge = 0 Then
            '            smc2(m) += X(m1) * G_k_m(edata, m, m1) / s0(m1) * (TAU_k_m(edata, m, m1) - s0t(m1) / s0(m1))
            '        End If
            '    Next
            '    sm3(m) = 0.0
            '    For a = 0 To n
            '        If cprops(a).Charge < 0 Then
            '            For c1 = 0 To n
            '                If cprops(c1).Charge > 0 Then
            '                    sm3(m) += x0(c1) * X(a) * G_ki_ji(edata, m, c1, a) / s2(a)(c1) * (TAU_ki_ji(edata, m, c1, a) - s2t(a)(c1) / s2(a)(c1))
            '                End If
            '            Next
            '        End If
            '    Next
            '    sma3(m) = 0.0
            '    For c = 0 To n
            '        If cprops(c).Charge > 0 Then
            '            For a1 = 0 To n
            '                If cprops(a1).Charge < 0 Then
            '                    sma3(m) += x0(a1) * X(c) * G_ki_ji(edata, m, c, a1) / s2(c)(a1) * (TAU_ki_ji(edata, m, c, a1) - s2t(c)(a1) / s2(c)(a1))
            '                End If
            '            Next
            '        End If
            '    Next
            '    smc3(m) = 0.0
            '    For a = 0 To n
            '        If cprops(a).Charge < 0 Then
            '            For c1 = 0 To n
            '                If cprops(c1).Charge > 0 Then
            '                    smc3(m) += x0(c1) * X(a) * G_ki_ji(edata, m, c1, a) / s2(a)(c1) * (TAU_ki_ji(edata, m, c1, a) - s2t(a)(c1) / s2(a)(c1))
            '                End If
            '            Next
            '        End If
            '    Next
            'Next

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
