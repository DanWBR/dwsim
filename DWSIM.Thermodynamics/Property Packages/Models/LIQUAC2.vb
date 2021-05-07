'    LIQUAC2 Property Package 
'    Copyright 2012 Daniel Wagner O. de Medeiros
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
'    Reference: https://pubs.acs.org/doi/10.1021/ie0510122

Imports System.Collections.Generic
Imports FileHelpers
Imports System.Math
Imports System.Linq


Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> _
    Public Class LIQUAC2_IPData

        Implements ICloneable

        Public ID1 As Integer = -1
        Public ID2 As Integer = -1
        Public Group1 As String = ""
        Public Group2 As String = ""
        Public A12 As Double = 0
        Public A21 As Double = 0
        Public B12 As Double = 0
        Public C12 As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New LIQUAC2_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .Group1 = Me.Group1
                .Group2 = Me.Group2
                .A12 = Me.A12
                .A21 = Me.A21
                .B12 = Me.B12
                .C12 = Me.C12
            End With
            Return newclass
        End Function

    End Class

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> _
    Public Class LIQUAC2_RiQiData

        Implements ICloneable

        Public Name As String = ""
        Public Formula As String = ""
        Public R As Double = 0
        Public Q As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New LIQUAC2_RiQiData
            With newclass
                .Name = Me.Name
                .Formula = Me.Formula
                .R = Me.R
                .Q = Me.Q
            End With
            Return newclass
        End Function

    End Class

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> _
    Public Class DielectricConstants

        Implements ICloneable

        Public Name As String = ""
        Public a As Double = 0
        Public b As Double = 0
        Public c As Double = 0
        Public d As Double = 0
        Public e As Double = 0
        Public Tmin As Double = 0
        Public Tmax As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New DielectricConstants
            With newclass
                .Name = Me.Name
                .a = Me.a
                .b = Me.b
                .c = Me.c
                .d = Me.d
                .e = Me.e
                .Tmin = Me.Tmin
                .Tmax = Me.Tmax
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class LIQUAC2

        Private _ipl As Dictionary(Of String, Dictionary(Of String, LIQUAC2_IPData))
        Private _rq As Dictionary(Of String, LIQUAC2_RiQiData)
        Private _dc As Dictionary(Of String, DielectricConstants)

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, LIQUAC2_IPData))
            Get
                Return _ipl
            End Get
        End Property

        Public ReadOnly Property ModelData() As Dictionary(Of String, LIQUAC2_RiQiData)
            Get
                Return _rq
            End Get
        End Property

        Public ReadOnly Property DielectricConstants() As Dictionary(Of String, DielectricConstants)
            Get
                Return _dc
            End Get
        End Property

        Sub New()

            _ipl = New Dictionary(Of String, Dictionary(Of String, LIQUAC2_IPData))
            _rq = New Dictionary(Of String, LIQUAC2_RiQiData)
            _dc = New Dictionary(Of String, DielectricConstants)

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim uniquacip As UNIQUAC_IPData
            Dim uniquacipc() As UNIQUAC_IPData
            Dim liquac2 As LIQUAC2_IPData
            Dim liquac2c() As LIQUAC2_IPData
            Dim liquac2rq As LIQUAC2_RiQiData
            Dim liquac2rqc() As LIQUAC2_RiQiData
            Dim liquac2dc As DielectricConstants
            Dim liquac2dcc() As DielectricConstants
            Dim fh1 As New FileHelperEngine(Of UNIQUAC_IPData)
            Dim fh2 As New FileHelperEngine(Of LIQUAC2_IPData)
            Dim fh3 As New FileHelperEngine(Of LIQUAC2_RiQiData)
            Dim fh4 As New FileHelperEngine(Of DielectricConstants)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.uniquac.dat")
                Using t As New IO.StreamReader(filestr)
                    uniquacipc = fh1.ReadStream(t)
                End Using
            End Using
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.LIQUAC2_IP.txt")
                Using t As New IO.StreamReader(filestr)
                    liquac2c = fh2.ReadStream(t)
                End Using
            End Using
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.LIQUAC2_RiQi.txt")
                Using t As New IO.StreamReader(filestr)
                    liquac2rqc = fh3.ReadStream(t)
                End Using
            End Using
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.dielectricconstants.txt")
                Using t As New IO.StreamReader(filestr)
                    liquac2dcc = fh4.ReadStream(t)
                End Using
            End Using

            Dim csdb As New ChemSepHelper.ChemSepIDConverter

            For Each uniquacip In uniquacipc
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(uniquacip.ID1)) Then
                    If Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                    Else
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.CloneToLIQUAC)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(uniquacip.ID1), New Dictionary(Of String, LIQUAC2_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.CloneToLIQUAC)
                End If
            Next

            For Each uniquacip In uniquacipc
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(uniquacip.ID1)) Then
                    If Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                    Else
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.CloneToLIQUAC)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(uniquacip.ID1), New Dictionary(Of String, LIQUAC2_IPData))
                    Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.CloneToLIQUAC)
                End If
            Next

            For Each liquac2 In liquac2c
                If Me.InteractionParameters.ContainsKey(liquac2.Group1) Then
                    If Not Me.InteractionParameters(liquac2.Group1).ContainsKey(liquac2.Group2) Then
                        Me.InteractionParameters(liquac2.Group1).Add((liquac2.Group2), liquac2.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(liquac2.Group1, New Dictionary(Of String, LIQUAC2_IPData))
                    Me.InteractionParameters(liquac2.Group1).Add(liquac2.Group2, liquac2.Clone)
                End If
            Next

            For Each liquac2rq In liquac2rqc
                Me.ModelData.Add(liquac2rq.Formula, liquac2rq.Clone)
            Next

            For Each liquac2dc In liquac2dcc
                Me.DielectricConstants.Add(liquac2dc.Name, liquac2dc.Clone)
            Next

        End Sub

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Array

            Dim n As Integer = Vx.Length - 1

            Dim tau_ij(n, n), tau_ji(n, n), a12(n, n), a21(n, n), b12(n, n), c12(n, n) As Double
            Dim Vids(n) As String, VQ(n), VR(n), vsolv(n), charge(n), molality(n), solvdensity(n), solvvfrac(n), solvmfrac(n) As Double
            Dim Msolv, DCsolv, dsolv, Xsolv, Im, A, b, a1(n, n), a2(n, n), Bij(n, n), Bref(n, n), dBdIm(n, n), sigma As Double

            Dim i, j, k, l As Integer

            i = 0
            Do
                If cprops(i).IsIon Then
                    Vids(i) = cprops(i).Formula
                    charge(i) = cprops(i).Charge
                    If Vx(i) = 0.0# Then Vx(i) = 0.0000000001
                Else
                    Vids(i) = cprops(i).Name
                    charge(i) = 0.0#
                    solvdensity(i) = cprops(i).Molar_Weight / cprops(i).Chao_Seader_Liquid_Molar_Volume * 1000
                End If
                If Me.ModelData.ContainsKey(cprops(i).Formula) Then
                    VQ(i) = Me.ModelData(cprops(i).Formula).Q
                    VR(i) = Me.ModelData(cprops(i).Formula).R
                Else
                    VQ(i) = cprops(i).UNIQUAC_Q
                    VR(i) = cprops(i).UNIQUAC_R
                End If
                i = i + 1
            Loop Until i = n + 1

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
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
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                    sumvfrac = Vx(i) / Xsolv * cprops(i).Chao_Seader_Liquid_Molar_Volume / 1000000.0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
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
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                    Msolv += Vx(i) / Xsolv * cprops(i).Molar_Weight / 1000
                    With Me.DielectricConstants(cprops(i).Name)
                        DCsolv += solvvfrac(i) * (.a + .b * T + .c * T ^ 2 + .d * T ^ 3 + .e * T ^ 4)
                    End With
                    dsolv += solvvfrac(i) * solvdensity(i)
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Im = 0.0#
            Do
                Im += cprops(i).Charge ^ 2 * molality(i) / 2
                i += 1
            Loop Until i = n + 1

            A = 132775.7 * dsolv ^ 0.5 / (DCsolv * T) ^ (1.5)
            b = 6.359696 * dsolv ^ 0.5 / (DCsolv * T) ^ 0.5

            i = 0
            Do
                j = 0
                Do
                    If Me.InteractionParameters.ContainsKey(Vids(i)) Then
                        If Me.InteractionParameters(Vids(i)).ContainsKey(Vids(j)) Then
                            a12(i, j) = Me.InteractionParameters(Vids(i))(Vids(j)).A12
                            a21(i, j) = Me.InteractionParameters(Vids(i))(Vids(j)).A21
                            b12(i, j) = Me.InteractionParameters(Vids(i))(Vids(j)).B12
                            c12(i, j) = Me.InteractionParameters(Vids(i))(Vids(j)).C12
                        Else
                            If Me.InteractionParameters.ContainsKey(Vids(j)) Then
                                If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                                    a12(i, j) = Me.InteractionParameters(Vids(j))(Vids(i)).A21
                                    a21(i, j) = Me.InteractionParameters(Vids(j))(Vids(i)).A12
                                    b12(i, j) = Me.InteractionParameters(Vids(j))(Vids(i)).B12
                                    c12(i, j) = Me.InteractionParameters(Vids(j))(Vids(i)).C12
                                End If
                            End If
                        End If
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    tau_ij(i, j) = Math.Exp(-a12(i, j) / T)
                    tau_ji(i, j) = Math.Exp(-a21(i, j) / T)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim r, q As Double

            i = 0
            Do
                r += Vx(i) * VR(i)
                q += Vx(i) * VQ(i)
                i = i + 1
            Loop Until i = n + 1

            Dim teta(n), fi(n), S(n), lngc(n), lngr(n), lngmr(n), lnglr(n), lngsr(n), lng(n), g(n), sum1(n) As Double

            'long range term

            For i = 0 To n
                With cprops(i)
                    If .IsIon Then
                        lnglr(i) = -A * .Charge ^ 2 * Im ^ 0.5 / (1 + b * Im ^ 0.5)
                    ElseIf .Name = "Water" Or .Name = "Methanol" Then
                        'lnglr(i) = 2 * A * .Molar_Weight / 1000 * dsolv / (b ^ 3 * solvdensity(i)) * (1 + b * Im ^ 0.5 - 1 / (b * Im ^ 0.5) - 2 * Log(b * Im ^ 0.5))
                        sigma = 3 / Vx(i) ^ 3 * (1 + Vx(i) - 1 / (1 + Vx(i)) - 2 * Log(1 + Vx(i)))
                        lnglr(i) = 2 / 3 * A * .Molar_Weight / 1000 * Im ^ (3 / 2) * sigma * (b * Im ^ 0.5)
                    Else
                        lnglr(i) = 0.0#
                    End If
                End With
            Next

            'middle range term

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).IsIon And cprops(j).IsIon Then
                        a1(i, j) = -1
                        a2(i, j) = 0.25 / (Abs(cprops(i).Charge - cprops(j).Charge))
                        If Double.IsInfinity(a2(i, j)) Then a2(i, j) = 0.0#
                    ElseIf Not cprops(i).IsIon And cprops(j).IsIon Then
                        a1(i, j) = -1.2
                        a2(i, j) = 0.25
                    ElseIf cprops(i).IsIon And Not cprops(j).IsIon Then
                        a1(i, j) = -1.2
                        a2(i, j) = 0.25
                    ElseIf Not cprops(i).IsIon And Not cprops(j).IsIon Then
                        a1(i, j) = 0
                        a2(i, j) = 0
                    End If
                Next
            Next

            Dim sum1mr(n), sum2mr, sum3mr, sum4mr(n), sum5mr, sum6mr(n), sum7mr, sum8mr(n), sum0 As Double

            For i = 0 To n
                For j = 0 To n
                    Bij(i, j) = b12(i, j) + c12(i, j) * Exp(a1(i, j) * Im ^ 0.5 + a2(i, j) * Im)
                    Bref(i, j) = b12(i, j) + c12(i, j)
                    dBdIm(i, j) = (a1(i, j) / (2 * Im ^ 0.5) + a2(i, j)) * c12(i, j) * Exp(a1(i, j) * Im ^ 0.5 + a2(i, j) * Im)
                Next
            Next

            For i = 0 To n
                sum1mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum1mr(i) += molality(j) * Bij(i, j)
                        End If
                    End If
                Next
            Next

            sum2mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum2mr += solvmfrac(i) * molality(j) * (Bij(i, j) + Im * dBdIm(i, j))
                        End If
                    End If
                Next
            Next

            sum3mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Charge > 0 And cprops(j).Charge < 0 Then
                        sum3mr += molality(i) * molality(j) * (Bij(i, j) + Im * dBdIm(i, j))
                    End If
                Next
            Next

            For i = 0 To n
                sum4mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum4mr(j) += solvmfrac(i) * Bij(i, j)
                        End If
                    End If
                Next
            Next

            sum5mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum5mr += solvmfrac(i) * molality(j) * dBdIm(i, j)
                        End If
                    End If
                Next
            Next


            For i = 0 To n
                sum6mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).IsIon Then
                        If cprops(j).IsIon Then
                            sum6mr(j) += molality(i) * Bij(i, j)
                        End If
                    End If
                Next
            Next

            sum7mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Charge > 0 And cprops(j).Charge < 0 Then
                        sum7mr += molality(i) * molality(j) * dBdIm(i, j)
                    End If
                Next
            Next

            'reference state normalization

            For i = 0 To n
                sum8mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(j).IsIon Then
                        sum8mr(j) += solvmfrac(i) * Bref(i, j)
                    End If
                Next
            Next

            For i = 0 To n
                With cprops(i)
                    If .IsIon Then
                        lngmr(i) = 1 / Msolv * sum4mr(i) + .Charge ^ 2 / (2 * Msolv) * sum5mr + sum6mr(i) + .Charge ^ 2 / 2 * sum7mr - 1 / Msolv * sum8mr(i)
                    ElseIf .Name = "Water" Or .Name = "Methanol" Then
                        lngmr(i) = sum1mr(i) - .Molar_Weight / 1000 / Msolv * sum2mr - Msolv * sum3mr
                    Else
                        lngmr(i) = 0.0#
                    End If
                End With
            Next

            'short range term

            i = 0
            Do
                fi(i) = Vx(i) * VR(i) / r
                teta(i) = Vx(i) * VQ(i) / q
                If fi(i) = 0 Then fi(i) = 1.0E-40
                If teta(i) = 0 Then teta(i) = 1.0E-40
                i = i + 1
            Loop Until i = n + 1

            For i = 0 To n
                For k = 0 To n
                    S(i) += teta(k) * tau_ji(i, k)
                Next
            Next

            For i = 0 To n
                sum1(i) = 0
                For k = 0 To n
                    sum0 = 0
                    For l = 0 To n
                        sum0 += teta(l) * tau_ij(l, k)
                    Next
                    sum1(i) += teta(k) * tau_ij(i, k) / sum0
                Next
            Next

            Dim Rref, Qref, phirefi(n), phiiref(n), sum3sr(n), sum4sr, sum5sr(n) As Double

            Rref = 0
            For i = 0 To n
                Rref += solvmfrac(i) * VR(i)
                Qref += solvmfrac(i) * VQ(i)
            Next

            sum4sr = 0.0#
            For i = 0 To n
                sum3sr(i) = 0.0#
                For j = 0 To n
                    If cprops(j).Name = "Water" Or cprops(j).Name = "Methanol" Then
                        sum3sr(i) += VQ(j) * solvmfrac(j) * tau_ji(i, j)
                        If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                            sum4sr += VQ(j) * solvmfrac(j)
                        End If
                    End If
                Next
            Next

            For i = 0 To n
                phirefi(i) = 0
                For j = 0 To n
                    If cprops(j).Name = "Water" Or cprops(j).Name = "Methanol" Then
                        phirefi(i) = sum3sr(i) / sum4sr
                    End If
                Next
            Next

            For i = 0 To n
                For k = 0 To n
                    sum0 = 0
                    For l = 0 To n
                        sum0 += VQ(l) * solvmfrac(l) * tau_ij(l, k)
                    Next
                    phiiref(i) += VQ(k) * solvmfrac(k) * tau_ij(i, k) / sum0
                Next
            Next

            i = 0
            Do
                With cprops(i)
                    lngc(i) = Log(fi(i) / Vx(i)) + 1 - fi(i) / Vx(i) - 5 * VQ(i) * (Log(fi(i) / teta(i)) + 1 - fi(i) / teta(i))
                    lngr(i) = VQ(i) * (1 - Log(S(i)) - sum1(i))
                    lngsr(i) = lngc(i) + lngr(i)
                    If .IsIon Then
                        'reference state normalization
                        If VR(i) > 0.0 Then lngsr(i) -= Log(VR(i) / Rref) + 1 - VR(i) / Rref - 5 * VQ(i) * (Log(VR(i) * Qref / (Rref * VQ(i))) + 1 - VR(i) * Qref / (Rref * VQ(i)))
                        lngsr(i) -= VQ(i) * (1 - Log(phirefi(i)) - phiiref(i))
                        lngsr(i) += Log(Xsolv)
                    ElseIf .IsSalt Then
                        lngsr(i) = 0.0#
                    End If
                End With
                i = i + 1
            Loop Until i = n + 1

            'final activity coefficients

            For i = 0 To n
                lng(i) = lngsr(i) + lngmr(i) + lnglr(i)
            Next

            'calculate activity of non-dissociated salts

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

            For i = 0 To n
                g(i) = Math.Exp(lng(i))
                If VR(i) = 0 And VQ(i) = 0 Then
                    g(i) = 1.0
                End If
            Next

            Return g

        End Function

    End Class

End Namespace

