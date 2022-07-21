'    Lee-Kesler-Plöcker Property Package 
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

Imports FileHelpers
Imports Cudafy
Imports Cudafy.Translator
Imports Cudafy.Host

Namespace PropertyPackages.Auxiliary


    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> _
    Public Class LKP_IPData

        Implements ICloneable

        Public ID1 As String
        Public ID2 As String
        Public kij As Double = 1.0#

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New LKP_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .kij = Me.kij
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class LeeKeslerPlocker

        Dim m_pr As New PropertyPackages.Auxiliary.PROPS

        Private _ip As Dictionary(Of String, Dictionary(Of String, LKP_IPData))

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, LKP_IPData))
            Get
                Return _ip
            End Get
        End Property

        Sub New()

            _ip = New Dictionary(Of String, Dictionary(Of String, LKP_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim lkpip As LKP_IPData
            Dim lkpipc() As LKP_IPData
            Dim fh1 As New FileHelperEngine(Of LKP_IPData)
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.lkp_ip.dat")
                Using t As New IO.StreamReader(filestr)
                    lkpipc = fh1.ReadStream(t)
                End Using
            End Using

            For Each lkpip In lkpipc
                If Me.InteractionParameters.ContainsKey((lkpip.ID1)) Then
                    If Me.InteractionParameters((lkpip.ID1)).ContainsKey((lkpip.ID2)) Then
                    Else
                        Me.InteractionParameters((lkpip.ID1)).Add((lkpip.ID2), lkpip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add((lkpip.ID1), New Dictionary(Of String, LKP_IPData))
                    Me.InteractionParameters((lkpip.ID1)).Add((lkpip.ID2), lkpip.Clone)
                End If
            Next
            lkpip = Nothing
            lkpipc = Nothing
            fh1 = Nothing

        End Sub

        Function MixCritProp_LK(ByVal Vz As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal Vw As Object, ByVal VVc As Object, ByVal VKij(,) As Double)

            Dim Pcm, Tcm, Vcm, wm As Double
            Dim n As Integer = Vz.Length - 1

            Dim vcjk(n, n), tcjk(n, n) As Double

            Dim i, j As Integer

            For i = 0 To n
                For j = 0 To n
                    If i = j Then
                        VKij(i, j) = 1
                    Else
                        If VKij(i, j) = 0 Then VKij(i, j) = 1
                    End If
                Next
            Next

            wm = 0
            For i = 0 To n
                wm += Vz(i) * Vw(i)
                For j = 0 To n
                    vcjk(i, j) = 1 / 8 * (VVc(i) ^ (1 / 3) + VVc(j) ^ (1 / 3)) ^ 3 / 1000
                    tcjk(i, j) = (VTc(i) * VTc(j)) ^ 0.5 * VKij(i, j)
                Next
            Next

            Vcm = 0
            i = 0
            Do
                j = 0
                Do
                    Vcm += Vz(i) * Vz(j) * vcjk(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Tcm = 0
            i = 0
            Do
                j = 0
                Do
                    Tcm += 1 / Vcm ^ 0.25 * Vz(i) * Vz(j) * vcjk(i, j) ^ 0.25 * tcjk(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Pcm = (0.2905 - 0.085 * wm) * 8.314 * Tcm / Vcm

            Return New Object() {Tcm, Pcm, Vcm, wm}

        End Function

        Function H_LK_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Object, ByVal VKij As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal Hid As Double, ByVal VVc As Object) As Double

            Dim n, R As Double
            Dim Tc(), Pc(), w(), Tr() As Double
            Dim i As Integer

            n = Vz.Length - 1

            ReDim Tc(n), Pc(n), w(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            'mixture critical properties
            Dim Tcm, Pcm, wm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw, VVc, VKij)
            Tcm = obj(0)
            Pcm = obj(1)
            wm = obj(3)

            'Dim DHres = R * Tcm * Me.Hlk(T / Tcm, P / Pcm, wm)
            Dim DHres = R * Tcm * Me.H_LK(TIPO, T / Tcm, P / Pcm, wm)

            If DHres = 0 Then Throw New Exception("Erro no calculo da entalpia [LK].")

            H_LK_MIX = Hid + DHres / MMm '/ 1000

        End Function

        Function H_LK_MIX(ByVal V As Double, ByVal T As Double, ByVal P As Double, ByVal Vz As Object, ByVal VKij As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal Hid As Double, ByVal VVc As Object) As Double

            Dim n, R As Double
            Dim Tc(), Pc(), w(), Tr() As Double
            Dim i As Integer

            n = Vz.Length - 1

            ReDim Tc(n), Pc(n), w(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            'mixture critical properties
            Dim Tcm, Pcm, Vcm, wm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw, VVc, VKij)
            Tcm = obj(0)
            Pcm = obj(1)
            Vcm = obj(2)
            wm = obj(3)

            'Dim DHres = R * Tcm * Me.Hlk(T / Tcm, P / Pcm, wm)
            Dim DHres = R * Tcm * Me.H_LK(V / Vcm, T / Tcm, P / Pcm, wm)

            If DHres = 0 Then Throw New Exception("Erro no calculo da entalpia [LK].")

            H_LK_MIX = Hid + DHres / MMm '/ 1000

        End Function

        Function S_LK_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Array, ByVal VKij As Object, ByVal VTc As Array, ByVal VPc As Array, ByVal Vw As Array, ByVal VMM As Array, ByVal Sid As Double, ByVal VVc As Object) As Double

            Dim n, R As Double
            Dim Tc(), Pc(), w(), Tr() As Double
            Dim i As Integer

            n = Vz.Length - 1

            ReDim Tc(n), Pc(n), w(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            'mixture critical properties
            Dim Tcm, Pcm, wm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw, VVc, VKij)
            Tcm = obj(0)
            Pcm = obj(1)
            wm = obj(3)

            Dim DSres = R * Me.S_LK(TIPO, T / Tcm, P, Pcm, wm)

            S_LK_MIX = Sid + DSres / MMm '/ 1000

        End Function

        Function Z_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double)

            Dim z, zh, zs, wh, Vr As Double
            Dim B, C, D, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr

            Vr = Me.ESTIMAR_Vr2(TIPO, Pr, Tr, B, C, D, c4, beta, gamma)
            zs = Pr * Vr / Tr

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr

            Vr = Me.ESTIMAR_Vr2(TIPO, Pr, Tr, B, C, D, c4, beta, gamma)

            zh = Pr * Vr / Tr

            wh = 0.3978

            z = zs + w / wh * (zh - zs)

            Return New Object() {z, zs, zh}

        End Function

        Function H_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double)

            Dim zs, zh, wh, Vr, z, DHresS, DHresH, DHres As Double
            Dim E, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            Dim tmp = Me.Z_LK(TIPO, Tr, Pr, w)
            z = tmp(0)
            zs = tmp(1)

            Vr = zs * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DHresS = Tr * (zs - 1 - (b2 + 2 * b3 / Tr + 3 * b4 / Tr ^ 2) / (Tr * Vr) - (c2 - 3 * c3 / Tr ^ 2) / (2 * Tr * Vr ^ 2) + d2 / (5 * Tr * Vr ^ 5) + 3 * E)

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            zh = tmp(2)

            Vr = zh * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DHresH = Tr * (zh - 1 - (b2 + 2 * b3 / Tr + 3 * b4 / Tr ^ 2) / (Tr * Vr) - (c2 - 3 * c3 / Tr ^ 2) / (2 * Tr * Vr ^ 2) + d2 / (5 * Tr * Vr ^ 5) + 3 * E)

            DHres = DHresS + w / wh * (DHresH - DHresS)

            Return DHres

        End Function

        Function H_LK(ByVal Vr As Double, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double)

            Dim zs, zh, wh, z, DHresS, DHresH, DHres As Double
            Dim B, C, D, E, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr

            Dim Pr1 = CalcPr(Vr, Tr, B, C, D, c4, beta, gamma)

            zs = Vr * Pr1 / Tr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DHresS = Tr * (zs - 1 - (b2 + 2 * b3 / Tr + 3 * b4 / Tr ^ 2) / (Tr * Vr) - (c2 - 3 * c3 / Tr ^ 2) / (2 * Tr * Vr ^ 2) + d2 / (5 * Tr * Vr ^ 5) + 3 * E)

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            Dim Pr2 = CalcPr(Vr, Tr, B, C, D, c4, beta, gamma)

            zh = Vr * Pr2 / Tr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DHresH = Tr * (zh - 1 - (b2 + 2 * b3 / Tr + 3 * b4 / Tr ^ 2) / (Tr * Vr) - (c2 - 3 * c3 / Tr ^ 2) / (2 * Tr * Vr ^ 2) + d2 / (5 * Tr * Vr ^ 5) + 3 * E)

            DHres = DHresS + w / wh * (DHresH - DHresS)

            Return DHres

        End Function

        Function S_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal P As Double, ByVal Pc As Double, ByVal w As Double)

            Dim zs, zh, wh, Vr, z, DSresS, DSresH, DSres, Pr As Double
            Dim E, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            Pr = P / Pc

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            Dim tmp = Me.Z_LK(TIPO, Tr, Pr, w)
            z = tmp(0)
            zs = tmp(1)

            Vr = zs * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DSresS = Math.Log(zs) - Math.Log(1) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E
            'DSresS = Math.Log(zs) - Math.Log(P / 101325) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            zh = tmp(2)

            Vr = zh * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            'DSresH = Math.Log(zh) - Math.Log(P / 101325) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E
            DSresH = Math.Log(zh) - Math.Log(1) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E

            DSres = DSresS + w / wh * (DSresH - DSresS)

            Return DSres

        End Function

        Function ESTIMAR_Vr(ByVal TIPO, ByVal Pr, ByVal Tr, ByVal B, ByVal C, ByVal D, ByVal c4, ByVal beta, ByVal gamma)

            Dim cnt As Integer = 0
            Dim Vr, Vrant, Vrant2, fi, fi_ant, fi_ant2, dfidVr As Double

            fi_ant2 = 0
            fi_ant = 0
            fi = 0
            If TIPO = "L" Then Vr = 0.05 Else Vr = 1.0# 'Tr / Pr * 0.3
            Do
                fi_ant2 = fi_ant
                fi_ant = fi
                fi = Pr * Vr / Tr - (1 + B / Vr + C / Vr ^ 2 + D / Vr ^ 5 + c4 / Tr ^ 3 / Vr ^ 2 * (beta + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))
                dfidVr = -B * Vr ^ -2 - 2 * C * Vr ^ -3 - 5 * D * Vr ^ -6 + _
                        (-2 * beta * c4 * Vr ^ -3 / Tr ^ 3 * Math.Exp(-gamma * Vr ^ -2) + beta * c4 * Vr ^ -2 / Tr ^ 3 * Math.Exp(-gamma * Vr ^ -2) * (2 * gamma * Vr ^ -3)) + _
                        (-4 * gamma * c4 / Tr ^ 3 * Vr ^ -5 * Math.Exp(-gamma * Vr ^ -2) + gamma * c4 * Tr ^ -3 * Vr ^ -4 * Math.Exp(-gamma * Vr ^ -2) * (2 * gamma * Vr ^ -3)) + _
                        -Pr / Tr
                dfidVr = -dfidVr

                Vrant2 = Vrant
                Vrant = Vr
                Vr = Vr - fi / dfidVr

                If Vr < 0 Then
                    If TIPO = "L" Then Vr = 0.996 * Vrant Else Vr = Tr / Pr * 0.35
                    fi_ant2 = 0
                    fi_ant = 0
                End If
                If Math.Abs(fi - fi_ant2) = 0 Then Vr = Vrant * 0.999
                cnt += 1
            Loop Until Math.Abs(fi) < 0.001 Or cnt >= 1000

            Return Vr

        End Function

        Function ESTIMAR_Vr2(ByVal TIPO As String, ByVal Pr As Double, ByVal Tr As Double, ByVal B As Double, ByVal C As Double, ByVal D As Double, ByVal c4 As Double, ByVal beta As Double, ByVal gamma As Double)

            Dim i As Integer

            Dim Tinf, Tsup, Vr As Double

            Dim fT, fT_inf, nsub, delta_T As Double

            If TIPO = "L" Then
                Tinf = 0
                Tsup = 10
                nsub = 500
            Else
                Tinf = 1001
                Tsup = 0
                nsub = 500
            End If

            delta_T = (Tsup - Tinf) / nsub

            i = 0
            Dim c4_tr_3 = c4 / Tr ^ 3
            Do
                i = i + 1
                Vr = Tinf
                Dim Vr_2 = Vr ^ 2
                fT = Pr * Vr / Tr - (1 + B / Vr + C / Vr_2 + D / Vr ^ 5 + c4_tr_3 / Vr_2 * (beta + gamma / Vr_2) * Math.Exp(-gamma / Vr_2))
                Tinf = Tinf + delta_T
                Vr = Tinf

                Vr_2 = Vr ^ 2
                fT_inf = Pr * Vr / Tr - (1 + B / Vr + C / Vr_2 + D / Vr ^ 5 + c4_tr_3 / Vr_2 * (beta + gamma / Vr_2) * Math.Exp(-gamma / Vr_2))
            Loop Until fT * fT_inf < 0 Or i >= 500
            Tsup = Tinf
            Tinf = Tinf - delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            Dim aaa_2 = aaa ^ 2
            faa = Pr * aaa / Tr - (1 + B / aaa + C / aaa_2 + D / aaa ^ 5 + c4 / Tr ^ 3 / aaa_2 * (beta + gamma / aaa_2) * Math.Exp(-gamma / aaa_2))
            Dim bbb_2 = bbb ^ 2
            fbb = Pr * bbb / Tr - (1 + B / bbb + C / bbb_2 + D / bbb ^ 5 + c4 / Tr ^ 3 / bbb_2 * (beta + gamma / bbb_2) * Math.Exp(-gamma / bbb_2))
            fcc = fbb
            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 0.0000001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                'If Math.Abs(fbb) < 0.1 Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                Dim bbb1_2 = bbb ^ 2
                fbb = Pr * bbb / Tr - (1 + B / bbb + C / bbb1_2 + D / bbb ^ 5 + c4 / Tr ^ 3 / bbb1_2 * (beta + gamma / bbb1_2) * Math.Exp(-gamma / bbb1_2))
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:

            Return bbb

        End Function

        Function CalcPr(Vr As Double, ByVal Tr As Double, ByVal B As Double, ByVal C As Double, ByVal D As Double, ByVal c4 As Double, ByVal beta As Double, ByVal gamma As Double) As Double

            Dim c4_tr_3 = c4 / Tr ^ 3
            Dim Pr = Tr / Vr * (1 + B / Vr + C / Vr + D / Vr ^ 5 + c4_tr_3 / Vr * (beta + gamma / Vr) * Math.Exp(-gamma / Vr))

            Return Pr

        End Function

        Function CPCV_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double)

            Dim zs, zh, wh, Vr, z, DCvS, DCvH, DCpS, DCpH, DCv, DCp, dPdT, dPdV As Double
            Dim E, B, C, D, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            Dim R = 8.314

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            Dim tmp = Me.Z_LK(TIPO, Tr, Pr, w)
            z = tmp(0)
            zs = tmp(1)

            Vr = zs * Tr / Pr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DCvS = 2 * (b3 + 3 * b4 / Tr) / (Tr ^ 2 * Vr) - 3 * c3 / (Tr ^ 3 * Vr ^ 2) - 6 * E
            dPdT = 1 / Vr * (1 + (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr + (c1 - 2 * c3 / Tr ^ 3) / (Vr ^ 2) + d1 / (Vr ^ 5) - 2 * c4 / (Tr ^ 3 * Vr ^ 2) * ((beta + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2)))
            dPdV = -Tr / Vr ^ 2 * (1 + 2 * B / Vr + 3 * C / Vr ^ 2 + 6 * D / Vr ^ 5 + c4 / (Tr ^ 3 * Vr ^ 2) * (3 * beta + (5 - 2 * (beta + gamma / Vr ^ 2)) * gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))
            DCpS = DCvS - 1 - Tr * dPdT ^ 2 / dPdV

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            zh = tmp(2)

            Vr = zh * Tr / Pr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DCvH = 2 * (b3 + 3 * b4 / Tr) / (Tr ^ 2 * Vr) - 3 * c3 / (Tr ^ 3 * Vr ^ 2) - 6 * E
            dPdT = 1 / Vr * (1 + (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr + (c1 - 2 * c3 / Tr ^ 3) / (Vr ^ 2) + d1 / (Vr ^ 5) - 2 * c4 / (Tr ^ 3 * Vr ^ 2) * ((beta + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2)))
            dPdV = -Tr / Vr ^ 2 * (1 + 2 * B / Vr + 3 * C / Vr ^ 2 + 6 * D / Vr ^ 5 + c4 / (Tr ^ 3 * Vr ^ 2) * (3 * beta + (5 - 2 * (beta + gamma / Vr ^ 2)) * gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))
            DCpH = DCvH - 1 - Tr * dPdT ^ 2 / dPdV

            DCv = DCvS + w / wh * (DCvH - DCvS)
            DCp = DCpS + w / wh * (DCpH - DCpS)

            Dim tmp2(1) As Double
            tmp2(0) = DCp
            tmp2(1) = DCv

            Return tmp2

        End Function

        Function CpCvR_LK(ByVal TIPO, ByVal T, ByVal P, ByVal Vz, ByVal VKij, ByVal Vzmass, ByVal VTc, ByVal VPc, ByVal VCpig, ByVal VMM, ByVal Vw, ByVal VZRa, ByVal VVc)

            Dim n, R As Double
            Dim Tc(), Pc(), Vc(), w(), Tr() As Double
            Dim i

            n = Vz.Length - 1

            ReDim Tc(n), Pc(n), Vc(n), w(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            Dim Cpm_ig = 0.0#
            i = 0
            Do
                Cpm_ig += Vzmass(i) * VCpig(i) * MMm
                i += 1
            Loop Until i = n + 1

            'mixture critical properties
            Dim Tcm, Pcm, wm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw, VVc, VKij)
            Tcm = obj(0)
            Pcm = obj(1)
            wm = obj(3)

            Dim lkcp = Me.CPCV_LK(TIPO, T / Tcm, P / Pcm, wm)

            Dim tmp(2) As Double
            tmp(0) = (lkcp(0) * R + Cpm_ig) / (lkcp(1) * R + Cpm_ig - R)
            tmp(1) = (lkcp(0) * R + Cpm_ig) / MMm
            tmp(2) = (lkcp(1) * R + Cpm_ig - R) / MMm

            CpCvR_LK = tmp

        End Function

        Function LnFugM(ByVal TIPO As String, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double) As Double()

            Dim zs, zh, wh, Vr, z, LnFugMS, LnFugMH, LnFugMr As Double
            Dim B, C, D, E, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            Dim tmp = Me.Z_LK(TIPO, Tr, Pr, w)
            z = tmp(0)
            zs = tmp(1)

            Vr = zs * Tr / Pr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            LnFugMS = zs - 1 - Math.Log(zs) + B / Vr + C / (2 * Vr ^ 2) + D / (5 * Vr ^ 5) + E

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            zh = tmp(2)

            Vr = zh * Tr / Pr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            LnFugMH = zh - 1 - Math.Log(zh) + B / Vr + C / (2 * Vr ^ 2) + D / (5 * Vr ^ 5) + E

            LnFugMr = LnFugMS + w / wh * (LnFugMH - LnFugMS)

            Return New Double() {LnFugMr, LnFugMS, LnFugMH}

        End Function

        Function LnFugM(ByVal Vr As Double, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double) As Double()

            Dim zs, zh, wh, z, LnFugMS, LnFugMH, LnFugMr As Double
            Dim B, C, D, E, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr

            Dim Pr1 = CalcPr(Vr, Tr, B, C, D, c4, beta, gamma)

            zs = Vr * Pr1 / Tr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            LnFugMS = zs - 1 - Math.Log(zs) + B / Vr + C / (2 * Vr ^ 2) + D / (5 * Vr ^ 5) + E

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr

            Dim Pr2 = CalcPr(Vr, Tr, B, C, D, c4, beta, gamma)

            zh = Vr * Pr2 / Tr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            LnFugMH = zh - 1 - Math.Log(zh) + B / Vr + C / (2 * Vr ^ 2) + D / (5 * Vr ^ 5) + E

            LnFugMr = LnFugMS + w / wh * (LnFugMH - LnFugMS)

            Return New Double() {LnFugMr, LnFugMS, LnFugMH}

        End Function

        Function CalcLnFug(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Object, ByVal VKij As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal VVc As Object, ByVal Hid As Double) As Object

            If Settings.EnableGPUProcessing Then
                Return CalcLnFugGPU(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, VVc, Hid)
            Else
                Return CalcLnFugCPU(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, VVc, Hid)
            End If

        End Function

        Function CalcLnFugCPU(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Object, ByVal VKij As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal VVc As Object, ByVal Hid As Double) As Object

            'mixture critical properties
            Dim Tcm, Pcm, Vcm, wm, Tr, Pr, zcm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw, VVc, VKij)
            Tcm = obj(0)
            Pcm = obj(1)
            Vcm = obj(2)
            wm = obj(3)
            Tr = T / Tcm
            Pr = P / Pcm
            zcm = Pcm * Vcm / (8.314 * Tcm)

            Dim i, j, l, n As Integer

            n = Vz.Length - 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            Dim dHmRTcm As Double

            dHmRTcm = Me.H_LK_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, 0, VVc) / (8.314 * Tcm) * MMm

            Dim dlnfidwm, dTcmdx(n, n), dVcmdx(n, n), dPcmdx(n, n), dZcmdx(n, n), lnfi(n) As Double
            Dim lnfugm, lnfugs, lnfugh As Double, res As Double()

            res = Me.LnFugM(TIPO, Tr, Pr, wm)
            lnfugm = res(0)
            lnfugs = res(1)
            lnfugh = res(2)

            dlnfidwm = 1 / 0.3978 * (lnfugh - lnfugs)

            Dim vcjk(n, n), tcjk(n, n) As Double

            For i = 0 To n
                For j = 0 To n
                    vcjk(i, j) = 1 / 8 * (VVc(i) ^ (1 / 3) + VVc(j) ^ (1 / 3)) ^ 3 / 1000
                    tcjk(i, j) = (VTc(i) * VTc(j)) ^ 0.5 * VKij(i, j)
                Next
            Next

            Dim sum1(n, n), sum2(n, n) As Double

            For i = 0 To n
                For j = 0 To n
                    sum1(i, j) = 0
                    sum2(i, j) = 0
                    For l = 0 To n
                        sum1(i, j) += Vz(l) * (vcjk(l, j) ^ 0.25 * tcjk(l, j) - vcjk(l, i) ^ 0.25 * tcjk(l, i))
                        sum2(i, j) += Vz(l) * (vcjk(l, j) - vcjk(l, i))
                    Next
                Next
            Next

            For i = 0 To n
                For j = 0 To n
                    dZcmdx(i, j) = -0.085 * (Vw(j) - Vw(i))
                    dVcmdx(i, j) = 2 * sum2(i, j)
                    dTcmdx(i, j) = (2 * sum1(i, j) - 0.25 * Vcm ^ (0.25 - 1) * dVcmdx(i, j) * Tcm) / Vcm ^ 0.25
                    dPcmdx(i, j) = Pcm * (dZcmdx(i, j) / zcm + dTcmdx(i, j) / Tcm - dVcmdx(i, j) / Vcm)
                Next
            Next

            Dim suma(n), sumb(n), sumc(n), zm As Double

            zm = Me.Z_LK(TIPO, Tr, Pr, wm)(0)

            For i = 0 To n
                suma(i) = 0
                sumb(i) = 0
                sumc(i) = 0
                For j = 0 To n
                    If j <> i Then
                        suma(i) += Vz(j) * dTcmdx(i, j)
                        sumb(i) += Vz(j) * dPcmdx(i, j)
                        sumc(i) += Vz(j) * (Vw(j) - Vw(i))
                    End If
                Next
            Next

            For i = 0 To n
                lnfi(i) = lnfugm - 1 / T * dHmRTcm * suma(i) + (zm - 1) / Pcm * sumb(i) - dlnfidwm * sumc(i)
            Next

            Return lnfi

        End Function

        Function CalcLnFugTV(T As Double, P As Double, V As Double, Vz As Double(), VKij As Double(,), VTc As Double(), VPc As Double(), Vw As Double(), VMM As Double(), VVc As Double(), Hid As Double) As Double()

            'mixture critical properties
            Dim Tcm, Pcm, Vcm, wm, Tr, Pr, Vr, zcm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw, VVc, VKij)
            Tcm = obj(0)
            Pcm = obj(1)
            Vcm = obj(2)
            wm = obj(3)
            Tr = T / Tcm
            Pr = P / Pcm
            Vr = V / Vcm
            zcm = Pcm * Vcm / (8.314 * Tcm)

            Dim i, j, l, n As Integer

            n = Vz.Length - 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            Dim dHmRTcm As Double

            dHmRTcm = Me.H_LK_MIX(V, T, P, Vz, VKij, VTc, VPc, Vw, VMM, 0, VVc) / (8.314 * Tcm) * MMm

            Dim dlnfidwm, dTcmdx(n, n), dVcmdx(n, n), dPcmdx(n, n), dZcmdx(n, n), lnfi(n) As Double
            Dim lnfugm, lnfugs, lnfugh As Double, res As Double()

            res = Me.LnFugM(Vr, Tr, Pr, wm)
            lnfugm = res(0)
            lnfugs = res(1)
            lnfugh = res(2)

            dlnfidwm = 1 / 0.3978 * (lnfugh - lnfugs)

            Dim vcjk(n, n), tcjk(n, n) As Double

            For i = 0 To n
                For j = 0 To n
                    vcjk(i, j) = 1 / 8 * (VVc(i) ^ (1 / 3) + VVc(j) ^ (1 / 3)) ^ 3 / 1000
                    tcjk(i, j) = (VTc(i) * VTc(j)) ^ 0.5 * VKij(i, j)
                Next
            Next

            Dim sum1(n, n), sum2(n, n) As Double

            For i = 0 To n
                For j = 0 To n
                    sum1(i, j) = 0
                    sum2(i, j) = 0
                    For l = 0 To n
                        sum1(i, j) += Vz(l) * (vcjk(l, j) ^ 0.25 * tcjk(l, j) - vcjk(l, i) ^ 0.25 * tcjk(l, i))
                        sum2(i, j) += Vz(l) * (vcjk(l, j) - vcjk(l, i))
                    Next
                Next
            Next

            For i = 0 To n
                For j = 0 To n
                    dZcmdx(i, j) = -0.085 * (Vw(j) - Vw(i))
                    dVcmdx(i, j) = 2 * sum2(i, j)
                    dTcmdx(i, j) = (2 * sum1(i, j) - 0.25 * Vcm ^ (0.25 - 1) * dVcmdx(i, j) * Tcm) / Vcm ^ 0.25
                    dPcmdx(i, j) = Pcm * (dZcmdx(i, j) / zcm + dTcmdx(i, j) / Tcm - dVcmdx(i, j) / Vcm)
                Next
            Next

            Dim suma(n), sumb(n), sumc(n), zm As Double

            zm = (P * V) / (8.314 * T)

            For i = 0 To n
                suma(i) = 0
                sumb(i) = 0
                sumc(i) = 0
                For j = 0 To n
                    If j <> i Then
                        suma(i) += Vz(j) * dTcmdx(i, j)
                        sumb(i) += Vz(j) * dPcmdx(i, j)
                        sumc(i) += Vz(j) * (Vw(j) - Vw(i))
                    End If
                Next
            Next

            For i = 0 To n
                lnfi(i) = lnfugm - 1 / T * dHmRTcm * suma(i) + (zm - 1) / Pcm * sumb(i) - dlnfidwm * sumc(i)
            Next

            Return lnfi

        End Function

        Private Function CalcLnFugGPU(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Object, ByVal VKij As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal VVc As Object, ByVal Hid As Double) As Object

            'mixture critical properties
            Dim Tcm, Pcm, Vcm, wm, Tr, Pr, zcm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw, VVc, VKij)
            Tcm = obj(0)
            Pcm = obj(1)
            Vcm = obj(2)
            wm = obj(3)
            Tr = T / Tcm
            Pr = P / Pcm
            zcm = Pcm * Vcm / (8.314 * Tcm)

            Dim i, n As Integer

            n = Vz.Length - 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            Dim dHmRTcm As Double

            dHmRTcm = Me.H_LK_MIX(TIPO, T, P, Vz, VKij, VTc, VPc, Vw, VMM, 0, VVc) / (8.314 * Tcm) * MMm

            Dim lnfugm, lnfugs, lnfugh As Double, res As Double()

            res = Me.LnFugM(TIPO, Tr, Pr, wm)
            lnfugm = res(0)
            lnfugs = res(1)
            lnfugh = res(2)

            Dim dlnfidwm, lnfi(n) As Double

            dlnfidwm = 1 / 0.3978 * (lnfugh - lnfugs)

            Dim suma(n), sumb(n), sumc(n) As Double

            lkp_gpu_func(n, VVc, VTc, VKij, Vz, Vw, Tcm, Pcm, Vcm, zcm, suma, sumb, sumc)

            Dim zm = Me.Z_LK(TIPO, Tr, Pr, wm)(0)

            For i = 0 To n
                lnfi(i) = lnfugm - 1 / T * dHmRTcm * suma(i) + (zm - 1) / Pcm * sumb(i) - dlnfidwm * sumc(i)
            Next

            Return lnfi

        End Function

        Private Sub lkp_gpu_func(n As Integer, VVc As Double(), VTc As Double(), VKij As Double(,), Vz As Double(), Vw As Double(), Tcm As Double, Pcm As Double, Vcm As Double, zcm As Double, suma As Double(), sumb As Double(), sumc As Double())

            Dim dTcmdx(n, n), dVcmdx(n, n), dPcmdx(n, n), dZcmdx(n, n) As Double
            Dim vcjk(n, n), tcjk(n, n) As Double
            Dim sum1(n, n), sum2(n, n) As Double

            Dim gpu As GPGPU = Settings.gpu

            If gpu.IsMultithreadingEnabled Then gpu.SetCurrentContext()

            ' allocate the memory on the GPU
            Dim dev_vcjk As Double(,) = gpu.Allocate(Of Double)(vcjk)
            Dim dev_tcjk As Double(,) = gpu.Allocate(Of Double)(tcjk)
            Dim dev_vvc As Double() = gpu.Allocate(Of Double)(VVc)
            Dim dev_vtc As Double() = gpu.Allocate(Of Double)(VTc)
            Dim dev_vkij As Double(,) = gpu.Allocate(Of Double)(VKij)
            Dim dev_sum1 As Double(,) = gpu.Allocate(Of Double)(sum1)
            Dim dev_sum2 As Double(,) = gpu.Allocate(Of Double)(sum2)
            Dim dev_vz As Double() = gpu.Allocate(Of Double)(Vz)
            Dim dev_vw As Double() = gpu.Allocate(Of Double)(Vw)
            Dim dev_dtcmdx As Double(,) = gpu.Allocate(Of Double)(dTcmdx)
            Dim dev_dvcmdx As Double(,) = gpu.Allocate(Of Double)(dVcmdx)
            Dim dev_dpcmdx As Double(,) = gpu.Allocate(Of Double)(dPcmdx)
            Dim dev_dzcmdx As Double(,) = gpu.Allocate(Of Double)(dZcmdx)
            Dim dev_suma As Double() = gpu.Allocate(Of Double)(suma)
            Dim dev_sumb As Double() = gpu.Allocate(Of Double)(sumb)
            Dim dev_sumc As Double() = gpu.Allocate(Of Double)(sumc)

            ' copy the arrays to the GPU
            gpu.CopyToDevice(vcjk, dev_vcjk)
            gpu.CopyToDevice(tcjk, dev_tcjk)
            gpu.CopyToDevice(VVc, dev_vvc)
            gpu.CopyToDevice(VTc, dev_vtc)
            gpu.CopyToDevice(VKij, dev_vkij)
            gpu.CopyToDevice(sum1, dev_sum1)
            gpu.CopyToDevice(sum2, dev_sum2)
            gpu.CopyToDevice(Vz, dev_vz)
            gpu.CopyToDevice(Vw, dev_vw)
            gpu.CopyToDevice(dTcmdx, dev_dtcmdx)
            gpu.CopyToDevice(dVcmdx, dev_dvcmdx)
            gpu.CopyToDevice(dPcmdx, dev_dpcmdx)
            gpu.CopyToDevice(dZcmdx, dev_dzcmdx)
            gpu.CopyToDevice(suma, dev_suma)
            gpu.CopyToDevice(sumb, dev_sumb)
            gpu.CopyToDevice(sumc, dev_sumc)

            ' launch subs
            gpu.Launch(New dim3(n + 1, n + 1), 1).lkp_gpu_sum1(dev_vcjk, dev_tcjk, dev_vvc, dev_vtc, dev_vkij)
            gpu.Launch(New dim3(n + 1, n + 1), 1).lkp_gpu_sum2(dev_sum1, dev_sum2, dev_vz, dev_vcjk, dev_tcjk)
            gpu.Launch(New dim3(n + 1, n + 1), 1).lkp_gpu_sum3(dev_sum1, dev_sum2, dev_dzcmdx, dev_dvcmdx, dev_dtcmdx, dev_dpcmdx, dev_vw, Tcm, Pcm, Vcm, zcm)
            gpu.Launch(n + 1, 1).lkp_gpu_sum4(dev_suma, dev_sumb, dev_sumc, dev_dtcmdx, dev_dpcmdx, dev_vz, dev_vw)

            ' copy the arrays back from the GPU to the CPU
            gpu.CopyFromDevice(dev_suma, suma)
            gpu.CopyFromDevice(dev_sumb, sumb)
            gpu.CopyFromDevice(dev_sumc, sumc)
            gpu.CopyFromDevice(dev_vcjk, vcjk)
            gpu.CopyFromDevice(dev_tcjk, tcjk)
            gpu.CopyFromDevice(dev_sum1, sum1)
            gpu.CopyFromDevice(dev_sum2, sum2)
            gpu.CopyFromDevice(dev_dtcmdx, dTcmdx)
            gpu.CopyFromDevice(dev_dvcmdx, dVcmdx)
            gpu.CopyFromDevice(dev_dpcmdx, dPcmdx)
            gpu.CopyFromDevice(dev_dzcmdx, dZcmdx)

            ' free the memory allocated on the GPU
            gpu.Free(dev_suma)
            gpu.Free(dev_sumb)
            gpu.Free(dev_sumc)
            gpu.Free(dev_vcjk)
            gpu.Free(dev_tcjk)
            gpu.Free(dev_sum1)
            gpu.Free(dev_sum2)
            gpu.Free(dev_dtcmdx)
            gpu.Free(dev_dvcmdx)
            gpu.Free(dev_dpcmdx)
            gpu.Free(dev_dzcmdx)

            'If gpu.IsMultithreadingEnabled Then gpu.Unlock()

        End Sub

        <Cudafy.Cudafy()> Private Shared Sub lkp_gpu_sum1(thread As Cudafy.GThread, vcjk As Double(,), tcjk As Double(,), VVc As Double(), VTc As Double(), VKij As Double(,))

            Dim i As Integer = thread.blockIdx.x
            Dim j As Integer = thread.blockIdx.y

            vcjk(i, j) = 1 / 8 * (VVc(i) ^ (1 / 3) + VVc(j) ^ (1 / 3)) ^ 3 / 1000
            tcjk(i, j) = (VTc(i) * VTc(j)) ^ 0.5 * VKij(i, j)

        End Sub

        <Cudafy.Cudafy()> Private Shared Sub lkp_gpu_sum2(thread As Cudafy.GThread, sum1 As Double(,), sum2 As Double(,), Vz As Double(), vcjk As Double(,), tcjk As Double(,))

            Dim i As Integer = thread.blockIdx.x
            Dim j As Integer = thread.blockIdx.y

            For l As Integer = 0 To Vz.Length - 1
                sum1(i, j) += Vz(l) * (vcjk(l, j) ^ 0.25 * tcjk(l, j) - vcjk(l, i) ^ 0.25 * tcjk(l, i))
                sum2(i, j) += Vz(l) * (vcjk(l, j) - vcjk(l, i))
            Next

        End Sub

        <Cudafy.Cudafy()> Private Shared Sub lkp_gpu_sum3(thread As Cudafy.GThread, sum1 As Double(,), sum2 As Double(,), dZcmdx As Double(,), dVcmdx As Double(,), dTcmdx As Double(,), dPcmdx As Double(,), Vw As Double(), Tcm As Double, Pcm As Double, Vcm As Double, zcm As Double)

            Dim i As Integer = thread.blockIdx.x
            Dim j As Integer = thread.blockIdx.y

            dZcmdx(i, j) = -0.085 * (Vw(j) - Vw(i))
            dVcmdx(i, j) = 2 * sum2(i, j)
            dTcmdx(i, j) = (2 * sum1(i, j) - 0.25 * Vcm ^ (0.25 - 1) * dVcmdx(i, j) * Tcm) / Vcm ^ 0.25
            dPcmdx(i, j) = Pcm * (dZcmdx(i, j) / zcm + dTcmdx(i, j) / Tcm - dVcmdx(i, j) / Vcm)

        End Sub

        <Cudafy.Cudafy()> Private Shared Sub lkp_gpu_sum4(thread As Cudafy.GThread, suma As Double(), sumb As Double(), sumc As Double(), dTcmdx As Double(,), dPcmdx As Double(,), Vz As Double(), Vw As Double())

            Dim i As Integer = thread.blockIdx.x
            Dim j As Integer = 0

            For j = 0 To Vz.Length - 1
                If j <> i Then
                    suma(i) += Vz(j) * dTcmdx(i, j)
                    sumb(i) += Vz(j) * dPcmdx(i, j)
                    sumc(i) += Vz(j) * (Vw(j) - Vw(i))
                End If
            Next

        End Sub

    End Class

End Namespace

