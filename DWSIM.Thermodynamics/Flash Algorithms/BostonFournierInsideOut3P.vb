'    Boston-Fournier Inside-Out Three-Phase Flash Algorithm
'    Copyright 2011 Daniel Wagner O. de Medeiros
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

Imports System.Math

Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.Common

Imports System.Linq

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class BostonFournierInsideOut3P

        Inherits FlashAlgorithm

        Dim n, ecount As Integer
        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Vn(n) As String
        Dim Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), ui2(n), uic1(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n), Vt(n), Vpc(n), VTc(n), Vw(n) As Double
        Dim L, L1, L2, beta, Lf, V, Vf, R, Rant, Rt, S, Sant, Tant, Pant, T, T_, Tf, P, P_, Pf, T0, P0, A, B, C, D, E, F, Ac, Bc, Cc, Dc, Ec, Fc As Double
        Dim Kb, Kb0, Kb_ As Double
        Dim DHv, DHl, DHv1, DHv2, DHl1, DHl2, Hv0, Hvid, Hlid, Hf, DHlsp, DHvsp As Double
        Dim DSv, DSl, DSv1, DSv2, DSl1, DSl2, Sv0, Svid, Slid, Sf, DSlsp, DSvsp As Double
        Dim Pb, Pd, Pmin, Pmax, Px, soma_x1, soma_x2, soma_y, Tmin, Tmax As Double
        Dim proppack As PropertyPackages.PropertyPackage
        Dim tmpdx, refx, currx As Object
        Sub New()
            MyBase.New()
            Order = 8
        End Sub


        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Algoritmo Flash baseado no procedimento Inside-Out de Boston e Fournier. Ref: http://www.tandfonline.com/doi/abs/10.1080/00986448108912588?journalCode=gcec20"
                Else
                    Return "Based on the Inside-Out procedure by Boston and Fournier. Ref: http://www.tandfonline.com/doi/abs/10.1080/00986448108912588?journalCode=gcec20"
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Inside-Out VLLE (Boston-Fournier)"
            End Get
        End Property

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Inside_Out_VLLE
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz() As Double, ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, k As Integer

            Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
            Dim nc As Integer = Vz.Length - 1

            d1 = Date.Now

            ' try a two-phase flash first.

            Dim _io As New BostonBrittInsideOut With {.FlashSettings = FlashSettings}

            Dim result As Object = _io.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)

            Dim GoneThrough As Boolean = False

            If L = 0 And (FlashSettings(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability)) Then

                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    Dim ioflash As New BostonBrittInsideOut With {.FlashSettings = FlashSettings}

                    Dim m As Double = UBound(stresult(1), 1)

                    Dim trialcomps As New List(Of Double())
                    Dim results As New List(Of Object)

                    For j = 0 To m
                        Dim vxtrial(n) As Double
                        For i = 0 To n
                            vxtrial(i) = stresult(1)(j, i)
                        Next
                        trialcomps.Add(vxtrial)
                    Next

                    For Each tcomp In trialcomps
                        Try
                            Dim r2 = ioflash.Flash_PT(Vz, P, T, PP, True, Vy.DivideY(tcomp))
                            results.Add(r2)
                        Catch ex As Exception
                        End Try
                    Next

                    If results.Where(Function(r) r(0) > 0.0).Count > 0 Then

                        Dim validresult = results.Where(Function(r) r(0) > 0.0).First

                        result = validresult

                        GoneThrough = True

                    End If

                End If

            End If

            ' check if there is a liquid phase

            If result(0) > 0 And Not GoneThrough Then ' we have a liquid phase

                If result(1) > 0.0001 And nc = 1 Then
                    'the liquid phase cannot be unstable when there's also vapor and only two compounds in the system.
                    Return result
                End If

                If nt = -1 Then nt = nc

                Dim Vtrials(nt, nc) As Double
                Dim idx(nt) As Integer

                For i = 0 To nt
                    If Me.StabSearchCompIDs.Length = 0 Then
                        idx(i) = i
                    Else
                        j = 0
                        For Each subst As Interfaces.ICompound In PP.CurrentMaterialStream.Phases(0).Compounds.Values
                            If subst.Name = Me.StabSearchCompIDs(i) Then
                                idx(i) = j
                                Exit For
                            End If
                            j += 1
                        Next
                    End If
                Next

                For i = 0 To nt
                    For j = 0 To nc
                        Vtrials(i, j) = 0.00001
                    Next
                Next
                For j = 0 To nt
                    Vtrials(j, idx(j)) = 1
                Next

                ' do a stability test in the liquid phase

                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    ' liquid phase NOT stable. proceed to three-phase flash.

                    Dim vx2est(nc), fcl(nc), fcv(nc) As Double
                    Dim m As Double = LBound(stresult(1), 1)
                    Dim gl, gli As Double

                    If StabSearchSeverity = 2 Then
                        gli = 0
                        For j = 0 To m
                            For i = 0 To nc
                                vx2est(i) = stresult(1)(j, i)
                            Next
                            fcl = PP.DW_CalcFugCoeff(vx2est, T, P, State.Liquid)
                            gl = 0.0#
                            For i = 0 To nc
                                If vx2est(i) <> 0.0# Then gl += vx2est(i) * Log(fcl(i) * vx2est(i))
                            Next
                            If gl <= gli Then
                                gli = gl
                                k = j
                            End If
                        Next
                        For i = 0 To Vz.Length - 1
                            vx2est(i) = stresult(1)(k, i)
                        Next
                    Else
                        For i = 0 To Vz.Length - 1
                            vx2est(i) = stresult(1)(m, i)
                        Next
                    End If


                    Dim vx1e(Vz.Length - 1), vx2e(Vz.Length - 1) As Double

                    Dim maxl As Double = MathEx.Common.Max(vx2est)
                    Dim imaxl As Integer = Array.IndexOf(vx2est, maxl)

                    F = 1
                    V = result(1)
                    L2 = F * result(3)(imaxl)
                    L1 = F - L2 - V

                    If L1 < 0.0# Then
                        L1 = Abs(L1)
                        L2 = F - L1 - V
                    End If

                    If L2 < 0.0# Then
                        V += L2
                        L2 = Abs(L2)
                    End If

                    For i = 0 To nc
                        If i <> imaxl Then
                            vx1e(i) = Vz(i) - V * result(3)(i) - L2 * vx2est(i)
                        Else
                            vx1e(i) = Vz(i) * L2
                        End If
                    Next

                    Dim sumvx2 = 0.0#
                    For i = 0 To nc
                        sumvx2 += Abs(vx1e(i))
                    Next

                    For i = 0 To nc
                        vx1e(i) = Abs(vx1e(i)) / sumvx2
                    Next

                    result = Flash_PT_3P(Vz, V, L1, L2, result(3), vx1e, vx2est, P, T, PP)

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [IO3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Overrides Function Flash_PH(ByVal Vz() As Double, ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, k As Integer

            d1 = Date.Now

            Dim _io As New BostonBrittInsideOut

            Dim result As Object = _io.Flash_PH(Vz, P, H, Tref, PP, ReuseKI, PrevKi)

            If result(0) > 0 Then

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = Vz.Length - 1

                If nt = -1 Then nt = nc

                Dim Vtrials(nt, nc) As Double
                Dim idx(nt) As Integer

                For i = 0 To nt
                    If Me.StabSearchCompIDs.Length = 0 Then
                        idx(i) = i
                    Else
                        j = 0
                        For Each subst As Interfaces.ICompound In PP.CurrentMaterialStream.Phases(0).Compounds.Values
                            If subst.Name = Me.StabSearchCompIDs(i) Then
                                idx(i) = j
                                Exit For
                            End If
                            j += 1
                        Next
                    End If
                Next

                For i = 0 To nt
                    For j = 0 To nc
                        Vtrials(i, j) = 0.00001
                    Next
                Next
                For j = 0 To nt
                    Vtrials(j, idx(j)) = 1
                Next

                Dim stresult As Object = StabTest(result(4), P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    Dim vx2est(nc), fcl(nc), fcv(nc) As Double
                    Dim m As Double = UBound(stresult(1), 1)
                    Dim gl, gli As Double

                    If StabSearchSeverity = 2 Then
                        gli = 0
                        For j = 0 To m
                            For i = 0 To nc
                                vx2est(i) = stresult(1)(j, i)
                            Next
                            fcl = PP.DW_CalcFugCoeff(vx2est, result(4), P, State.Liquid)
                            gl = 0.0#
                            For i = 0 To nc
                                If vx2est(i) <> 0.0# Then gl += vx2est(i) * Log(fcl(i) * vx2est(i))
                            Next
                            If gl <= gli Then
                                gli = gl
                                k = j
                            End If
                        Next
                        For i = 0 To nc
                            vx2est(i) = stresult(1)(k, i)
                        Next
                    Else
                        For i = 0 To nc
                            vx2est(i) = stresult(1)(m, i)
                        Next
                    End If


                    Dim vx1e(Vz.Length - 1), vx2e(Vz.Length - 1) As Double

                    Dim maxl As Double = MathEx.Common.Max(vx2est)
                    Dim imaxl As Integer = Array.IndexOf(vx2est, maxl)

                    F = 1
                    V = result(1)
                    L1 = (F * Vz(imaxl) - result(3)(imaxl) - F * vx2est(imaxl) + V * vx2est(imaxl)) / (result(2)(imaxl) - vx2est(imaxl))
                    L1 = L1 * (1 - result(2)(imaxl))
                    L2 = F - L1 - V

                    If L2 < 0.0# Then
                        L2 = Abs(L2)
                        L1 = F - L2 - V
                    End If

                    For i = 0 To nc
                        vx1e(i) = (result(2)(i) * L1 - vx2est(i) * L2) / (L1 - L2)
                    Next

                    Dim sumvx2 = 0.0#
                    For i = 0 To nc
                        sumvx2 += Abs(vx1e(i))
                    Next

                    For i = 0 To nc
                        vx1e(i) = Abs(vx1e(i)) / sumvx2
                    Next

                    result = Flash_PH_3P(Vz, result(1), L1, L2, result(3), vx1e, vx2est, P, H, result(4), PP)

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [IO3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Overrides Function Flash_PS(ByVal Vz() As Double, ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i As Integer

            d1 = Date.Now

            Dim _io As New BostonBrittInsideOut

            Dim result As Object = _io.Flash_PS(Vz, P, S, Tref, PP, ReuseKI, PrevKi)

            If result(0) > 0 Then

                Dim stresult As Object = StabTest(result(4), P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    Dim vx2est(n) As Double

                    For i = 0 To n
                        vx2est(i) = stresult(1)(0, i)
                    Next

                    Dim vx1e(Vz.Length - 1), vx2e(Vz.Length - 1) As Double

                    Dim maxl As Double = MathEx.Common.Max(vx2est)
                    Dim imaxl As Integer = Array.IndexOf(vx2est, maxl)

                    F = 1
                    V = result(1)
                    L1 = (F * Vz(imaxl) - result(3)(imaxl) - F * vx2est(imaxl) + V * vx2est(imaxl)) / (result(2)(imaxl) - vx2est(imaxl))
                    L1 = L1 * (1 - result(2)(imaxl))
                    L2 = F - L1 - V

                    If L2 < 0 Then
                        L2 = Abs(L2)
                        L1 = F - L2 - V
                    End If

                    For i = 0 To n
                        vx1e(i) = (result(2)(i) * L1 - vx2est(i) * L2) / (L1 - L2)
                    Next

                    Dim sumvx2 = 0.0#
                    For i = 0 To n
                        sumvx2 += Abs(vx1e(i))
                    Next

                    For i = 0 To n
                        vx1e(i) = Abs(vx1e(i)) / sumvx2
                    Next

                    result = Flash_PS_3P(Vz, result(1), L1, L2, result(3), vx1e, vx2est, P, S, result(4), PP)

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [IO3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Overrides Function Flash_TV(ByVal Vz() As Double, ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j As Integer

            d1 = Date.Now

            Dim _io As New BostonBrittInsideOut

            Dim result As Object = _io.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)

            P = result(4)

            If result(0) > 0 Then

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = Vz.Length - 1

                If nt = -1 Then nt = nc

                Dim Vtrials(nt, nc) As Double
                Dim idx(nt) As Integer

                For i = 0 To nt
                    If Me.StabSearchCompIDs.Length = 0 Then
                        idx(i) = i
                    Else
                        j = 0
                        For Each subst As Interfaces.ICompound In PP.CurrentMaterialStream.Phases(0).Compounds.Values
                            If subst.Name = Me.StabSearchCompIDs(i) Then
                                idx(i) = j
                                Exit For
                            End If
                            j += 1
                        Next
                    End If
                Next

                For i = 0 To nt
                    For j = 0 To nc
                        Vtrials(i, j) = 0.00001
                    Next
                Next
                For j = 0 To nt
                    Vtrials(j, idx(j)) = 1
                Next

                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    Dim vx2est(nc) As Double

                    For i = 0 To nc
                        vx2est(i) = stresult(1)(0, i)
                    Next

                    'do a simple LLE calculation to get initial estimates.
                    Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = result(2), .InitialEstimatesForPhase2 = vx2est, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                    Dim resultL As Object = slle.Flash_PT(Vz, P, T * 0.9, PP)

                    L1 = resultL(0)
                    L2 = resultL(5)
                    Vx1 = resultL(2)
                    Vx2 = resultL(6)

                    result = Flash_TV_3P(Vz, result(1), result(0) * L1, result(0) * L2, result(3), Vx1, Vx2, T, V, result(4), PP)

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("TV Flash [IO3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Overrides Function Flash_PV(ByVal Vz() As Double, ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j As Integer

            d1 = Date.Now

            Dim _io As New BostonBrittInsideOut

            Dim result As Object = _io.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)

            T = result(4)

            If result(0) > 0 Then

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = Vz.Length - 1

                If nt = -1 Then nt = nc

                Dim Vtrials(nt, nc) As Double
                Dim idx(nt) As Integer

                For i = 0 To nt
                    If Me.StabSearchCompIDs.Length = 0 Then
                        idx(i) = i
                    Else
                        j = 0
                        For Each subst As Interfaces.ICompound In PP.CurrentMaterialStream.Phases(0).Compounds.Values
                            If subst.Name = Me.StabSearchCompIDs(i) Then
                                idx(i) = j
                                Exit For
                            End If
                            j += 1
                        Next
                    End If
                Next

                For i = 0 To nt
                    For j = 0 To nc
                        Vtrials(i, j) = 0.00001
                    Next
                Next
                For j = 0 To nt
                    Vtrials(j, idx(j)) = 1
                Next

                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    Dim vx2est(nc) As Double
                    For i = 0 To n
                        vx2est(i) = stresult(1)(0, i)
                    Next

                    'do a simple LLE calculation to get initial estimates.
                    Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = result(2), .InitialEstimatesForPhase2 = vx2est, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                    Dim resultL As Object = slle.Flash_PT(Vz, P, T * 0.9, PP)

                    L1 = resultL(0)
                    L2 = resultL(5)
                    Vx1 = resultL(2)
                    Vx2 = resultL(6)

                    result = Flash_PV_3P(Vz, result(1), result(0) * L1, result(0) * L2, result(3), Vx1, Vx2, P, V, T, PP)

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PV Flash [IO3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Private Function CalcKbj1(ByVal K() As Double) As Double

            Dim i As Integer
            Dim n As Integer = UBound(K) - 1

            Dim Kbj1 As Object

            Kbj1 = K(0)
            For i = 1 To n
                If Abs(K(i) - 1) < Abs(Kbj1 - 1) Then Kbj1 = K(i)
            Next

            Return Kbj1

        End Function

        Private Function CalcKbjw(ByVal K1() As Double, ByVal K2() As Double, ByVal L1 As Double, ByVal L2 As Double, Vx1() As Double, Vx2() As Double) As Double

            Dim i As Integer
            Dim n As Integer = UBound(K1) - 1

            Dim Kbj1 As Object

            Kbj1 = Vx1(0) * L1 * K1(0) + Vx2(0) * L2 * K2(0)
            For i = 1 To n
                If Abs(K1(i) - 1) < Abs(Kbj1 - 1) Then Kbj1 = Vx1(i) * L1 * K1(i) + Vx2(i) * L2 * K2(i)
            Next

            Return Kbj1

        End Function

        Private Function EnergyBalance(ByVal R As Double) As Double

            Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0
            Dim i As Integer

            S = 1 - R
            Do
                S0 = S
                S1 = S + 0.0001
                fr = Me.SErrorFunc(S0, R)
                dfr = (fr - Me.SErrorFunc(S1, R)) / -0.0001
                S += -0.3 * fr / dfr
                icount += 1
            Loop Until Abs(fr) < itol Or icount > maxit_i

            If S <= -(1 - R) Then S = -(1 - R)
            If S >= (1 - R) Then S = (1 - R)

            For i = 0 To n
                pi(i) = fi(i) / (R + (1 - R + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - R - S) / (2 * Kb0 * Exp(ui2(i))))
            Next

            Dim sumpi As Double = 0
            Dim sumeuipi1 As Double = 0
            Dim sumeuipi2 As Double = 0
            For i = 0 To n
                sumpi += pi(i)
                sumeuipi1 += pi(i) / Exp(ui1(i))
                sumeuipi2 += pi(i) / Exp(ui2(i))
            Next
            For i = 0 To n
                Vx1(i) = (pi(i) / Exp(ui1(i))) / sumeuipi1
                Vx2(i) = (pi(i) / Exp(ui2(i))) / sumeuipi2
                Vy(i) = pi(i) / sumpi
            Next

            If R <> 1.0# Then
                Kb = ((1 - R + S) * sumeuipi1 + (1 - R - S) * sumeuipi2) / (2 * (1 - R) * sumpi)
            Else
                Kb = 1.0#
            End If

            V = R * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)
            L = L1 + L2

            Dim eberror As Double

            T = 1 / T_ + (Log(Kb) - A) / B
            T = 1 / T

            'If T < Tmin Then T = Tmin
            'If T > Tmax Then T = Tmax

            Dim Hv, Hl1, Hl2 As Double

            Hv = proppack.DW_CalcEnthalpy(Vy, T, Pf, State.Vapor) * proppack.AUX_MMM(Vy)
            Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx1)
            Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx2)
            eberror = Hf - V * Hv - L1 * Hl1 - L2 * Hl2

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return eberror

        End Function

        Private Function EnergyBalanceAbs(ByVal R As Double) As Double

            Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0
            Dim i As Integer

            S = 1 - R
            Do
                S0 = S
                S1 = S + 0.0001
                fr = Me.SErrorFunc(S0, R)
                dfr = (fr - Me.SErrorFunc(S1, R)) / -0.0001
                S += -0.3 * fr / dfr
                icount += 1
            Loop Until Abs(fr) < itol Or icount > maxit_i

            If S <= -(1 - R) Then S = -(1 - R)
            If S >= (1 - R) Then S = (1 - R)

            For i = 0 To n
                pi(i) = fi(i) / (R + (1 - R + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - R - S) / (2 * Kb0 * Exp(ui2(i))))
            Next

            Dim sumpi As Double = 0
            Dim sumeuipi1 As Double = 0
            Dim sumeuipi2 As Double = 0
            For i = 0 To n
                sumpi += pi(i)
                sumeuipi1 += pi(i) / Exp(ui1(i))
                sumeuipi2 += pi(i) / Exp(ui2(i))
            Next
            For i = 0 To n
                Vx1(i) = (pi(i) / Exp(ui1(i))) / sumeuipi1
                Vx2(i) = (pi(i) / Exp(ui2(i))) / sumeuipi2
                Vy(i) = pi(i) / sumpi
            Next

            If R <> 1.0# Then
                Kb = ((1 - R + S) * sumeuipi1 + (1 - R - S) * sumeuipi2) / (2 * (1 - R) * sumpi)
            Else
                Kb = 1.0#
            End If

            V = R * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)
            L = L1 + L2

            Dim eberror As Double

            T = 1 / T_ + (Log(Kb) - A) / B
            T = 1 / T

            'If T < Tmin Then T = Tmin
            'If T > Tmax Then T = Tmax

            Dim Hv, Hl1, Hl2 As Double

            Hv = proppack.DW_CalcEnthalpy(Vy, T, Pf, State.Vapor) * proppack.AUX_MMM(Vy)
            Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx1)
            Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx2)
            eberror = Hf - V * Hv - L1 * Hl1 - L2 * Hl2

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return Abs(eberror)

        End Function

        Private Function EntropyBalance(ByVal R As Double) As Double

            Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0
            Dim i As Integer

            S = (1 - R)
            Do
                S0 = S
                S1 = S + 0.001
                fr = Me.SErrorFunc(S0, R)
                dfr = (fr - Me.SErrorFunc(S1, R)) / -0.001
                S += -fr / dfr
                icount += 1
            Loop Until Abs(fr) < itol Or icount > maxit_i

            If S <= -(1 - R) Then S = -(1 - R)
            If S >= (1 - R) Then S = (1 - R)

            For i = 0 To n
                pi(i) = fi(i) / (R + (1 - R + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - R - S) / (2 * Kb0 * Exp(ui2(i))))
            Next

            Dim sumpi As Double = 0
            Dim sumeuipi1 As Double = 0
            Dim sumeuipi2 As Double = 0
            For i = 0 To n
                sumpi += pi(i)
                sumeuipi1 += pi(i) / Exp(ui1(i))
                sumeuipi2 += pi(i) / Exp(ui2(i))
            Next
            For i = 0 To n
                Vx1(i) = (pi(i) / Exp(ui1(i))) / sumeuipi1
                Vx2(i) = (pi(i) / Exp(ui2(i))) / sumeuipi2
                Vy(i) = pi(i) / sumpi
            Next

            If R <> 1 Then
                Kb = ((1 - R + S) * sumeuipi1 + (1 - R - S) * sumeuipi2) / (2 * (1 - R) * sumpi)
            Else
                Kb = 1.0#
            End If

            V = R * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)

            T = 1 / T_ + (Log(Kb) - A) / B
            T = 1 / T

            'If T < Tmin Then T = Tmin
            'If T > Tmax Then T = Tmax

            Dim Sv, Sl1, Sl2 As Double

            Sv = proppack.DW_CalcEntropy(Vy, T, Pf, State.Vapor) * proppack.AUX_MMM(Vy)
            Sl1 = proppack.DW_CalcEntropy(Vx1, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx1)
            Sl2 = proppack.DW_CalcEntropy(Vx2, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx2)

            Dim eberror As Double = Sf - V * Sv - L1 * Sl1 - L2 * Sl2

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return eberror

        End Function

        Private Function EntropyBalanceAbs(ByVal R As Double) As Double

            Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0
            Dim i As Integer

            S = (1 - R)
            Do
                S0 = S
                S1 = S + 0.001
                fr = Me.SErrorFunc(S0, R)
                dfr = (fr - Me.SErrorFunc(S1, R)) / -0.001
                S += -fr / dfr
                icount += 1
            Loop Until Abs(fr) < itol Or icount > maxit_i

            If S <= -(1 - R) Then S = -(1 - R)
            If S >= (1 - R) Then S = (1 - R)

            For i = 0 To n
                pi(i) = fi(i) / (R + (1 - R + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - R - S) / (2 * Kb0 * Exp(ui2(i))))
            Next

            Dim sumpi As Double = 0
            Dim sumeuipi1 As Double = 0
            Dim sumeuipi2 As Double = 0
            For i = 0 To n
                sumpi += pi(i)
                sumeuipi1 += pi(i) / Exp(ui1(i))
                sumeuipi2 += pi(i) / Exp(ui2(i))
            Next
            For i = 0 To n
                Vx1(i) = (pi(i) / Exp(ui1(i))) / sumeuipi1
                Vx2(i) = (pi(i) / Exp(ui2(i))) / sumeuipi2
                Vy(i) = pi(i) / sumpi
            Next

            If R <> 1 Then
                Kb = ((1 - R + S) * sumeuipi1 + (1 - R - S) * sumeuipi2) / (2 * (1 - R) * sumpi)
            Else
                Kb = 1.0#
            End If

            V = R * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)

            T = 1 / T_ + (Log(Kb) - A) / B
            T = 1 / T

            'If T < Tmin Then T = Tmin
            'If T > Tmax Then T = Tmax

            Dim Sv, Sl1, Sl2 As Double

            Sv = proppack.DW_CalcEntropy(Vy, T, Pf, State.Vapor) * proppack.AUX_MMM(Vy)
            Sl1 = proppack.DW_CalcEntropy(Vx1, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx1)
            Sl2 = proppack.DW_CalcEntropy(Vx2, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx2)

            Dim eberror As Double = Sf - V * Sv - L1 * Sl1 - L2 * Sl2

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return Abs(eberror)

        End Function

        Public Function Flash_PH_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST() As Double, ByVal Vx1EST() As Double, ByVal Vx2EST() As Double, ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackage) As Object

            Dim i, j As Integer

            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            n = Vz.Length - 1

            proppack = PP
            Hf = H * PP.AUX_MMM(Vz)
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), uic1(n), ui2(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n), Vpc(n), VTc(n), Vw(n)

            Vn = PP.RET_VNAMES()
            VTc = PP.RET_VTC
            fi = Vz.Clone

            Tmin = 0
            Tmax = 0
            If Tref = 0 Or Double.IsNaN(Tref) Then
                i = 0
                Tref = 0
                Do
                    Tref += 0.8 * Vz(i) * VTc(i)
                    Tmin += 0.1 * Vz(i) * VTc(i)
                    Tmax += 5.0 * Vz(i) * VTc(i)
                    i += 1
                Loop Until i = n + 1
            Else
                Tmin = Tref - 200
                Tmax = Tref + 200
            End If
            If Tmin < 100 Then Tmin = 100

            '--------------------------------------
            ' STEP 1 - Assume u, A, B, C, D, E, F 
            '--------------------------------------

            T = Tref + 0.1
            T_ = Tref - 0.1
            T0 = Tref

            '----------------------------------------
            ' STEP 1.1 - Estimate K, Vx, Vy, V and L 
            '----------------------------------------

            'Calculate Ki`s

            Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P, "LV")
            Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P, "LV")

            'Estimate V

            Kb_ = L1est * CalcKbj1(PP.DW_CalcKvalue(Vx1EST, VyEST, T_, P)) + L2est * CalcKbj1(PP.DW_CalcKvalue(Vx2EST, VyEST, T_, P))
            Kb = L1est * CalcKbj1(PP.DW_CalcKvalue(Vx1EST, VyEST, T, P)) + L2est * CalcKbj1(PP.DW_CalcKvalue(Vx2EST, VyEST, T, P))
            Kb0 = Kb_

            B = Log(Kb_ / Kb) / (1 / T_ - 1 / T)
            A = Log(Kb) - B * (1 / T - 1 / T_)

            For i = 0 To n
                ui1(i) = Log(Ki1(i) / Kb)
                ui2(i) = Log(Ki2(i) / Kb)
            Next

            Dim fx(2 * n + 3), x(2 * n + 3), dfdx(2 * n + 3, 2 * n + 3), dx(2 * n + 3), xbr(2 * n + 3), fbr(2 * n + 3), fr As Double

            ecount = 0

            V = Vest
            L1 = L1est
            L2 = L2est
            L = L1 + L2
            beta = L1 / L2

            R = Kb * V / (Kb * V + Kb0 * L)

            Dim bo2 As New BrentOpt.BrentMinimize

restart:    Do

                '--------------------------------------------------------------
                ' STEPS 2, 3, 4, 5, 6, 7 and 8 - Calculate R and Energy Balance
                '--------------------------------------------------------------

                Rant = R
                Tant = T

                bo2.DefineFuncDelegate(AddressOf EnergyBalanceAbs)
                fr = bo2.brentoptimize(0.0#, 1.0#, 0.00000000001, R)

                If ecount > 0 Then
                    If Rant = 0.0# And R = 1.0# Then R = 0.0#
                    If Rant = 1.0# And R = 0.0# Then R = 1.0#
                End If

                'At this point, we have converged T and R for the simplified model. Proceed to step 9.

                '----------------------------------------------------------
                ' STEP 9 - Rigorous model Enthalpy and K-values calculation
                '----------------------------------------------------------

                Me.EnergyBalance(R)

                Ki1 = PP.DW_CalcKvalue(Vx1, Vy, T, P)
                Ki2 = PP.DW_CalcKvalue(Vx2, Vy, T, P)

                For i = 0 To n
                    If Ki1(i) <> 0 Then
                        uic1(i) = Log(Ki1(i) / Kb)
                    Else
                        uic1(i) = ui1(i)
                    End If
                    If Ki2(i) <> 0 Then
                        uic2(i) = Log(Ki2(i) / Kb)
                    Else
                        uic2(i) = ui2(i)
                    End If
                Next

                Bc = Log(Kb_ / Kb) / (1 / T_ - 1 / T)
                Ac = Log(Kb) - Bc * (1 / T - 1 / T_)

                '-------------------------------------------
                ' STEP 10 - Update variables using Broyden
                '-------------------------------------------

                For i = 0 To n
                    fx(i) = (ui1(i) - uic1(i))
                    x(i) = ui1(i)
                Next

                For i = n + 1 To 2 * n + 1
                    fx(i) = (ui2(i - n - 1) - uic2(i - n - 1))
                    x(i) = ui2(i - n - 1)
                Next

                If Me.FlashSettings(Interfaces.Enums.FlashSetting.IO_FastMode) = True Then

                    fx(2 * n + 2) = (A - Ac)
                    fx(2 * n + 3) = (B - Bc)
                    x(2 * n + 2) = A
                    x(2 * n + 3) = B

                    If ecount = 0 Then
                        For i = 0 To 2 * n + 3
                            For j = 0 To 2 * n + 3
                                If i = j Then dfdx(i, j) = 1 Else dfdx(i, j) = 0
                            Next
                        Next
                        Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 0)
                    Else
                        Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 1)
                    End If

                    For i = 0 To n
                        ui1(i) += dx(i)
                    Next
                    For i = n + 1 To 2 * n + 1
                        ui2(i - n - 1) += dx(i)
                    Next

                    A += dx(2 * n + 2)
                    B += dx(2 * n + 3)

                Else

                    For i = 0 To n
                        ui1(i) = uic1(i)
                        ui2(i) = uic2(i)
                    Next
                    A = Ac
                    B = Bc

                End If

                ecount += 1

                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))
                If Double.IsNaN(AbsSum(fx)) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                WriteDebugInfo("PH Flash 3P [IO]: Iteration #" & ecount & ", T = " & T)
                WriteDebugInfo("PH Flash 3P [IO]: Iteration #" & ecount & ", VF = " & V)
                WriteDebugInfo("PH Flash 3P [IO]: Iteration #" & ecount & ", L1F = " & L1)
                WriteDebugInfo("PH Flash 3P [IO]: Iteration #" & ecount & ", L2F = " & L2)
                WriteDebugInfo("PH Flash 3P [IO]: Iteration #" & ecount & ", H error = " & fr)

                If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until AbsSum(fx) < etol

            If Abs(fr) > itol Then
                If V <= 0.001 Then
                    'single phase solution found (liquid only). Obtain T using single phase calculation.
                    Dim ic As Integer
                    Dim bo As New BrentOpt.Brent
                    bo.DefineFuncDelegate(AddressOf EnergyBalanceSPL)
                    ic = 0
                    Do
                        Tant = T
                        T = bo.BrentOpt(Tmin, Tmax, 5, 0.1, maxit_i, Nothing)
                        Me.Flash_PT_3P(fi, V, L1, L2, Vy, Vx1, Vx2, P, T, PP)
                        ic += 1
                    Loop Until Abs(T - Tant) < 0.1 Or ic > 10
                Else
                    'single phase solution found (vapor only). Obtain T using single phase calculation.
                    Dim x1, fx2, dfdx2 As Double
                    ecount = 0
                    If Tref = 0 Then Tref = 298.15
                    x1 = Tref
                    Do
                        fx2 = EnergyBalanceSPV(x1, Nothing)
                        If Math.Abs(fx2) < etol Then Exit Do
                        dfdx2 = (EnergyBalanceSPV(x1 + 1, Nothing) - fx2)
                        x1 = x1 - fx2 / dfdx2
                        ecount += 1
                    Loop Until ecount > maxit_e Or Double.IsNaN(x1)
                    T = x1
                    Vy = Vz
                End If
            End If

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Private Function EnergyBalanceSPL(ByVal T As Double, ByVal otherargs As Object) As Double

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim Hl1, Hl2, balerror As Double

            Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx1)
            Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx2)

            balerror = Hf - L1 * Hl1 - L2 * Hl2

            WriteDebugInfo("PH Flash [IO]: Iteration #" & ecount & ", T = " & T)
            WriteDebugInfo("PH Flash [IO]: Iteration #" & ecount & ", L = 1 (SP)")
            WriteDebugInfo("PH Flash [IO]: Iteration #" & ecount & ", H error = " & balerror)

            Return balerror

        End Function

        Private Function EnergyBalanceSPV(ByVal T As Double, ByVal otherargs As Object) As Double

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim HV, balerror As Double

            HV = proppack.DW_CalcEnthalpy(fi, T, Pf, PropertyPackages.State.Vapor) * proppack.AUX_MMM(fi)

            balerror = Hf - HV

            WriteDebugInfo("PH Flash [IO]: Iteration #" & ecount & ", T = " & T)
            WriteDebugInfo("PH Flash [IO]: Iteration #" & ecount & ", L1 = 1 (SP)")
            WriteDebugInfo("PH Flash [IO]: Iteration #" & ecount & ", H error = " & balerror)

            Return balerror


        End Function

        Public Function Flash_PS_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST() As Double, ByVal Vx1EST() As Double, ByVal Vx2EST() As Double, ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackage) As Object

            Dim i, j As Integer

            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            n = Vz.Length - 1

            proppack = PP
            Sf = S * PP.AUX_MMM(Vz)
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), uic1(n), ui2(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n), Vpc(n), VTc(n), Vw(n)

            Vn = PP.RET_VNAMES()
            VTc = PP.RET_VTC
            fi = Vz.Clone

            Tmin = 0.0#
            Tmax = 0.0#
            If Tref = 0.0# Or Double.IsNaN(Tref) Then
                i = 0
                Tref = 0.0#
                Do
                    Tref += 0.8 * Vz(i) * VTc(i)
                    Tmin += 0.1 * Vz(i) * VTc(i)
                    Tmax += 5.0 * Vz(i) * VTc(i)
                    i += 1
                Loop Until i = n + 1
            Else
                Tmin = Tref - 200
                Tmax = Tref + 200
            End If
            If Tmin < 100 Then Tmin = 100

            '--------------------------------------
            ' STEP 1 - Assume u, A, B, C, D, E, F 
            '--------------------------------------

            T = Tref + 0.1
            T_ = Tref - 0.1
            T0 = Tref

            '----------------------------------------
            ' STEP 1.1 - Estimate K, Vx, Vy, V and L 
            '----------------------------------------

            'Calculate Ki`s

            Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P, "LV")
            Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P, "LV")

            'Estimate V

            Kb_ = CalcKbj1(PP.DW_CalcKvalue(Vx1EST, VyEST, T_, P))
            Kb = CalcKbj1(PP.DW_CalcKvalue(Vx1EST, VyEST, T, P))
            Kb0 = Kb_

            B = Log(Kb_ / Kb) / (1 / T_ - 1 / T)
            A = Log(Kb) - B * (1 / T - 1 / T_)

            For i = 0 To n
                ui1(i) = Log(Ki1(i) / Kb)
                ui2(i) = Log(Ki2(i) / Kb)
            Next

            Dim fx(2 * n + 3), x(2 * n + 3), dfdx(2 * n + 3, 2 * n + 3), dx(2 * n + 3), xbr(2 * n + 3), fbr(2 * n + 3), fr As Double

            ecount = 0

            V = Vest
            L1 = L1est
            L2 = L2est
            L = L1 + L2
            beta = L1 / L2

            R = Kb * V / (Kb * V + Kb0 * L)

            Dim bo2 As New BrentOpt.BrentMinimize

restart:    Do

                '--------------------------------------------------------------
                ' STEPS 2, 3, 4, 5, 6, 7 and 8 - Calculate R and Energy Balance
                '--------------------------------------------------------------

                Rant = R
                Tant = T

                bo2.DefineFuncDelegate(AddressOf EntropyBalanceAbs)
                fr = bo2.brentoptimize(0.0#, 1.0#, 0.00000001, R)

                'At this point, we have converged T and R for the simplified model. Proceed to step 9.

                '----------------------------------------------------------
                ' STEP 9 - Rigorous model Enthalpy and K-values calculation
                '----------------------------------------------------------

                Me.EntropyBalance(R)

                Ki1 = PP.DW_CalcKvalue(Vx1, Vy, T, P)
                Ki2 = PP.DW_CalcKvalue(Vx2, Vy, T, P)


                For i = 0 To n
                    If Ki1(i) <> 0 Then
                        uic1(i) = Log(Ki1(i) / Kb)
                    Else
                        uic1(i) = ui1(i)
                    End If
                    If Ki2(i) <> 0 Then
                        uic2(i) = Log(Ki2(i) / Kb)
                    Else
                        uic2(i) = ui2(i)
                    End If
                Next

                Bc = Log(Kb_ / Kb) / (1 / T_ - 1 / T)
                Ac = Log(Kb) - Bc * (1 / T - 1 / T_)

                '-------------------------------------------
                ' STEP 10 - Update variables using Broyden
                '-------------------------------------------

                For i = 0 To n
                    fx(i) = (ui1(i) - uic1(i))
                    x(i) = ui1(i)
                Next

                For i = n + 1 To 2 * n + 1
                    fx(i) = (ui2(i - n - 1) - uic2(i - n - 1))
                    x(i) = ui2(i - n - 1)
                Next

                If Me.FlashSettings(Interfaces.Enums.FlashSetting.IO_FastMode) = True Then

                    fx(2 * n + 2) = (A - Ac)
                    fx(2 * n + 3) = (B - Bc)
                    x(2 * n + 2) = A
                    x(2 * n + 3) = B

                    If ecount = 0 Then
                        For i = 0 To 2 * n + 3
                            For j = 0 To 2 * n + 3
                                If i = j Then dfdx(i, j) = 1 Else dfdx(i, j) = 0
                            Next
                        Next
                        Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 0)
                    Else
                        Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 1)
                    End If

                    For i = 0 To n
                        ui1(i) = ui1(i) + dx(i)
                    Next
                    For i = n + 1 To 2 * n + 1
                        ui2(i - n - 1) = ui2(i - n - 1) + dx(i)
                    Next

                    A += dx(2 * n + 2)
                    B += dx(2 * n + 3)

                Else

                    For i = 0 To n
                        ui1(i) = uic1(i)
                        ui2(i) = uic2(i)
                    Next
                    A = Ac
                    B = Bc

                End If

                ecount += 1

                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))
                If Double.IsNaN(AbsSum(fx)) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                WriteDebugInfo("PS Flash 3P [IO]: Iteration #" & ecount & ", T = " & T)
                WriteDebugInfo("PS Flash 3P [IO]: Iteration #" & ecount & ", VF = " & V)
                WriteDebugInfo("PS Flash 3P [IO]: Iteration #" & ecount & ", S error = " & fr)

                If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until AbsSum(fx) < etol

            If Abs(fr) > itol Then
                If V <= 0.001 Then
                    'single phase solution found (liquid only). Obtain T using single phase calculation.
                    Dim bo As New BrentOpt.Brent
                    Dim ic As Integer
                    bo.DefineFuncDelegate(AddressOf EntropyBalanceSPL)
                    T = bo.BrentOpt(Tmin, Tmax, 5, 0.1, maxit_i, Nothing)
                    ic = 0
                    Do
                        Tant = T
                        T = bo.BrentOpt(Tmin, Tmax, 5, 0.1, maxit_i, Nothing)
                        Me.Flash_PT_3P(fi, V, L1, L2, Vy, Vx1, Vx2, P, T, PP)
                        ic += 1
                    Loop Until Abs(T - Tant) < 0.01 Or ic > 10
                Else
                    'single phase solution found (vapor only). Obtain T using single phase calculation.
                    Dim x1, fx2, dfdx2 As Double
                    ecount = 0
                    If Tref = 0 Then Tref = 298.15
                    x1 = Tref
                    Do
                        fx2 = EntropyBalanceSPV(x1, Nothing)
                        If Math.Abs(fx2) < etol Then Exit Do
                        dfdx2 = (EntropyBalanceSPV(x1 + 1, Nothing) - fx2)
                        x1 = x1 - fx2 / dfdx2
                        ecount += 1
                    Loop Until ecount > maxit_e Or Double.IsNaN(x1)
                    T = x1
                    Vy = Vz
                End If
                'confirm single-phase solution with a PT Flash.
                Dim res As Object = Me.Flash_PT(Vz, P, T, PP, False, Nothing)
                If Abs(L - res(0)) > 0.0001 And Abs(V - res(1)) > 0.0001 Then
                    'NOT SP solution. go back to 2-phase loop.
                    GoTo restart
                End If
            End If

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Private Function EntropyBalanceSPL(ByVal T As Double, ByVal otherargs As Object) As Double

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim Sl1, Sl2, balerror As Double

            Sl1 = proppack.DW_CalcEntropy(Vx1, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx1)
            Sl2 = proppack.DW_CalcEntropy(Vx2, T, Pf, State.Liquid) * proppack.AUX_MMM(Vx2)

            balerror = Sf - L1 * Sl1 - L2 * Sl2

            WriteDebugInfo("PS 3P Flash [IO]: Iteration #" & ecount & ", T = " & T)
            WriteDebugInfo("PS 3P Flash [IO]: Iteration #" & ecount & ", L = 1 (SP)")
            WriteDebugInfo("PS 3P Flash [IO]: Iteration #" & ecount & ", S error = " & balerror)

            Return balerror

        End Function

        Private Function EntropyBalanceSPV(ByVal T As Double, ByVal otherargs As Object) As Double

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim SV, balerror As Double

            SV = proppack.DW_CalcEnthalpy(fi, T, Pf, PropertyPackages.State.Vapor) * proppack.AUX_MMM(fi)

            balerror = Sf - SV

            WriteDebugInfo("PS Flash [IO]: Iteration #" & ecount & ", T = " & T)
            WriteDebugInfo("PS Flash [IO]: Iteration #" & ecount & ", V = 1 (SP)")
            WriteDebugInfo("PS Flash [IO]: Iteration #" & ecount & ", S error = " & balerror)

            Return balerror


        End Function

        Public Function Flash_PT_3P(ByVal Vz As Double(), ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object

            Dim i, j As Integer

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), ui2(n), uic1(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            '--------------------------------------
            ' STEP 1 - Assume u, A, B, C, D, E, F 
            '--------------------------------------


            '----------------------------------------
            ' STEP 1.1 - Estimate K, Vx, Vy, V and L 
            '----------------------------------------

            'Calculate Ki`s

            Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P)
            Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P)

            If n = 0 Then
                If Vp(0) <= P Then
                    L = 1
                    V = 0
                    Vx1 = Vz
                    GoTo out
                Else
                    L = 0
                    V = 1
                    Vy = Vz
                    GoTo out
                End If
            End If

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = VyEST(i)
                    Vx1(i) = Vx1EST(i)
                    Vx2(i) = Vx2EST(i)
                Else
                    Vy(i) = 0
                    Vx1(i) = 0
                    Vx2(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x1 = 0
            soma_x2 = 0
            soma_y = 0
            Do
                soma_x1 = soma_x1 + Vx1(i)
                soma_x2 = soma_x2 + Vx2(i)
                soma_y = soma_y + Vy(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                Vx1(i) = Vx1(i) / soma_x1
                Vx2(i) = Vx2(i) / soma_x2
                Vy(i) = Vy(i) / soma_y
                i = i + 1
            Loop Until i = n + 1

            Kb = 1.0# 'CalcKbjw(Ki1, Ki2, L1, L2, Vx1, Vx2)
            Kb0 = Kb

            For i = 0 To n
                ui1(i) = Log(Ki1(i))
                ui2(i) = Log(Ki2(i))
            Next

            Dim fx(2 * n + 1), x(2 * n + 1), dfdx(2 * n + 1, 2 * n + 1), dx(2 * n + 1), xbr(2 * n + 1), fbr(2 * n + 1) As Double
            Dim bo As New BrentOpt.Brent
            Dim bo2 As New BrentOpt.BrentMinimize

            ecount = 0

            V = Vest
            L1 = L1est
            L2 = L2est
            L = L1 + L2
            beta = L1 / L2

            R = Kb * V / (Kb * V + Kb0 * L)

            Do

                '--------------------------------------------------------------
                ' STEPS 2, 3, 4, 5, 6, 7 and 8 - Calculate R and Energy Balance
                '--------------------------------------------------------------

                'Rant = R
                'R = Kb * V / (Kb * V + Kb0 * L)

                Dim fr As Double
                bo2.DefineFuncDelegate(AddressOf TPErrorFunc)
                Rant = R
                fr = bo2.brentoptimize(0.0#, 1.0#, 0.00000001, R)

                If Rant = 0.0# And R = 1.0# Then R = 0.0#
                If Rant = 1.0# And R = 0.0# Then R = 1.0#

                fr = Me.TPErrorFunc(R)

                'At this point, we have converged R for the simplified model. Proceed to step 9.

                '----------------------------------------------------------
                ' STEP 9 - Rigorous model Enthalpy and K-values calculation
                '----------------------------------------------------------

                Ki1 = PP.DW_CalcKvalue(Vx1, Vy, T, P)
                Ki2 = PP.DW_CalcKvalue(Vx2, Vy, T, P)
                'Kb = CalcKbjw(Ki1, Ki2, L1, L2, Vx1, Vx2)

                For i = 0 To n
                    uic1(i) = Log(Ki1(i))
                    uic2(i) = Log(Ki2(i))
                Next

                '-------------------------------------------
                ' STEP 10 - Update variables using Broyden
                '-------------------------------------------

                For i = 0 To n
                    fx(i) = (ui1(i) - uic1(i))
                    x(i) = ui1(i)
                Next

                For i = n + 1 To 2 * n + 1
                    fx(i) = (ui2(i - n - 1) - uic2(i - n - 1))
                    x(i) = ui2(i - n - 1)
                Next

                If Me.FlashSettings(Interfaces.Enums.FlashSetting.IO_FastMode) = True Then

                    If ecount = 0 Then
                        For i = 0 To 2 * n + 1
                            For j = 0 To 2 * n + 1
                                If i = j Then dfdx(i, j) = 1 Else dfdx(i, j) = 0
                            Next
                        Next
                        Broyden.broydn(2 * n + 1, x, fx, dx, xbr, fbr, dfdx, 0)
                    Else
                        Broyden.broydn(2 * n + 1, x, fx, dx, xbr, fbr, dfdx, 1)
                    End If

                    For i = 0 To n
                        ui1(i) = ui1(i) + dx(i)
                    Next
                    For i = n + 1 To 2 * n + 1
                        ui2(i - n - 1) = ui2(i - n - 1) + dx(i)
                    Next

                Else

                    For i = 0 To n
                        ui1(i) = uic1(i)
                        ui2(i) = uic2(i)
                    Next

                End If

                ecount += 1

                If Double.IsNaN(V) Then
                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))
                End If
                If ecount > maxit_e Then
                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))
                End If

                WriteDebugInfo("PT Flash [IO]: Iteration #" & ecount & ", VF = " & V)

                If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until AbsSum(fx) < etol

out:

            'check if liquid phase compositions are the same.
            Dim Kl(n) As Double
            For i = 0 To n
                If Vx1(i) <> 0.0# Then Kl(i) = Vx2(i) / Vx1(i) Else Kl(i) = 0.0#
            Next

            If PP.AUX_CheckTrivial(Kl) Then
                'the liquid phases are the same. condense them into only one phase.
                L1 = L1 + L2
                L2 = 0.0#
                Vx2 = PP.RET_NullVector
                Return New Object() {L1, V, Vx1, Vy, ecount, L2, Vx2, 0.0#, PP.RET_NullVector}
            Else
                'order liquid phases by mixture NBP

                Dim VNBP = PP.RET_VTB()
                Dim nbp1 As Double = 0
                Dim nbp2 As Double = 0
                For i = 0 To n
                    nbp1 += Vx1(i) * VNBP(i)
                    nbp2 += Vx2(i) * VNBP(i)
                Next
                If nbp1 >= nbp2 Then
                    Return New Object() {L1, V, Vx1, Vy, ecount, L2, Vx2, 0.0#, PP.RET_NullVector}
                Else
                    Return New Object() {L2, V, Vx2, Vy, ecount, L1, Vx1, 0.0#, PP.RET_NullVector}
                End If
            End If


        End Function

        Private Function TPErrorFunc(ByVal Rtv As Double) As Double

            Dim i As Integer

            'Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0

            Dim bo2 As New BrentOpt.BrentMinimize
            Dim fr As Double
            bo2.DefineFuncDelegate(AddressOf SErrorFuncAbs)
            Rt = Rtv
            fr = bo2.brentoptimize(-(1 - Rt), (1 - Rt), 0.00000001, S)

            If S <= -(1 - Rt) Then S = -(1 - Rt)
            If S >= (1 - Rt) Then S = (1 - Rt)

            For i = 0 To n
                pi(i) = fi(i) / (Rt + (1 - Rt + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - Rt - S) / (2 * Kb0 * Exp(ui2(i))))
            Next

            Dim sumpi As Double = 0
            Dim sumeuipi1 As Double = 0
            Dim sumeuipi2 As Double = 0
            For i = 0 To n
                sumpi += pi(i)
                sumeuipi1 += pi(i) / Exp(ui1(i))
                sumeuipi2 += pi(i) / Exp(ui2(i))
            Next
            For i = 0 To n
                Vx1(i) = (pi(i) / Exp(ui1(i))) / sumeuipi1
                Vx2(i) = (pi(i) / Exp(ui2(i))) / sumeuipi2
                Vy(i) = pi(i) / sumpi
            Next

            'If Rt <> 1.0# Then
            Kb = ((1 - Rt + S) * sumeuipi1 + (1 - Rt - S) * sumeuipi2) / (2 * (1 - Rt) * sumpi)
            'Else
            'Kb = 1.0#
            'End If

            V = Rt * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)

            Dim err1 As Double = Kb - 1

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return err1 ^ 2

        End Function

        Private Function SErrorFuncAbs(ByVal S0 As Double) As Double

            Dim i As Integer

            Dim errfunc As Double = 0
            For i = 0 To n
                errfunc += fi(i) * (1 / Exp(ui1(i)) - 1 / Exp(ui2(i))) / (Rt + (1 - Rt + S0) / (2 * Kb0 * Exp(ui1(i))) + (1 - Rt - S0) / (2 * Kb0 * Exp(ui2(i))))
            Next
            Return errfunc ^ 2

        End Function

        Private Function SErrorFunc(ByVal S0 As Double, ByVal Rt As Double) As Double

            Dim i As Integer

            Dim errfunc As Double = 0
            For i = 0 To n
                errfunc += fi(i) * (1 / Exp(ui1(i)) - 1 / Exp(ui2(i))) / (Rt + (1 - Rt + S0) / (2 * Kb0 * Exp(ui1(i))) + (1 - Rt - S0) / (2 * Kb0 * Exp(ui2(i))))
            Next
            Return errfunc

        End Function

        Public Function Flash_PV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim i, j As Integer

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP
            Vf = V
            L = 1 - V
            Lf = 1 - Vf
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), uic1(n), ui2(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n)
            Dim Vt(n), VTc(n), Tmin, Tmax As Double

            Vn = PP.RET_VNAMES()
            VTc = PP.RET_VTC
            fi = Vz.Clone

            '--------------------------------------
            ' STEP 1 - Assume u, A, B, C, D, E, F 
            '--------------------------------------

            Tmin = 0
            Tmax = 0
            If Tref = 0 Then

                i = 0
                Tref = 0
                Do
                    Tref += 0.8 * Vz(i) * VTc(i)
                    Tmin += 0.1 * Vz(i) * VTc(i)
                    Tmax += 2.0 * Vz(i) * VTc(i)
                    i += 1
                Loop Until i = n + 1

            Else

                Tmin = Tref - 50
                Tmax = Tref + 50

            End If

            T = Tref
            T_ = T - 10
            T0 = T - 20

            '----------------------------------------
            ' STEP 1.1 - Estimate K, Vx, Vy, V and L 
            '----------------------------------------

            'Calculate Ki`s

            Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P, "LV")
            Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P, "LV")

            'Estimate V

            Kb_ = CalcKbj1(PP.DW_CalcKvalue(Vx1EST, VyEST, T_, P))
            Kb = CalcKbj1(PP.DW_CalcKvalue(Vx1EST, VyEST, T, P))
            Kb0 = Kb_

            B = Log(Kb_ / Kb) / (1 / T_ - 1 / T)
            A = Log(Kb) - B * (1 / T - 1 / T0)

            For i = 0 To n
                ui1(i) = Log(Ki1(i) / Kb)
                ui2(i) = Log(Ki2(i) / Kb)
            Next

            Dim fx(2 * n + 3), x(2 * n + 3), dfdx(2 * n + 3, 2 * n + 3), dx(2 * n + 3), xbr(2 * n + 3), fbr(2 * n + 3), fr As Double

            ecount = 0

            V = Vest
            L1 = L1est
            L2 = L2est
            L = L1 + L2
            beta = L1 / L2

            R = Kb * V / (Kb * V + Kb0 * L)
            Dim RLoop As Boolean = True

            If V = 0.0# Or V = 1.0# Then RLoop = False

            ecount = 0

            Do

                '--------------------------------------------------------------
                ' STEPS 2, 3, 4, 5, 6, 7 and 8 - Calculate R and Energy Balance
                '--------------------------------------------------------------

                R = Kb * V / (Kb * V + Kb0 * L)

                If RLoop Then

                    Dim dfr, R0, R1 As Double
                    Dim icount As Integer = 0

                    Do
                        R1 = R + 0.001
                        fr = Me.LiquidFractionBalance(R)
                        dfr = (fr - Me.LiquidFractionBalance(R1)) / -0.001
                        R0 = R
                        If (R - fr / dfr) < 0.0# Or (R - fr / dfr) > 1.0# Then
                            If (R + 0.1) < 1.0# Then R += 0.1 Else R -= 0.1
                        Else
                            R = R - fr / dfr
                        End If
                        If R < 0.0# Then R = 0.0#
                        If R > 1.0# Then R = 1.0#
                        icount += 1
                    Loop Until Abs(fr) < itol Or icount > maxit_i Or R = 0 Or R = 1

                    If icount > maxit_i Then R = Rant
                    If Rant = 0.0# And R = 1.0# Then R = 0.0#
                    If Rant = 1.0# And R = 0.0# Then R = 1.0#

                    Me.LiquidFractionBalance(R)

                Else

                    Me.LiquidFractionBalance(R)

                End If

                'At this point, we have converged T for the simplified model. Proceed to step 9.

                '----------------------------------------------------------
                ' STEP 9 - Rigorous model Enthalpy and K-values calculation
                '----------------------------------------------------------

                Ki1 = PP.DW_CalcKvalue(Vx1, Vy, T, P)
                Ki2 = PP.DW_CalcKvalue(Vx2, Vy, T, P)

                For i = 0 To n
                    uic1(i) = Log(Ki1(i) / Kb)
                    uic2(i) = Log(Ki2(i) / Kb)
                Next

                Bc = Log(Kb_ / Kb) / (1 / T_ - 1 / T)
                Ac = Log(Kb) - Bc * (1 / T - 1 / T0)

                '-------------------------------------------
                ' STEP 10 - Update variables using Broyden
                '-------------------------------------------

                For i = 0 To n
                    fx(i) = (ui1(i) - uic1(i))
                    x(i) = ui1(i)
                Next

                For i = n + 1 To 2 * n + 1
                    fx(i) = (ui2(i - n - 1) - uic2(i - n - 1))
                    x(i) = ui2(i - n - 1)
                Next

                fx(2 * n + 2) = (A - Ac)
                fx(2 * n + 3) = (B - Bc)
                x(2 * n + 2) = A
                x(2 * n + 3) = B

                If Me.FlashSettings(Interfaces.Enums.FlashSetting.IO_FastMode) = True Then

                    If ecount = 0 Then
                        For i = 0 To 2 * n + 3
                            For j = 0 To 2 * n + 3
                                If i = j Then dfdx(i, j) = 1 Else dfdx(i, j) = 0
                            Next
                        Next
                        Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 0)
                    Else
                        Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 1)
                    End If

                    For i = 0 To n
                        ui1(i) = ui1(i) + dx(i)
                    Next
                    For i = n + 1 To 2 * n + 1
                        ui2(i - n - 1) = ui2(i - n - 1) + dx(i)
                    Next
                    A += dx(2 * n + 2)
                    B += dx(2 * n + 3)

                Else

                    For i = 0 To n
                        ui1(i) = uic1(i)
                    Next
                    For i = n + 1 To 2 * n + 1
                        ui2(i - n - 1) = uic2(i - n - 1)
                    Next

                    A = Ac
                    B = Bc

                End If

                ecount += 1

                If ecount > maxit_e Then
                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))
                End If
                If Double.IsNaN(AbsSum(fx)) Then
                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))
                End If

                WriteDebugInfo("PV Flash 3P [IO]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until AbsSum(fx) < etol * (n + 2)

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Function Flash_TV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackage) As Object

            Dim i, j As Integer

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)

            n = Vz.Length - 1

            proppack = PP
            Vf = V
            L = 1 - V
            Lf = 1 - Vf
            Tf = T

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), uic1(n), ui2(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n)

            Dim VTc = PP.RET_VTC()

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            If Pref = 0 Then

                i = 0
                Do
                    If T / VTc(i) <= 0.9 Then
                        Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    End If
                    i += 1
                Loop Until i = n + 1

                Pmin = Common.Min(Vp)
                Pmax = Common.Max(Vp)

                Pref = Pmin + (1 - V) * (Pmax - Pmin)

            Else

                Pmin = Pref * 0.8
                Pmax = Pref * 1.2

            End If

            P = Pref
            P_ = Pref * 1.05
            P0 = Pref * 0.95

            '----------------------------------------
            ' STEP 1.1 - Estimate K, Vx, Vy, V and L 
            '----------------------------------------

            'Calculate Ki`s

            Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P, "LV")
            Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P, "LV")

            'Estimate V

            T_ = 298.15

            Kb0 = CalcKbj1(PP.DW_CalcKvalue(Vx1, Vy, T, P0))
            Kb_ = CalcKbj1(PP.DW_CalcKvalue(Vx1, Vy, T, P_))
            Kb = CalcKbj1(PP.DW_CalcKvalue(Vx1, Vy, T, P))

            B = Log(Kb_ * P_ / (Kb0 * P0)) / Log(P_ / P0)
            A = Log(Kb * P) - B * Log(P / P0)

            For i = 0 To n
                ui1(i) = Log(Ki1(i) / Kb)
                ui2(i) = Log(Ki2(i) / Kb)
            Next

            Dim fx(2 * n + 3), x(2 * n + 3), dfdx(2 * n + 3, 2 * n + 3), dx(2 * n + 3), xbr(2 * n + 3), fbr(2 * n + 3), fr As Double

            ecount = 0

            V = Vest
            L1 = L1est
            L2 = L2est
            L = L1 + L2
            beta = L1 / L2

            R = Kb * V / (Kb * V + Kb0 * L)
            Dim RLoop As Boolean = True

            If V = 0.0# Or V = 1.0# Then RLoop = False

            ecount = 0

            Do

                '--------------------------------------------------------------
                ' STEPS 2, 3, 4, 5, 6, 7 and 8 - Calculate R and Energy Balance
                '--------------------------------------------------------------

                R = Kb * V / (Kb * V + Kb0 * L)

                If RLoop Then

                    Dim dfr, R0, R1 As Double
                    Dim icount As Integer = 0

                    Do
                        R1 = R + 0.01
                        fr = Me.LiquidFractionBalanceP(R)
                        dfr = (fr - Me.LiquidFractionBalanceP(R1)) / -0.01
                        R0 = R
                        If (R - fr / dfr) < 0.0# Or (R - fr / dfr) > 1.0# Then
                            If (R + 0.1) < 1.0# Then R += 0.1 Else R -= 0.1
                        Else
                            R = R - fr / dfr
                        End If
                        If R < 0.0# Then R = 0.0#
                        If R > 1.0# Then R = 1.0#
                        icount += 1
                    Loop Until Abs(fr) < itol Or icount > maxit_i Or R = 0 Or R = 1

                    If icount > maxit_i Then R = Rant
                    If Rant = 0.0# And R = 1.0# Then R = 0.0#
                    If Rant = 1.0# And R = 0.0# Then R = 1.0#

                    Me.LiquidFractionBalanceP(R)

                Else

                    Me.LiquidFractionBalanceP(R)

                End If

                'At this point, we have converged T for the simplified model. Proceed to step 9.

                '----------------------------------------------------------
                ' STEP 9 - Rigorous model Enthalpy and K-values calculation
                '----------------------------------------------------------

                Ki1 = PP.DW_CalcKvalue(Vx1, Vy, T, P)
                Ki2 = PP.DW_CalcKvalue(Vx2, Vy, T, P)

                For i = 0 To n
                    uic1(i) = Log(Ki1(i) / Kb)
                    uic2(i) = Log(Ki2(i) / Kb)
                Next

                Bc = Log(Kb_ * P_ / (Kb0 * P0)) / Log(P_ / P0)
                Ac = Log(Kb * P) - Bc * Log(P / P0)

                '-------------------------------------------
                ' STEP 10 - Update variables using Broyden
                '-------------------------------------------

                For i = 0 To n
                    fx(i) = (ui1(i) - uic1(i))
                    x(i) = ui1(i)
                Next

                For i = n + 1 To 2 * n + 1
                    fx(i) = (ui2(i - n - 1) - uic2(i - n - 1))
                    x(i) = ui2(i - n - 1)
                Next

                fx(2 * n + 2) = (A - Ac)
                fx(2 * n + 3) = (B - Bc)
                x(2 * n + 2) = A
                x(2 * n + 3) = B

                If ecount = 0 Then
                    For i = 0 To 2 * n + 3
                        For j = 0 To 2 * n + 3
                            If i = j Then dfdx(i, j) = 1 Else dfdx(i, j) = 0
                        Next
                    Next
                    Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 0)
                Else
                    Broyden.broydn(2 * n + 3, x, fx, dx, xbr, fbr, dfdx, 1)
                End If

                For i = 0 To n
                    ui1(i) = ui1(i) + dx(i)
                Next
                For i = n + 1 To 2 * n + 1
                    ui2(i - n - 1) = ui2(i - n - 1) + dx(i)
                Next
                A += dx(2 * n + 2)
                B += dx(2 * n + 3)

                ecount += 1

                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))
                If Double.IsNaN(AbsSum(fx)) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                WriteDebugInfo("TV Flash 3P [IO]: Iteration #" & ecount & ", P = " & P & ", VF = " & V)

                If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until AbsSum(fx) < etol

            Return New Object() {L1, V, Vx1, Vy, P, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Private Function LiquidFractionBalance(ByVal R As Double) As Double

            Dim i As Integer

            Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0

            S = 1 - R
            Do
                S0 = S
                S1 = S + 0.001
                fr = Me.SErrorFunc(S0, R)
                dfr = (fr - Me.SErrorFunc(S1, R)) / -0.001
                S += -fr / dfr
                icount += 1
            Loop Until Abs(fr) < itol Or icount > maxit_i

            If S <= -(1 - R) Then S = -(1 - R)
            If S >= (1 - R) Then S = (1 - R)

            For i = 0 To n
                pi(i) = fi(i) / (R + (1 - R + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - R - S) / (2 * Kb0 * Exp(ui2(i))))
            Next

            Dim sumpi As Double = 0.0#
            Dim sumeuipi1 As Double = 0.0#
            Dim sumeuipi2 As Double = 0.0#
            For i = 0 To n
                sumpi += pi(i)
                sumeuipi1 += pi(i) / Exp(ui1(i))
                sumeuipi2 += pi(i) / Exp(ui2(i))
            Next
            For i = 0 To n
                Vx1(i) = (pi(i) / Exp(ui1(i))) / sumeuipi1
                Vx2(i) = (pi(i) / Exp(ui2(i))) / sumeuipi2
                Vy(i) = pi(i) / sumpi
            Next

            If R <> 1.0# Then
                Kb = ((1 - R + S) * sumeuipi1 + (1 - R - S) * sumeuipi2) / (2 * (1 - R) * sumpi)
            Else
                Kb = 1.0#
            End If

            V = R * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)

            T = 1 / T0 + (Log(Kb) - A) / B
            T = 1 / T

            Dim eberror As Double = (L1 + L2) - Lf

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return eberror

        End Function

        Private Function LiquidFractionBalanceP(ByVal R As Double) As Double

            Dim i As Integer

            Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0

            S = ((-(1 - R)) + (1 - R)) / 2
            Do
                S0 = S
                S1 = S + 0.001
                fr = Me.SErrorFunc(S0, R)
                dfr = (fr - Me.SErrorFunc(S1, R)) / -0.001
                S += -fr / dfr
                icount += 1
            Loop Until Abs(fr) < itol Or icount > maxit_i

            For i = 0 To n
                pi(i) = fi(i) / (R + (1 - R + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - R - S) / (2 * Kb0 * Exp(ui2(i))))
            Next

            Dim sumpi As Double = 0
            Dim sumeuipi1 As Double = 0
            Dim sumeuipi2 As Double = 0
            For i = 0 To n
                sumpi += pi(i)
                sumeuipi1 += pi(i) / Exp(ui1(i))
                sumeuipi2 += pi(i) / Exp(ui2(i))
            Next
            For i = 0 To n
                Vx1(i) = (pi(i) / Exp(ui1(i))) / sumeuipi1
                Vx2(i) = (pi(i) / Exp(ui2(i))) / sumeuipi2
                Vy(i) = pi(i) / sumpi
            Next

            If R <> 1.0# Then
                Kb = ((1 - R + S) * sumeuipi1 + (1 - R - S) * sumeuipi2) / (2 * (1 - R) * sumpi)
            Else
                Kb = 1.0#
            End If

            V = R * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)

            P = Exp((A - Log(Kb) - B * Log(P0)) / (1 - B))

            Dim eberror As Double = (L1 + L2) - Lf

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return eberror

        End Function

        Private Function MinimizeError(ByVal alpha As Double) As Double

            Dim n As Integer = UBound(tmpdx)
            Dim i As Integer
            Dim errors(n) As Double

            For i = 0 To n
                errors(i) = (refx(i) - (currx(i) + alpha * tmpdx(i))) ^ 2
            Next

            Return errors.Sum()

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property
    End Class

End Namespace
