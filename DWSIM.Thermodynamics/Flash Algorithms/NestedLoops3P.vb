'    DWSIM Three-Phase Hybrid Nested Loops / Inside-Out Flash Algorithms
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

Imports System.Math

Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.Common


Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    ''' <summary>
    ''' The Flash algorithms in this class are based on the Nested Loops approach to solve equilibrium calculations.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class NestedLoops3P

        Inherits FlashAlgorithm

        Dim n, ecount As Integer
        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Vn(n) As String
        Dim Vx(n), Vx1(n), Vx2(n), Vy(n), Vx_ant(n), Vx1_ant(n), Vx2_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki2(n), Ki_ant(n), Ki2_ant(n), fi(n) As Double
        Dim L, Lf, L1, L2, V, Vant, Vf, T, Tf, P, Pf, Hf, Hl, Sf, Sl As Double
        Dim ui1(n), ui2(n), uic1(n), uic2(n), pi(n), Ki1(n), Vt(n), Vpc(n), VTc(n), Vw(n) As Double
        Dim beta, R, Rant, S, Sant, Tant, Pant, T_, P_, T0, P0, A, B, C, D, E, F, Ac, Bc, Cc, Dc, Ec, Fc As Double
        Dim DHv, DHl, DHl1, DHl2, Hv0, Hvid, Hlid1, Hlid2, Hm, Hv, Hl1, Hl2 As Double
        Dim DSv, DSl, DSl1, DSl2, Sv0, Svid, Slid1, Slid2, Sm, Sv, Sl1, Sl2 As Double
        Dim Pb, Pd, Pmin, Pmax, Px, soma_x, soma_x1, soma_y, soma_x2 As Double
        Dim Kb, Kb0, Kb_ As Double
        Dim proppack As PropertyPackages.PropertyPackage

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, j, k As Integer

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Convert.ToDouble(PP.Parameters("PP_PTFELT"))
            maxit_e = Convert.ToInt32(PP.Parameters("PP_PTFMEI"))
            itol = Convert.ToDouble(PP.Parameters("PP_PTFILT"))
            maxit_i = Convert.ToInt32(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            proppack = PP

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            'Calculate Ki`s

            If Not ReuseKI Then
                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                For i = 0 To n
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Ki(i) = PrevKi(i)
                Next
            End If

            'Estimate V

            If T > MathEx.Common.Max(proppack.RET_VTC, Vz) Then
                Vy = Vz
                V = 1
                L = 0
                GoTo out
            End If

            i = 0
            Px = 0
            Do
                If Vp(i) <> 0.0# Then Px = Px + (Vz(i) / Vp(i))
                i = i + 1
            Loop Until i = n + 1
            Px = 1 / Px
            Pmin = Px
            i = 0
            Px = 0
            Do
                Px = Px + Vz(i) * Vp(i)
                i = i + 1
            Loop Until i = n + 1
            Pmax = Px
            Pb = Pmax
            Pd = Pmin

            If Abs(Pb - Pd) / Pb < 0.0000001 Then
                'one comp only
                If Px <= P Then
                    L = 1
                    V = 0
                    Vx = Vz
                    GoTo out
                Else
                    L = 0
                    V = 1
                    Vy = Vz
                    GoTo out
                End If
            End If


            Dim Vmin, Vmax, g As Double
            Vmin = 1.0#
            Vmax = 0.0#
            For i = 0 To n
                If (Ki(i) * Vz(i) - 1) / (Ki(i) - 1) < Vmin Then Vmin = (Ki(i) * Vz(i) - 1) / (Ki(i) - 1)
                If (1 - Vz(i)) / (1 - Ki(i)) > Vmax Then Vmax = (1 - Vz(i)) / (1 - Ki(i))
            Next

            If Vmin < 0.0# Then Vmin = 0.0#
            If Vmin = 1.0# Then Vmin = 0.0#
            If Vmax > 1.0# Then Vmax = 1.0#

            V = (Vmin + Vmax) / 2

            g = 0.0#
            For i = 0 To n
                g += Vz(i) * (Ki(i) - 1) / (V + (1 - V) * Ki(i))
            Next

            If g > 0 Then Vmin = V Else Vmax = V

            V = Vmin + (Vmax - Vmin) / 4

            L = 1 - V

            If n = 0 Then
                If Vp(0) <= P Then
                    L = 1
                    V = 0
                Else
                    L = 0
                    V = 1
                End If
            End If

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                    Vx(i) = Vy(i) / Ki(i)
                    If Vy(i) < 0 Then Vy(i) = 0
                    If Vx(i) < 0 Then Vx(i) = 0
                Else
                    Vy(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x = 0
            soma_y = 0
            Do
                soma_x = soma_x + Vx(i)
                soma_y = soma_y + Vy(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                Vx(i) = Vx(i) / soma_x
                Vy(i) = Vy(i) / soma_y
                i = i + 1
            Loop Until i = n + 1

            ecount = 0
            Dim convergiu = 0

            Do

                Ki_ant = Ki.Clone
                Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                i = 0
                Do
                    If Vz(i) <> 0 Then
                        Vy_ant(i) = Vy(i)
                        Vx_ant(i) = Vx(i)
                        Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                        Vx(i) = Vy(i) / Ki(i)
                    Else
                        Vy(i) = 0
                        Vx(i) = 0
                    End If
                    i += 1
                Loop Until i = n + 1

                i = 0
                soma_x = 0
                soma_y = 0
                Do
                    soma_x = soma_x + Vx(i)
                    soma_y = soma_y + Vy(i)
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    Vx(i) = Vx(i) / soma_x
                    Vy(i) = Vy(i) / soma_y
                    i = i + 1
                Loop Until i = n + 1

                Dim e1 As Double = 0
                Dim e2 As Double = 0
                Dim e3 As Double = 0
                i = 0
                Do
                    e1 = e1 + (Vx(i) - Vx_ant(i))
                    e2 = e2 + (Vy(i) - Vy_ant(i))
                    i = i + 1
                Loop Until i = n + 1

                e3 = (V - Vant)

                If Double.IsNaN(Math.Abs(e1) + Math.Abs(e2)) Then

                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                ElseIf Math.Abs(e3) < 0.0000000001 And ecount > 0 Then

                    convergiu = 1

                    Exit Do

                Else

                    Vant = V

                    Dim F = 0.0#
                    Dim dF = 0.0#
                    i = 0
                    Do
                        If Vz(i) > 0 Then
                            F = F + Vz(i) * (Ki(i) - 1) / (1 + V * (Ki(i) - 1))
                            dF = dF - Vz(i) * (Ki(i) - 1) ^ 2 / (1 + V * (Ki(i) - 1)) ^ 2
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    If Abs(F) < 0.000001 Then Exit Do

                    V = -F / dF + V

                End If

                L = 1 - V

                If V > 1 Then
                    V = 1
                    L = 0
                    i = 0
                    Do
                        Vy(i) = Vz(i)
                        i = i + 1
                    Loop Until i = n + 1
                ElseIf V < 0 Then
                    V = 0
                    L = 1
                    i = 0
                    Do
                        Vx(i) = Vz(i)
                        i = i + 1
                    Loop Until i = n + 1
                End If

                ecount += 1

                If Double.IsNaN(V) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))
                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

                WriteDebugInfo("PT Flash [NL]: Iteration #" & ecount & ", VF = " & V)

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until convergiu = 1

out:

            Dim result As Object = New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

            ' check if there is a liquid phase

            If result(0) > 0 Then ' we have a liquid phase

                If V > 0.0001 And n = 1 Then
                    'the liquid phase cannot be unstable when there's also vapor and only two compounds in the system.
                    Return result
                End If

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = UBound(Vz)

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

                Dim stresult As Object = StabTest(T, P, result(2), PP, Vtrials, Me.StabSearchSeverity)

                If stresult(0) = False Then

                    ' liquid phase NOT stable. proceed to three-phase flash.

                    Dim vx2est(n), fcl(n), fcv(n) As Double
                    Dim m As Double = UBound(stresult(1), 1)
                    Dim gl, gv, gli As Double

                    If StabSearchSeverity = 2 Then
                        gli = 0
                        For j = 0 To m
                            For i = 0 To n
                                vx2est(i) = stresult(1)(j, i)
                            Next
                            fcl = PP.DW_CalcFugCoeff(vx2est, T, P, State.Liquid)
                            gl = 0.0#
                            For i = 0 To n
                                gl += Vz(i) * Log(fcl(i) * Vz(i))
                            Next
                            If gl <= gli Then
                                gli = gl
                                k = j
                            End If
                        Next
                        For i = 0 To UBound(Vz)
                            vx2est(i) = stresult(1)(k, i)
                        Next
                    Else
                        For i = 0 To UBound(Vz)
                            vx2est(i) = stresult(1)(m, i)
                        Next
                    End If

                    fcl = PP.DW_CalcFugCoeff(vx2est, T, P, State.Liquid)
                    fcv = PP.DW_CalcFugCoeff(vx2est, T, P, State.Vapor)

                    gv = 0.0#
                    gl = 0.0#
                    For i = 0 To n
                        gv += vx2est(i) * Log(fcv(i) * vx2est(i))
                        gl += vx2est(i) * Log(fcl(i) * vx2est(i))
                    Next

                    If gl < gv Then 'test phase is liquid-like.

                        Dim vx1e(UBound(Vz)), vx2e(UBound(Vz)) As Double

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

                        For i = 0 To n
                            If i <> imaxl Then
                                vx1e(i) = Vz(i) - V * result(3)(i) - L2 * vx2est(i)
                            Else
                                vx1e(i) = Vz(i) * L2
                            End If
                        Next

                        Dim sumvx2 = 0
                        For i = 0 To n
                            sumvx2 += Abs(vx1e(i))
                        Next

                        For i = 0 To n
                            vx1e(i) = Abs(vx1e(i)) / sumvx2
                        Next

                        result = Flash_PT_3P(Vz, V, L1, L2, Vy, Vx, vx2est, P, T, PP)

                    End If

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [NL3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            Return result

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

        Public Function Flash_PT_3P(ByVal Vz As Double(), ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object

            Dim i, j As Integer

            etol = Convert.ToDouble(PP.Parameters("PP_PTFELT"))
            maxit_e = Convert.ToInt32(PP.Parameters("PP_PTFMEI"))
            itol = Convert.ToDouble(PP.Parameters("PP_PTFILT"))
            maxit_i = Convert.ToInt32(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

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

                Rant = R
                R = Kb * V / (Kb * V + Kb0 * L)

                Dim fr, dfr, R0, R1 As Double
                Dim icount As Integer = 0

                Do
                    R0 = R
                    If R > 0.999 Then
                        R1 = R - 0.01
                        fr = Me.TPErrorFunc(R0)
                        dfr = (fr - Me.TPErrorFunc(R1)) / 0.01
                    Else
                        R1 = R + 0.01
                        fr = Me.TPErrorFunc(R0)
                        dfr = (fr - Me.TPErrorFunc(R1)) / (-0.01)
                    End If
                    R0 = R
                    If (R - fr / dfr) < 0.0# Or (R - fr / dfr) > 1.0# Then
                        If (R + 0.1) < 1.0# Then R += 0.1 Else R -= 0.1
                    Else
                        R = R - fr / dfr
                    End If
                    If R < 0 Then R = 0.0#
                    If R > 1 Then R = 1.0#
                    icount += 1
                Loop Until Abs(fr) < itol Or icount > maxit_i Or R = 0 Or R = 1

                If icount > maxit_i Then R = Rant
                If Rant = 0.0# And R = 1.0# Then R = 0.0#
                If Rant = 1.0# And R = 0.0# Then R = 1.0#

                Me.TPErrorFunc(R)

                'At this point, we have converged R for the simplified model. Proceed to step 9.

                '----------------------------------------------------------
                ' STEP 9 - Rigorous model Enthalpy and K-values calculation
                '----------------------------------------------------------

                Ki1 = PP.DW_CalcKvalue(Vx1, Vy, T, P)
                Ki2 = PP.DW_CalcKvalue(Vx2, Vy, T, P)
                Kb = CalcKbjw(Ki1, Ki2, L1, L2, Vx1, Vx2)

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

                If PP.Parameters("PP_FLASHALGORITHMFASTMODE") = 1 Then

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

                If Double.IsNaN(V) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))
                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

                WriteDebugInfo("PT Flash [IO]: Iteration #" & ecount & ", VF = " & V)

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until AbsSum(fx) < etol

out:
            'order liquid phases by mixture NBP

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

        Private Function TPErrorFunc(ByVal Rt As Double) As Double

            Dim fr, dfr, S0, S1 As Double
            Dim icount As Integer = 0

            S = 1 - Rt
            Do
                S0 = S
                S1 = S + 0.01
                fr = Me.SErrorFunc(S0, Rt)
                dfr = (fr - Me.SErrorFunc(S1, Rt)) / -0.01
                S += -fr / dfr
                If S < -(1 - Rt) Then S = -(1 - Rt) + 0.01
                If S > (1 - Rt) Then S = (1 - Rt) - 0.01
                icount += 1
            Loop Until Abs(fr) < itol Or icount > maxit_i

            If S <= -(1 - Rt) Then S = -(1 - Rt)
            If S >= (1 - Rt) Then S = (1 - Rt)

            For i = 0 To n
                pi(i) = fi(i) / (Rt + (1 - Rt + S) / (2 * Kb0 * Exp(ui1(i))) + (1 - Rt - S) / (2 * Kb0 * Exp(ui2(i))))
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

            If Rt <> 1 Then
                Kb = ((1 - Rt + S) * sumeuipi1 + (1 - Rt - S) * sumeuipi2) / (2 * (1 - Rt) * sumpi)
            Else
                Kb = 1.0#
            End If

            V = Rt * sumpi
            L1 = 0.5 * (S * V * (Kb / Kb0 - 1) + (1 + S) - V)
            L2 = 1 - L1 - V
            beta = L1 / (L1 + L2)

            Dim err1 As Double = Kb - 1

            Return err1

        End Function

        Private Function SErrorFunc(ByVal S0 As Double, ByVal Rt As Double)

            Dim errfunc As Double = 0
            For i = 0 To n
                errfunc += fi(i) * (1 / Exp(ui1(i)) - 1 / Exp(ui2(i))) / (Rt + (1 - Rt + S0) / (2 * Kb0 * Exp(ui1(i))) + (1 - Rt - S0) / (2 * Kb0 * Exp(ui2(i))))
            Next
            Return errfunc

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            n = UBound(Vz)

            proppack = PP
            Hf = H
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Convert.ToInt32(PP.Parameters("PP_PHFMII"))
            Dim maxitEXT As Integer = Convert.ToInt32(PP.Parameters("PP_PHFMEI"))
            Dim tolINT As Double = Convert.ToDouble(PP.Parameters("PP_PHFILT"))
            Dim tolEXT As Double = Convert.ToDouble(PP.Parameters("PP_PHFELT"))

            Dim Tsup, Tinf ', Hsup, Hinf

            If Tref <> 0 Then
                Tinf = Tref - 250
                Tsup = Tref + 250
            Else
                Tinf = 100
                Tsup = 2000
            End If
            If Tinf < 100 Then Tinf = 100

            'Hinf = PP.DW_CalcEnthalpy(Vz, Tinf, P, State.Liquid)
            'Hsup = PP.DW_CalcEnthalpy(Vz, Tsup, P, State.Vapor)

            'If H >= Hsup Then
            '    Tf = Me.ESTIMAR_T_H(H, Tref, "V", P, Vz)
            'ElseIf H <= Hinf Then
            '    Tf = Me.ESTIMAR_T_H(H, Tref, "L", P, Vz)
            'Else
            Dim bo As New BrentOpt.Brent
            bo.DefineFuncDelegate(AddressOf Herror)
            WriteDebugInfo("PH Flash: Starting calculation for " & Tinf & " <= T <= " & Tsup)

            Dim fx, dfdx, x1 As Double

            ecount = 0

            If Tref = 0.0# Then Tref = 298.15
            x1 = Tref
            Do
                fx = Herror(x1, Nothing)
                If Abs(fx) < etol Then Exit Do
                dfdx = (Herror(x1 + 1, Nothing) - fx)
                x1 = x1 - fx / dfdx
                If x1 < 0 Then GoTo alt
                ecount += 1
            Loop Until ecount > maxit_e Or Double.IsNaN(x1)
            If Double.IsNaN(x1) Then
alt:            Tf = bo.BrentOpt(Tinf, Tsup, 4, tolEXT, maxitEXT, Nothing)
            Else
                Tf = x1
            End If

            'End If

            Dim tmp As Object = Flash_PT(Vz, P, Tf, PP)

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)
            L2 = tmp(5)
            Vx2 = tmp(6)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx1(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [NL3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            Return New Object() {L, V, Vx1, Vy, Tf, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            n = UBound(Vz)

            proppack = PP
            Sf = S
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Convert.ToInt32(PP.Parameters("PP_PSFMII"))
            Dim maxitEXT As Integer = Convert.ToInt32(PP.Parameters("PP_PSFMEI"))
            Dim tolINT As Double = Convert.ToDouble(PP.Parameters("PP_PSFILT"))
            Dim tolEXT As Double = Convert.ToDouble(PP.Parameters("PP_PSFELT"))

            Dim Tsup, Tinf ', Ssup, Sinf

            If Tref <> 0 Then
                Tinf = Tref - 200
                Tsup = Tref + 200
            Else
                Tinf = 100
                Tsup = 2000
            End If
            If Tinf < 100 Then Tinf = 100

            'Sinf = PP.DW_CalcEntropy(Vz, Tinf, P, State.Liquid)
            'Ssup = PP.DW_CalcEntropy(Vz, Tsup, P, State.Vapor)

            'If S >= Ssup Then
            '    Tf = Me.ESTIMAR_T_S(S, Tref, "V", P, Vz)
            'ElseIf S <= Sinf Then
            '    Tf = Me.ESTIMAR_T_S(S, Tref, "L", P, Vz)
            'Else
            Dim bo As New BrentOpt.Brent
            bo.DefineFuncDelegate(AddressOf Serror)
            WriteDebugInfo("PS Flash: Starting calculation for " & Tinf & " <= T <= " & Tsup)

            Dim fx, dfdx, x1 As Double

            ecount = 0

            If Tref = 0 Then Tref = 298.15
            x1 = Tref
            Do
                fx = Serror(x1, Nothing)
                If Abs(fx) < etol Then Exit Do
                dfdx = (Serror(x1 + 1, Nothing) - fx)
                x1 = x1 - fx / dfdx
                If x1 < 0 Then GoTo alt
                ecount += 1
            Loop Until ecount > maxit_e Or Double.IsNaN(x1)
            If Double.IsNaN(x1) Then
alt:            Tf = bo.BrentOpt(Tinf, Tsup, 4, tolEXT, maxitEXT, Nothing)
            Else
                Tf = x1
            End If

            'End If

            Dim tmp As Object = Flash_PT(Vz, P, Tf, PP)

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)
            L2 = tmp(5)
            Vx2 = tmp(6)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx1(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [NL3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            Return New Object() {L, V, Vx1, Vy, Tf, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object) As Object

            Dim tmp = Me.Flash_PT(Vz, Pf, T, proppack)

            Dim n = UBound(Vz)

            Dim L1, L2, V, Vx1(), Vx2(), Vy() As Double

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)

            Dim _Hv, _Hl1, _Hl2 As Double

            _Hv = 0
            _Hl1 = 0
            _Hl2 = 0

            If V > 0 Then _Hv = proppack.DW_CalcEnthalpy(Vy, T, Pf, State.Vapor)
            If L1 > 0 Then _Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, Pf, State.Liquid)
            If L2 > 0 Then _Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, Pf, State.Liquid)

            Dim mmg, mml, mml2 As Double
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx1)
            mml2 = proppack.AUX_MMM(Vx2)

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2)) * _Hv - (mml * L1 / (mmg * V + mml * L1 + mml2 * L2)) * _Hl1 - (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2)) * _Hl2
            OBJ_FUNC_PH_FLASH = herr

            WriteDebugInfo("PH Flash [NL3P]: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object) As Object

            Dim tmp = Me.Flash_PT(Vz, Pf, T, proppack)

            Dim n = UBound(Vz)

            Dim L1, L2, V, Vx1(), Vx2(), Vy() As Double

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)

            Dim _Sv, _Sl1, _Sl2 As Double

            _Sv = 0
            _Sl1 = 0
            _Sl2 = 0

            If V > 0 Then _Sv = proppack.DW_CalcEntropy(Vy, T, Pf, State.Vapor)
            If L1 > 0 Then _Sl1 = proppack.DW_CalcEntropy(Vx1, T, Pf, State.Liquid)
            If L2 > 0 Then _Sl2 = proppack.DW_CalcEntropy(Vx2, T, Pf, State.Liquid)

            Dim mmg, mml, mml2
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx1)
            mml2 = proppack.AUX_MMM(Vx2)

            Dim serr As Double = Sf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2)) * _Sv - (mml * L1 / (mmg * V + mml * L1 + mml2 * L2)) * _Sl1 - (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2)) * _Sl2
            OBJ_FUNC_PS_FLASH = serr

            WriteDebugInfo("PS Flash [NL3P]: Current T = " & T & ", Current S Error = " & serr)

        End Function

        Function Herror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PH_FLASH(Tt, Sf, Pf, fi)
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, Pf, fi)
        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i As Integer

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Convert.ToDouble(PP.Parameters("PP_PTFELT"))
            maxit_e = Convert.ToInt32(PP.Parameters("PP_PTFMEI"))
            itol = Convert.ToDouble(PP.Parameters("PP_PTFILT"))
            maxit_i = Convert.ToInt32(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            proppack = PP
            Vf = V
            L = 1 - V
            Lf = 1 - Vf
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n)
            Dim dFdP As Double

            Dim VTc = PP.RET_VTC()

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            If Pref = 0 Then

                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
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

            'Calculate Ki`s

            If Not ReuseKI Then
                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                If Not PP.AUX_CheckTrivial(PrevKi) Then
                    For i = 0 To n
                        Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                        Ki(i) = PrevKi(i)
                    Next
                Else
                    i = 0
                    Do
                        Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                        Ki(i) = Vp(i) / P
                        i += 1
                    Loop Until i = n + 1
                End If
            End If

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                    Vx(i) = Vy(i) / Ki(i)
                Else
                    Vy(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x = 0
            soma_y = 0
            Do
                soma_x = soma_x + Vx(i)
                soma_y = soma_y + Vy(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                Vx(i) = Vx(i) / soma_x
                Vy(i) = Vy(i) / soma_y
                i = i + 1
            Loop Until i = n + 1

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Pant, fval As Double
            Dim chk As Boolean = False

            If V = 1.0# Or V = 0.0# Then

                ecount = 0
                Do

                    marcador3 = 0

                    Dim cont_int = 0
                    Do


                        Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        marcador = 0
                        If stmp4_ant <> 0 Then
                            marcador = 1
                        End If
                        stmp4_ant = stmp4

                        If V = 0 Then
                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Ki(i) * Vx(i)
                                i = i + 1
                            Loop Until i = n + 1
                        Else
                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Vy(i) / Ki(i)
                                i = i + 1
                            Loop Until i = n + 1
                        End If

                        If V = 0 Then
                            i = 0
                            Do
                                Vy_ant(i) = Vy(i)
                                Vy(i) = Ki(i) * Vx(i) / stmp4
                                i = i + 1
                            Loop Until i = n + 1
                        Else
                            i = 0
                            Do
                                Vx_ant(i) = Vx(i)
                                Vx(i) = (Vy(i) / Ki(i)) / stmp4
                                i = i + 1
                            Loop Until i = n + 1
                        End If

                        marcador2 = 0
                        If marcador = 1 Then
                            If V = 0 Then
                                If Math.Abs(Vy(0) - Vy_ant(0)) < itol Then
                                    marcador2 = 1
                                End If
                            Else
                                If Math.Abs(Vx(0) - Vx_ant(0)) < itol Then
                                    marcador2 = 1
                                End If
                            End If
                        End If

                        cont_int = cont_int + 1

                    Loop Until marcador2 = 1 Or Double.IsNaN(stmp4) Or cont_int > maxit_i

                    Dim K1(n), K2(n), dKdP(n) As Double

                    K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                    K2 = PP.DW_CalcKvalue(Vx, Vy, T, P * 1.001)

                    For i = 0 To n
                        dKdP(i) = (K2(i) - K1(i)) / (0.001 * P)
                    Next

                    fval = stmp4 - 1

                    ecount += 1

                    i = 0
                    dFdP = 0
                    Do
                        If V = 0 Then
                            dFdP = dFdP + Vx(i) * dKdP(i)
                        Else
                            dFdP = dFdP - Vy(i) / (Ki(i) ^ 2) * dKdP(i)
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    If (P - fval / dFdP) < 0 Then
                        P = (P + Pant) / 2
                    Else
                        Pant = P
                        P = P - fval / dFdP
                    End If

                    WriteDebugInfo("TV Flash [NL]: Iteration #" & ecount & ", P = " & P & ", VF = " & V)

                    pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Loop Until Math.Abs(P - Pant) < 1 Or Double.IsNaN(P) = True Or ecount > maxit_e Or Double.IsNaN(P) Or Double.IsInfinity(P)

            Else

                ecount = 0

                Do

                    Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                    i = 0
                    Do
                        If Vz(i) <> 0 Then
                            Vy_ant(i) = Vy(i)
                            Vx_ant(i) = Vx(i)
                            Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                            Vx(i) = Vy(i) / Ki(i)
                        Else
                            Vy(i) = 0
                            Vx(i) = 0
                        End If
                        i += 1
                    Loop Until i = n + 1
                    i = 0
                    soma_x = 0
                    soma_y = 0
                    Do
                        soma_x = soma_x + Vx(i)
                        soma_y = soma_y + Vy(i)
                        i = i + 1
                    Loop Until i = n + 1
                    i = 0
                    Do
                        Vx(i) = Vx(i) / soma_x
                        Vy(i) = Vy(i) / soma_y
                        i = i + 1
                    Loop Until i = n + 1

                    If V <= 0.5 Then

                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Ki(i) * Vx(i)
                            i = i + 1
                        Loop Until i = n + 1

                        Dim K1(n), K2(n), dKdP(n) As Double

                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                        K2 = PP.DW_CalcKvalue(Vx, Vy, T, P * 1.001)

                        For i = 0 To n
                            dKdP(i) = (K2(i) - K1(i)) / (0.001 * P)
                        Next

                        i = 0
                        dFdP = 0
                        Do
                            dFdP = dFdP + Vx(i) * dKdP(i)
                            i = i + 1
                        Loop Until i = n + 1

                    Else

                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Vy(i) / Ki(i)
                            i = i + 1
                        Loop Until i = n + 1

                        Dim K1(n), K2(n), dKdP(n) As Double

                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                        K2 = PP.DW_CalcKvalue(Vx, Vy, T, P * 1.001)

                        For i = 0 To n
                            dKdP(i) = (K2(i) - K1(i)) / (0.001 * P)
                        Next

                        i = 0
                        dFdP = 0
                        Do
                            dFdP = dFdP - Vy(i) / (Ki(i) ^ 2) * dKdP(i)
                            i = i + 1
                        Loop Until i = n + 1
                    End If

                    ecount += 1

                    fval = stmp4 - 1

                    If (P - fval / dFdP) < 0 Then
                        P = (P + Pant) / 2
                    Else
                        Pant = P
                        P = P - fval / dFdP
                    End If

                    WriteDebugInfo("TV Flash [NL]: Iteration #" & ecount & ", P = " & P & ", VF = " & V)

                    pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(P) = True Or ecount > maxit_e

            End If

            d2 = Date.Now

            dt = d2 - d1

            If PP.AUX_CheckTrivial(Ki) Then Throw New Exception("TV Flash [NL3P]: Invalid result: converged to the trivial solution (P = " & P & " ).")

            WriteDebugInfo("TV Flash [NL3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            Return New Object() {L, V, Vx, Vy, P, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i As Integer

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Convert.ToDouble(PP.Parameters("PP_PTFELT"))
            maxit_e = Convert.ToInt32(PP.Parameters("PP_PTFMEI"))
            itol = Convert.ToDouble(PP.Parameters("PP_PTFILT"))
            maxit_i = Convert.ToInt32(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            proppack = PP
            Vf = V
            L = 1 - V
            Lf = 1 - Vf
            Tf = T

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n)
            Dim Vt(n), VTc(n), Tmin, Tmax, dFdT As Double

            Vn = PP.RET_VNAMES()
            VTc = PP.RET_VTC()
            fi = Vz.Clone

            If Tref = 0.0# Then

                i = 0
                Tref = 0
                Do
                    Tref += 0.7 * Vz(i) * VTc(i)
                    Tmin += 0.1 * Vz(i) * VTc(i)
                    Tmax += 2.0 * Vz(i) * VTc(i)
                    i += 1
                Loop Until i = n + 1

            Else

                Tmin = Tref - 50
                Tmax = Tref + 50

            End If

            T = Tref

            'Calculate Ki`s

            If Not ReuseKI Then
                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                If Not PP.AUX_CheckTrivial(PrevKi) And Not Double.IsNaN(PrevKi(0)) Then
                    For i = 0 To n
                        Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                        Ki(i) = PrevKi(i)
                    Next
                Else
                    i = 0
                    Do
                        Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                        Ki(i) = Vp(i) / P
                        i += 1
                    Loop Until i = n + 1
                End If
            End If

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                    Vx(i) = Vy(i) / Ki(i)
                Else
                    Vy(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x = 0
            soma_y = 0
            Do
                soma_x = soma_x + Vx(i)
                soma_y = soma_y + Vy(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                Vx(i) = Vx(i) / soma_x
                Vy(i) = Vy(i) / soma_y
                i = i + 1
            Loop Until i = n + 1

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Tant, fval As Double
            Dim chk As Boolean = False

            If V = 1.0# Or V = 0.0# Then

                ecount = 0
                Do

                    marcador3 = 0

                    Dim cont_int = 0
                    Do


                        Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        marcador = 0
                        If stmp4_ant <> 0 Then
                            marcador = 1
                        End If
                        stmp4_ant = stmp4

                        If V = 0 Then
                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Ki(i) * Vx(i)
                                i = i + 1
                            Loop Until i = n + 1
                        Else
                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Vy(i) / Ki(i)
                                i = i + 1
                            Loop Until i = n + 1
                        End If

                        If V = 0 Then
                            i = 0
                            Do
                                Vy_ant(i) = Vy(i)
                                Vy(i) = Ki(i) * Vx(i) / stmp4
                                i = i + 1
                            Loop Until i = n + 1
                        Else
                            i = 0
                            Do
                                Vx_ant(i) = Vx(i)
                                Vx(i) = (Vy(i) / Ki(i)) / stmp4
                                i = i + 1
                            Loop Until i = n + 1
                        End If

                        marcador2 = 0
                        If marcador = 1 Then
                            If V = 0 Then
                                If Math.Abs(Vy(0) - Vy_ant(0)) < itol Then
                                    marcador2 = 1
                                End If
                            Else
                                If Math.Abs(Vx(0) - Vx_ant(0)) < itol Then
                                    marcador2 = 1
                                End If
                            End If
                        End If

                        cont_int = cont_int + 1

                    Loop Until marcador2 = 1 Or Double.IsNaN(stmp4) Or cont_int > maxit_i

                    Dim K1(n), K2(n), dKdT(n) As Double

                    K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                    K2 = PP.DW_CalcKvalue(Vx, Vy, T + 0.1, P)

                    For i = 0 To n
                        dKdT(i) = (K2(i) - K1(i)) / (0.1)
                    Next

                    fval = stmp4 - 1

                    ecount += 1

                    i = 0
                    dFdT = 0
                    Do
                        If V = 0 Then
                            dFdT = dFdT + Vx(i) * dKdT(i)
                        Else
                            dFdT = dFdT - Vy(i) / (Ki(i) ^ 2) * dKdT(i)
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    Tant = T
                    T = T - fval / dFdT
                    'If T < Tmin Then T = Tmin
                    'If T > Tmax Then T = Tmax

                    WriteDebugInfo("PV Flash [NL]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Loop Until Math.Abs(T - Tant) < 0.1 Or Double.IsNaN(T) = True Or ecount > maxit_e Or Double.IsNaN(T) Or Double.IsInfinity(T)

            Else

                ecount = 0

                Do

                    Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                    i = 0
                    Do
                        If Vz(i) <> 0 Then
                            Vy_ant(i) = Vy(i)
                            Vx_ant(i) = Vx(i)
                            Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                            Vx(i) = Vy(i) / Ki(i)
                        Else
                            Vy(i) = 0
                            Vx(i) = 0
                        End If
                        i += 1
                    Loop Until i = n + 1
                    i = 0
                    soma_x = 0
                    soma_y = 0
                    Do
                        soma_x = soma_x + Vx(i)
                        soma_y = soma_y + Vy(i)
                        i = i + 1
                    Loop Until i = n + 1
                    i = 0
                    Do
                        Vx(i) = Vx(i) / soma_x
                        Vy(i) = Vy(i) / soma_y
                        i = i + 1
                    Loop Until i = n + 1

                    If V <= 0.5 Then

                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Ki(i) * Vx(i)
                            i = i + 1
                        Loop Until i = n + 1

                        Dim K1(n), K2(n), dKdT(n) As Double

                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                        K2 = PP.DW_CalcKvalue(Vx, Vy, T + 0.1, P)

                        For i = 0 To n
                            dKdT(i) = (K2(i) - K1(i)) / (0.1)
                        Next

                        i = 0
                        dFdT = 0
                        Do
                            dFdT = dFdT + Vx(i) * dKdT(i)
                            i = i + 1
                        Loop Until i = n + 1

                    Else

                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Vy(i) / Ki(i)
                            i = i + 1
                        Loop Until i = n + 1

                        Dim K1(n), K2(n), dKdT(n) As Double

                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                        K2 = PP.DW_CalcKvalue(Vx, Vy, T + 0.1, P)

                        For i = 0 To n
                            dKdT(i) = (K2(i) - K1(i)) / (0.1)
                        Next

                        i = 0
                        dFdT = 0
                        Do
                            dFdT = dFdT - Vy(i) / (Ki(i) ^ 2) * dKdT(i)
                            i = i + 1
                        Loop Until i = n + 1
                    End If

                    ecount += 1

                    fval = stmp4 - 1

                    Tant = T
                    T = T - fval / dFdT
                    'If T < Tmin Then T = Tmin
                    'If T > Tmax Then T = Tmax

                    WriteDebugInfo("PV Flash [NL]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(T) = True Or ecount > maxit_e

            End If

            d2 = Date.Now

            dt = d2 - d1

            If PP.AUX_CheckTrivial(Ki) Then Throw New Exception("PV Flash [NL3P]: Invalid result: converged to the trivial solution (T = " & T & " ).")

            WriteDebugInfo("PV Flash [NL3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

    End Class

End Namespace
