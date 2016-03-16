'    DWSIM Three-Phase Nested Loops Flash Algorithms
'    Copyright 2014-2015 Daniel Wagner O. de Medeiros
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
Imports DWSIM.DWSIM.SimulationObjects
Imports DWSIM.DWSIM.MathEx
Imports DWSIM.DWSIM.MathEx.Common
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports System.Threading.Tasks

Namespace DWSIM.SimulationObjects.PropertyPackages.Auxiliary.FlashAlgorithms

    ''' <summary>
    ''' The Flash algorithms in this class are based on the Nested Loops approach to solve equilibrium calculations.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class NestedLoops3PV2

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
        Dim proppack As PropertyPackages.PropertyPackage

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, k As Integer

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

            Vpc = PP.RET_VPC
            VTc = PP.RET_VTC
            Vw = PP.RET_VW

            'Calculate Ki`s

            If Not ReuseKI Then
                i = 0
                Do
                    'Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Vp(i) = Vpc(i) * Exp(5.37 * (1 + Vw(i)) * (1 - VTc(i) / T))
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

            If T > DWSIM.MathEx.Common.Max(proppack.RET_VTC, Vz) Then
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
            If Vmax = 0.0# Then Vmax = 1.0#
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

                    Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashError"))

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

                    If Abs(F) < 0.0000000000000001 Then Exit Do

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

                If Double.IsNaN(V) Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashTPVapFracError"))
                If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt2"))

                WriteDebugInfo("PT Flash [NL]: Iteration #" & ecount & ", VF = " & V)

                CheckCalculatorStatus()

            Loop Until convergiu = 1

out:

            Dim result As Object = New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

            ' check if there is a liquid phase

            If L > 0 Then ' we have a liquid phase

                'If V > 0.0001 And n = 1 Then
                '    'the liquid phase cannot be unstable when there's also vapor and only two compounds in the system.
                '    Return result
                'End If

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
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In PP.CurrentMaterialStream.Phases(0).Compounds.Values
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
                    For i = 0 To nc
                        If vx2est(i) <> 0.0# Then gv += vx2est(i) * Log(fcv(i) * vx2est(i))
                        If vx2est(i) <> 0.0# Then gl += vx2est(i) * Log(fcl(i) * vx2est(i))
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

                        Dim sumvx2 As Double = 0
                        For i = 0 To n
                            sumvx2 += Abs(vx1e(i))
                        Next

                        For i = 0 To n
                            vx1e(i) = Abs(vx1e(i)) / sumvx2
                        Next

                        result = Flash_PT_3P(Vz, V, L1, L2, Vy, vx1e, vx2est, P, T, PP)

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

            etol = Convert.ToDouble(PP.Parameters("PP_PTFELT"))
            maxit_e = Convert.ToInt32(PP.Parameters("PP_PTFMEI"))
            itol = Convert.ToDouble(PP.Parameters("PP_PTFILT"))
            maxit_i = Convert.ToInt32(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), ui2(n), uic1(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n)
            Dim b1(n), b2(n), CFL1(n), CFL2(n), CFV(n), Kil(n), L1ant, L2ant As Double
            Dim i As Integer

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

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

            i = 0
            Do
                b1(i) = 1 - Ki1(i) ^ -1
                b2(i) = 1 - Ki2(i) ^ -1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                    Vx1(i) = Vy(i) / Ki1(i)
                    Vx2(i) = Vy(i) / Ki2(i)
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

            Vant = 0.0#
            L1ant = 0.0#
            L2ant = 0.0#

            ecount = 0

            V = Vest
            L1 = L1est
            L2 = L2est

            WriteDebugInfo("PT Flash [NL-3PV2]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", L2 = " & L2)

            Do

                CFL1 = proppack.DW_CalcFugCoeff(Vx1, T, P, State.Liquid)
                CFL2 = proppack.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                CFV = proppack.DW_CalcFugCoeff(Vy, T, P, State.Vapor)

                i = 0
                Do
                    If Vz(i) <> 0 Then Ki1(i) = CFL1(i) / CFV(i)
                    If Vz(i) <> 0 Then Ki2(i) = CFL2(i) / CFV(i)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Dim Vx1ant(n), Vx2ant(n), Vyant(n) As Double
                Do
                    Vx1ant(i) = Vx1(i)
                    Vx2ant(i) = Vx2(i)
                    Vyant(i) = Vy(i)
                    b1(i) = 1 - Ki1(i) ^ -1
                    b2(i) = 1 - Ki2(i) ^ -1
                    Vy(i) = Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                    Vx1(i) = Vy(i) / Ki1(i)
                    Vx2(i) = Vy(i) / Ki2(i)
                    i = i + 1
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

                Dim e1 = 0.0#
                Dim e2 = 0.0#
                Dim e3 = 0.0#
                Dim e4 = 0.0#
                i = 0
                Do
                    e1 = e1 + (Vx1(i) - Vx1ant(i))
                    e4 = e4 + (Vx2(i) - Vx2ant(i))
                    e2 = e2 + (Vy(i) - Vyant(i))
                    i = i + 1
                Loop Until i = n + 1
                e3 = (V - Vant) + (L1 - L1ant) + (L2 - L2ant)

                If (Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e3) + Math.Abs(e2) + Math.Abs(L1ant - L1) + Math.Abs(L2ant - L2)) < etol Then

                    Exit Do

                ElseIf Double.IsNaN(Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e2)) Then

                    Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashTPVapFracError"))

                Else

                    Vant = V
                    Dim F1 = 0.0#, F2 = 0.0#
                    Dim dF1dL1 = 0.0#, dF1dL2 = 0.0#, dF2dL1 = 0.0#, dF2dL2 = 0.0#
                    Dim dL1, dL2 As Double
                    i = 0
                    Do
                        F1 = F1 + b1(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        F2 = F2 + b2(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        dF1dL1 = dF1dL1 + b1(i) * Vz(i) * (-b1(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        dF1dL2 = dF1dL2 + b1(i) * Vz(i) * (-b2(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        dF2dL1 = dF2dL1 + b2(i) * Vz(i) * (-b1(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        dF2dL2 = dF2dL2 + b2(i) * Vz(i) * (-b2(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        i = i + 1
                    Loop Until i = n + 1

                    If Abs(F1) + Abs(F2) < etol Then Exit Do

                    Dim MA As Mapack.Matrix = New Mapack.Matrix(2, 2)
                    Dim MB As Mapack.Matrix = New Mapack.Matrix(2, 1)
                    Dim MX As Mapack.Matrix = New Mapack.Matrix(1, 2)

                    MA(0, 0) = dF1dL1
                    MA(0, 1) = dF1dL2
                    MA(1, 0) = dF2dL1
                    MA(1, 1) = dF2dL2
                    MB(0, 0) = -F1
                    MB(1, 0) = -F2

                    MX = MA.Solve(MB)
                    dL1 = MX(0, 0)
                    dL2 = MX(1, 0)

                    'dL1 = dL1 / (Math.Abs(dL1) + Math.Abs(dL2))
                    'dL2 = dL2 / (Math.Abs(dL1) + Math.Abs(dL2))

                    L2ant = L2
                    L1ant = L1
                    Vant = V

                    L1 += -dL1
                    L2 += -dL2

                    V = 1 - L1 - L2

                    If V <= 0.0# Or Abs(L1) > 1.0# Or Abs(L2) > 1.0# Then
                        'switch to simple LLE flash procedure.
                        Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = Vx1EST, .InitialEstimatesForPhase2 = Vx2EST, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                        Dim result As Object = slle.Flash_PT(Vz, P, T, PP)
                        L1 = result(0)
                        V = result(1)
                        L2 = result(5)
                        Vx1 = result(2)
                        Vy = result(3)
                        Vx2 = result(6)
                        Exit Do
                    ElseIf V > 1.0# Then
                        V = 1.0#
                    End If

                End If

                If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt"))

                ecount += 1

                WriteDebugInfo("PT Flash [NL-3PV2]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", L2 = " & L2)

            Loop

out:
            'order liquid phases by mixture NBP

            Dim VNBP = PP.RET_VTB()
            Dim nbp1 As Double = 0.0#
            Dim nbp2 As Double = 0.0#

            For i = 0 To n
                nbp1 += Vx1(i) * VNBP(i)
                nbp2 += Vx2(i) * VNBP(i)
            Next

            If nbp1 >= nbp2 Then
                Return New Object() {L1, V, Vx1, Vy, ecount, L2, Vx2, 0.0#, PP.RET_NullVector}
            Else
                Return New Object() {L2, V, Vx2, Vy, ecount, L1, Vx1, 0.0#, PP.RET_NullVector}
            End If

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, n, ecount As Integer

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

            Dim Tmin, Tmax, epsilon(4) As Double

            Tmax = 2000.0#
            Tmin = 50.0#

            epsilon(0) = 0.001
            epsilon(1) = 0.01
            epsilon(2) = 0.1
            epsilon(3) = 1
            epsilon(4) = 10

            Dim fx, fx2, dfdx, x1, dx As Double

            Dim cnt As Integer

            If Tref = 0 Then Tref = 298.15

            For j = 0 To 4

                cnt = 0
                x1 = Tref

                Do

                    If My.Settings.EnableParallelProcessing Then
                        My.Application.IsRunningParallelTasks = True
                        If My.Settings.EnableGPUProcessing Then
                            My.Application.gpu.EnableMultithreading()
                        End If
                        Try
                            Dim task1 As Task = New Task(Sub()
                                                             fx = Herror(x1, {P, Vz, PP})
                                                         End Sub)
                            Dim task2 As Task = New Task(Sub()
                                                             fx2 = Herror(x1 + epsilon(j), {P, Vz, PP})
                                                         End Sub)
                            task1.Start()
                            task2.Start()
                            Task.WaitAll(task1, task2)
                        Catch ae As AggregateException
                            Throw ae.Flatten().InnerException
                        Finally
                            If My.Settings.EnableGPUProcessing Then
                                My.Application.gpu.DisableMultithreading()
                                My.Application.gpu.FreeAll()
                            End If
                        End Try
                        My.Application.IsRunningParallelTasks = False
                    Else
                        fx = Herror(x1, {P, Vz, PP})
                        fx2 = Herror(x1 + epsilon(j), {P, Vz, PP})
                    End If

                    If Abs(fx) < tolEXT Then Exit Do

                    dfdx = (fx2 - fx) / epsilon(j)
                    dx = fx / dfdx

                    x1 = x1 - dx

                    cnt += 1

                Loop Until cnt > maxitEXT Or Double.IsNaN(x1)

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > maxitEXT Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            If Double.IsNaN(T) Or cnt > maxitEXT Then

alt:
                Dim bo As New BrentOpt.Brent
                bo.DefineFuncDelegate(AddressOf Herror)
                WriteDebugInfo("PH Flash [NL3PV2]: Newton's method failed. Starting fallback Brent's method calculation for " & Tmin & " <= T <= " & Tmax)

                T = bo.BrentOpt(Tmin, Tmax, 25, tolEXT, maxitEXT, {P, Vz, PP})

            End If

            If T <= Tmin Or T >= Tmax Then Throw New Exception("PH Flash [NL3PV2]: Invalid result: Temperature did not converge.")


            Dim tmp As Object = Flash_PT(Vz, P, T, PP)

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

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, n, ecount As Integer

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

            Dim Tmin, Tmax, epsilon(4) As Double

            Tmax = 2000.0#
            Tmin = 50.0#

            epsilon(0) = 0.001
            epsilon(1) = 0.01
            epsilon(2) = 0.1
            epsilon(3) = 1
            epsilon(4) = 10

            Dim fx, fx2, dfdx, x1, dx As Double

            Dim cnt As Integer

            If Tref = 0 Then Tref = 298.15

            For j = 0 To 4

                cnt = 0
                x1 = Tref

                Do

                    If My.Settings.EnableParallelProcessing Then
                        My.Application.IsRunningParallelTasks = True
                        If My.Settings.EnableGPUProcessing Then
                            My.Application.gpu.EnableMultithreading()
                        End If
                        Try
                            Dim task1 As Task = New Task(Sub()
                                                             fx = Serror(x1, {P, Vz, PP})
                                                         End Sub)
                            Dim task2 As Task = New Task(Sub()
                                                             fx2 = Serror(x1 + epsilon(j), {P, Vz, PP})
                                                         End Sub)
                            task1.Start()
                            task2.Start()
                            Task.WaitAll(task1, task2)
                        Catch ae As AggregateException
                            Throw ae.Flatten().InnerException
                        Finally
                            If My.Settings.EnableGPUProcessing Then
                                My.Application.gpu.DisableMultithreading()
                                My.Application.gpu.FreeAll()
                            End If
                        End Try
                        My.Application.IsRunningParallelTasks = False
                    Else
                        fx = Serror(x1, {P, Vz, PP})
                        fx2 = Serror(x1 + epsilon(j), {P, Vz, PP})
                    End If

                    If Abs(fx) < tolEXT Then Exit Do

                    dfdx = (fx2 - fx) / epsilon(j)
                    dx = fx / dfdx

                    x1 = x1 - dx

                    cnt += 1

                Loop Until cnt > maxitEXT Or Double.IsNaN(x1)

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > maxitEXT Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            If Double.IsNaN(T) Or cnt > maxitEXT Then

alt:
                Dim bo As New BrentOpt.Brent
                bo.DefineFuncDelegate(AddressOf Serror)
                WriteDebugInfo("PS Flash [NL3PV2]: Newton's method failed. Starting fallback Brent's method calculation for " & Tmin & " <= T <= " & Tmax)

                T = bo.BrentOpt(Tmin, Tmax, 25, tolEXT, maxitEXT, {P, Vz, PP})

            End If

            If T <= Tmin Or T >= Tmax Then Throw New Exception("PS Flash [NL3PV2]: Invalid result: Temperature did not converge.")


            Dim tmp As Object = Flash_PT(Vz, P, T, PP)

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

            WriteDebugInfo("PS Flash [NL3PV2]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

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

            _Hv = 0.0#
            _Hl1 = 0.0#
            _Hl2 = 0.0#

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

            _Sv = 0.0#
            _Sl1 = 0.0#
            _Sl2 = 0.0#

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
            CheckCalculatorStatus()
            Return OBJ_FUNC_PH_FLASH(Tt, Sf, Pf, fi)
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            CheckCalculatorStatus()
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, Pf, fi)
        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, k As Integer

            d1 = Date.Now

            Dim _nl As New DWSIMDefault

            Dim result As Object = _nl.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)

            P = result(4)

            If result(0) > 0 Then

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = UBound(Vz)

                If nt = -1 Then nt = nc
                n = nc

                Dim Vtrials(nt, nc) As Double
                Dim idx(nt) As Integer

                For i = 0 To nt
                    If Me.StabSearchCompIDs.Length = 0 Then
                        idx(i) = i
                    Else
                        j = 0
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In PP.CurrentMaterialStream.Phases(0).Compounds.Values
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

                Dim stresult As Object = StabTest(T, P, result(2), PP, Vtrials, Me.StabSearchSeverity)

                If stresult(0) = False Then

                    Dim vx2est(n), fcl(n), fcv(n) As Double
                    Dim m As Double = UBound(stresult(1), 1)
                    Dim gl, gv, gli As Double

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
                    For i = 0 To nc
                        If vx2est(i) <> 0.0# Then gv += vx2est(i) * Log(fcv(i) * vx2est(i))
                        If vx2est(i) <> 0.0# Then gl += vx2est(i) * Log(fcl(i) * vx2est(i))
                    Next

                    If gl < gv Then 'liquid-like

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

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("TV Flash [NL-3PV2]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, k As Integer

            d1 = Date.Now

            Dim _nl As New DWSIMDefault

            Dim result As Object = _nl.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)

            T = result(4)

            If result(0) > 0 Then

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = UBound(Vz)

                If nt = -1 Then nt = nc
                n = nc

                Dim Vtrials(nt, nc) As Double
                Dim idx(nt) As Integer

                For i = 0 To nt
                    If Me.StabSearchCompIDs.Length = 0 Then
                        idx(i) = i
                    Else
                        j = 0
                        For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In PP.CurrentMaterialStream.Phases(0).Compounds.Values
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

                Dim stresult As Object = StabTest(T, P, result(2), PP, Vtrials, Me.StabSearchSeverity)

                If stresult(0) = False Then

                    Dim vx2est(n), fcl(nc), fcv(n) As Double
                    Dim m As Double = UBound(stresult(1), 1)
                    Dim gl, gv, gli As Double

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
                    For i = 0 To nc
                        If vx2est(i) <> 0.0# Then gv += vx2est(i) * Log(fcv(i) * vx2est(i))
                        If vx2est(i) <> 0.0# Then gl += vx2est(i) * Log(fcl(i) * vx2est(i))
                    Next

                    If gl < gv Then 'liquid-like

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

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PV Flash [NL-3PV2]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Function Flash_PV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim i As Integer

            etol = Convert.ToDouble(PP.Parameters("PP_PTFELT"))
            maxit_e = Convert.ToInt32(PP.Parameters("PP_PTFMEI"))
            itol = Convert.ToDouble(PP.Parameters("PP_PTFILT"))
            maxit_i = Convert.ToInt32(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), ui2(n), uic1(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n)
            Dim b1(n), b2(n), CFL1(n), CFL2(n), CFV(n), db1dT(n), db2dT(n), Kil(n), Tant, L1ant, L2ant, Ki12(n), Ki22(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            'Calculate Ki`s

            Tant = Tref
            T = Tref

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

            i = 0
            Do
                b1(i) = 1 - Ki1(i) ^ -1
                b2(i) = 1 - Ki2(i) ^ -1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                    Vx1(i) = Vy(i) / Ki1(i)
                    Vx2(i) = Vy(i) / Ki2(i)
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

            Vant = 0.0#
            L1ant = 0.0#
            L2ant = 0.0#

            ecount = 0

            L1 = L1est
            L2 = L2est

            WriteDebugInfo("PV Flash [NL-3PV2]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", T = " & T)

            Do

                CFL1 = proppack.DW_CalcFugCoeff(Vx1, T, P, State.Liquid)
                CFL2 = proppack.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                CFV = proppack.DW_CalcFugCoeff(Vy, T, P, State.Vapor)

                i = 0
                Do
                    If Vz(i) <> 0 Then Ki1(i) = CFL1(i) / CFV(i)
                    If Vz(i) <> 0 Then Ki2(i) = CFL2(i) / CFV(i)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Dim Vx1ant(n), Vx2ant(n), Vyant(n) As Double
                Do
                    Vx1ant(i) = Vx1(i)
                    Vx2ant(i) = Vx2(i)
                    Vyant(i) = Vy(i)
                    b1(i) = 1 - Ki1(i) ^ -1
                    b2(i) = 1 - Ki2(i) ^ -1
                    Vy(i) = Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                    Vx1(i) = Vy(i) / Ki1(i)
                    Vx2(i) = Vy(i) / Ki2(i)
                    i = i + 1
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

                Dim e1 = 0.0#
                Dim e2 = 0.0#
                Dim e3 = 0.0#
                Dim e4 = 0.0#
                i = 0
                Do
                    e1 = e1 + (Vx1(i) - Vx1ant(i))
                    e4 = e4 + (Vx2(i) - Vx2ant(i))
                    e2 = e2 + (Vy(i) - Vyant(i))
                    i = i + 1
                Loop Until i = n + 1
                e3 = (T - Tant) + (L1 - L1ant) + (L2 - L2ant)

                If (Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e3) + Math.Abs(e2) + Math.Abs(L1ant - L1) + Math.Abs(L2ant - L2)) < etol Then

                    Exit Do

                ElseIf Double.IsNaN(Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e2)) Then

                    Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashTPVapFracError"))

                Else

                    Ki12 = PP.DW_CalcKvalue(Vx1, Vy, T + 2, P)
                    Ki22 = PP.DW_CalcKvalue(Vx2, Vy, T + 2, P)

                    For i = 0 To n
                        db1dT(i) = ((1 - Ki12(i) ^ -1) - (1 - Ki1(i) ^ -1)) / 2
                        db2dT(i) = ((1 - Ki22(i) ^ -1) - (1 - Ki2(i) ^ -1)) / 2
                    Next

                    Dim F1 = 0.0#, F2 = 0.0#
                    Dim dF1dT = 0.0#, dF1dL2 = 0.0#, dF2dT = 0.0#, dF2dL2 = 0.0#, dF1db1(n), dF1db2(n), dF2db1(n), dF2db2(n) As Double
                    Dim dT, dL2 As Double
                    i = 0
                    Do
                        F1 = F1 + b1(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        F2 = F2 + b2(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        dF1db1(i) = -Vz(i) * (b2(i) * L2) / (b1(i) * L1 + b2(i) * L2 - 1) ^ 2
                        dF1db2(i) = b1(i) * Vz(i) * L2 / (b1(i) * L1 + b2(i) * L2 - 1) ^ 2
                        dF2db1(i) = b2(i) * Vz(i) * L1 / (b2(i) * L2 + b1(i) * L1 - 1) ^ 2
                        dF2db2(i) = -Vz(i) * (b1(i) * L1) / (b2(i) * L2 + b1(i) * L1 - 1) ^ 2
                        dF1dL2 = dF1dL2 + b1(i) * Vz(i) * (-b2(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        dF2dL2 = dF2dL2 + b2(i) * Vz(i) * (-b2(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        dF1dT = dF1dT + dF1db1(i) * db1dT(i) + dF1db2(i) * db2dT(i)
                        dF2dT = dF2dT + dF2db1(i) * db1dT(i) + dF2db2(i) * db2dT(i)
                        i = i + 1
                    Loop Until i = n + 1

                    If Abs(F1) + Abs(F2) < etol Then Exit Do

                    Dim MA As Mapack.Matrix = New Mapack.Matrix(2, 2)
                    Dim MB As Mapack.Matrix = New Mapack.Matrix(2, 1)
                    Dim MX As Mapack.Matrix = New Mapack.Matrix(1, 2)

                    MA(0, 0) = dF1dT
                    MA(0, 1) = dF1dL2
                    MA(1, 0) = dF2dT
                    MA(1, 1) = dF2dL2
                    MB(0, 0) = -F1
                    MB(1, 0) = -F2

                    MX = MA.Solve(MB)
                    dT = MX(0, 0)
                    dL2 = MX(1, 0)

                    L2ant = L2
                    L1ant = L1
                    Tant = T

                    L2 += -dL2 * 0.3

                    If L2 < 0.0# Then L2 = 0.0#
                    If L2 > 1.0# - V Then L2 = 1.0# - V

                    L1 = 1 - V - L2

                    If V = 0.0# Then
                        'switch to simple LLE flash procedure.
                        Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = Vx1EST, .InitialEstimatesForPhase2 = Vx2EST, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                        Dim result As Object = slle.Flash_PT(Vz, P, T, PP)
                        L1 = result(0)
                        'V = result(1)
                        L2 = result(5)
                        Vx1 = result(2)
                        'Vy = result(3)
                        Vx2 = result(6)
                        Exit Do
                    End If

                    T += -dT * 0.3

                End If

                If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt"))

                ecount += 1

                WriteDebugInfo("PV Flash [NL-3PV2]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", T = " & T)

            Loop

out:        Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Function Flash_TV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackage) As Object

            Dim i As Integer

            etol = Convert.ToDouble(PP.Parameters("PP_PTFELT"))
            maxit_e = Convert.ToInt32(PP.Parameters("PP_PTFMEI"))
            itol = Convert.ToDouble(PP.Parameters("PP_PTFILT"))
            maxit_i = Convert.ToInt32(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), ui2(n), uic1(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n)
            Dim b1(n), b2(n), CFL1(n), CFL2(n), CFV(n), db1dP(n), db2dP(n), Kil(n), Pant, L1ant, L2ant, Ki12(n), Ki22(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            'Calculate Ki`s

            Pant = Pref
            P = Pref

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

            i = 0
            Do
                b1(i) = 1 - Ki1(i) ^ -1
                b2(i) = 1 - Ki2(i) ^ -1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                    Vx1(i) = Vy(i) / Ki1(i)
                    Vx2(i) = Vy(i) / Ki2(i)
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

            Vant = 0.0#
            L1ant = 0.0#
            L2ant = 0.0#

            ecount = 0

            L1 = L1est
            L2 = L2est

            WriteDebugInfo("TV Flash [NL-3PV2]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", P = " & P)

            Do

                CFL1 = proppack.DW_CalcFugCoeff(Vx1, T, P, State.Liquid)
                CFL2 = proppack.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                CFV = proppack.DW_CalcFugCoeff(Vy, T, P, State.Vapor)

                i = 0
                Do
                    If Vz(i) <> 0 Then Ki1(i) = CFL1(i) / CFV(i)
                    If Vz(i) <> 0 Then Ki2(i) = CFL2(i) / CFV(i)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Dim Vx1ant(n), Vx2ant(n), Vyant(n) As Double
                Do
                    Vx1ant(i) = Vx1(i)
                    Vx2ant(i) = Vx2(i)
                    Vyant(i) = Vy(i)
                    b1(i) = 1 - Ki1(i) ^ -1
                    b2(i) = 1 - Ki2(i) ^ -1
                    Vy(i) = Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                    Vx1(i) = Vy(i) / Ki1(i)
                    Vx2(i) = Vy(i) / Ki2(i)
                    i = i + 1
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

                Dim e1 = 0.0#
                Dim e2 = 0.0#
                Dim e3 = 0.0#
                Dim e4 = 0.0#
                i = 0
                Do
                    e1 = e1 + (Vx1(i) - Vx1ant(i))
                    e4 = e4 + (Vx2(i) - Vx2ant(i))
                    e2 = e2 + (Vy(i) - Vyant(i))
                    i = i + 1
                Loop Until i = n + 1
                e3 = (T - Tant) + (L1 - L1ant) + (L2 - L2ant)

                If (Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e3) + Math.Abs(e2) + Math.Abs(L1ant - L1) + Math.Abs(L2ant - L2)) < etol Then

                    Exit Do

                ElseIf Double.IsNaN(Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e2)) Then

                    Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashTPVapFracError"))

                Else

                    Ki12 = PP.DW_CalcKvalue(Vx1, Vy, T, P + 100)
                    Ki22 = PP.DW_CalcKvalue(Vx2, Vy, T, P + 100)

                    For i = 0 To n
                        db1dP(i) = ((1 - Ki12(i) ^ -1) - (1 - Ki1(i) ^ -1)) / 100
                        db2dP(i) = ((1 - Ki22(i) ^ -1) - (1 - Ki2(i) ^ -1)) / 100
                    Next

                    Dim F1 = 0.0#, F2 = 0.0#
                    Dim dF1dP = 0.0#, dF1dL2 = 0.0#, dF2dP = 0.0#, dF2dL2 = 0.0#, dF1db1(n), dF1db2(n), dF2db1(n), dF2db2(n) As Double
                    Dim dP, dL2 As Double
                    i = 0
                    Do
                        F1 = F1 + b1(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        F2 = F2 + b2(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        dF1db1(i) = -Vz(i) * (b2(i) * L2) / (b1(i) * L1 + b2(i) * L2 - 1) ^ 2
                        dF1db2(i) = b1(i) * Vz(i) * L2 / (b1(i) * L1 + b2(i) * L2 - 1) ^ 2
                        dF2db1(i) = b2(i) * Vz(i) * L1 / (b2(i) * L2 + b1(i) * L1 - 1) ^ 2
                        dF2db2(i) = -Vz(i) * (b1(i) * L1) / (b2(i) * L2 + b1(i) * L1 - 1) ^ 2
                        dF1dL2 = dF1dL2 + b1(i) * Vz(i) * (-b2(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        dF2dL2 = dF2dL2 + b2(i) * Vz(i) * (-b2(i)) / (1 - b1(i) * L1 - b2(i) * L2) ^ 2
                        dF1dP = dF1dP + dF1db1(i) * db1dP(i) + dF1db2(i) * db2dP(i)
                        dF2dP = dF2dP + dF2db1(i) * db1dP(i) + dF2db2(i) * db2dP(i)
                        i = i + 1
                    Loop Until i = n + 1

                    If Abs(F1) + Abs(F2) < etol Then Exit Do

                    Dim MA As Mapack.Matrix = New Mapack.Matrix(2, 2)
                    Dim MB As Mapack.Matrix = New Mapack.Matrix(2, 1)
                    Dim MX As Mapack.Matrix = New Mapack.Matrix(1, 2)

                    MA(0, 0) = dF1dP
                    MA(0, 1) = dF1dL2
                    MA(1, 0) = dF2dP
                    MA(1, 1) = dF2dL2
                    MB(0, 0) = -F1
                    MB(1, 0) = -F2

                    MX = MA.Solve(MB)
                    dP = MX(0, 0)
                    dL2 = MX(1, 0)

                    L2ant = L2
                    L1ant = L1
                    Pant = P

                    P += -dP * 0.3
                    L2 += -dL2 * 0.3

                    If L2 < 0.0# Then L2 = 0.0#
                    If L2 > 1.0# - V Then L2 = 1.0# - V

                    L1 = 1 - V - L2

                    If V = 0.0# Then
                        'switch to simple LLE flash procedure.
                        Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = Vx1EST, .InitialEstimatesForPhase2 = Vx2EST, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                        Dim result As Object = slle.Flash_PT(Vz, P, T, PP)
                        L1 = result(0)
                        'V = result(1)
                        L2 = result(5)
                        Vx1 = result(2)
                        'Vy = result(3)
                        Vx2 = result(6)
                        Exit Do
                    End If

                End If

                If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt"))

                ecount += 1

                WriteDebugInfo("TV Flash [NL-3PV2]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", P = " & P)

            Loop

out:        Return New Object() {L1, V, Vx1, Vy, P, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

    End Class

End Namespace
