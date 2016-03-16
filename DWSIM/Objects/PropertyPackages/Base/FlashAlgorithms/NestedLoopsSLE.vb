'    DWSIM Nested Loops Flash Algorithms for Solid-Liquid Equilibria (SLE)
'    Copyright 2013 Daniel Wagner O. de Medeiros
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
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports System.Linq

Namespace DWSIM.SimulationObjects.PropertyPackages.Auxiliary.FlashAlgorithms

    ''' <summary>
    ''' The Flash algorithms in this class are based on the Nested Loops approach to solve equilibrium calculations.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class NestedLoopsSLE

        Inherits FlashAlgorithm

        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Hv0, Hvid, Hlid, Hf, Hv, Hl, Hs As Double
        Dim Sv0, Svid, Slid, Sf, Sv, Sl, Ss As Double

        Public Property CompoundProperties As List(Of ConstantProperties)

        Public Property SolidSolution As Boolean = False

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            If SolidSolution Then
                Return Flash_PT_SS(Vz, P, T, PP, ReuseKI, PrevKi)
            Else
                'Return Flash_PT_E(Vz, P, T, PP, ReuseKI, PrevKi)
                Return Flash_PT_NL(Vz, P, T, PP, ReuseKI, PrevKi)
            End If

        End Function

        Public Function Flash_PT_SS(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount As Integer
            Dim soma_x, soma_y, soma_s As Double
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, S, Lant, V As Double

            Dim ids As New List(Of String)

            d1 = Date.Now

            etol = CDbl(PP.Parameters("PP_PTFELT"))
            maxit_e = CInt(PP.Parameters("PP_PTFMEI"))
            itol = CDbl(PP.Parameters("PP_PTFILT"))
            maxit_i = CInt(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            Dim Vn(n) As String, Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki_ant(n), fi(n), Vs(n), Vs_ant(n), activcoeff(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            'Calculate Ki`s

            i = 0
            Do
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
                If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                    Ki(i) = Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                Else
                    Ki(i) = 1.0E+20
                End If
                i += 1
            Loop Until i = n + 1

            V = 0.0#
            L = 1.0#
            S = 0.0#

            i = 0
            Do
                If Vz(i) <> 0.0# Then
                    Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                    If Ki(i) <> 0 Then Vs(i) = Vx(i) / Ki(i) Else Vs(i) = Vz(i)
                    If Vs(i) < 0 Then Vs(i) = 0
                    If Vx(i) < 0 Then Vx(i) = 0
                Else
                    Vs(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x = 0
            soma_s = 0
            soma_y = 0.0#
            Do
                soma_x = soma_x + Vx(i)
                soma_s = soma_s + Vs(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                Vx(i) = Vx(i) / soma_x
                Vs(i) = Vs(i) / soma_s
                i = i + 1
            Loop Until i = n + 1

            ecount = 0
            Dim convergiu = 0
            Dim F = 0.0#

            Do

                Ki_ant = Ki.Clone

                activcoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid)

                For i = 0 To n
                    If Not CompoundProperties(i).IsSalt Then activcoeff(i) = activcoeff(i) * P / Vp(i)
                    If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                        Ki(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                    Else
                        Ki(i) = 1.0E+20
                    End If
                Next

                i = 0
                Do
                    If Vz(i) <> 0 Then
                        Vs_ant(i) = Vs(i)
                        Vx_ant(i) = Vx(i)
                        Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                        Vs(i) = Vx(i) / Ki(i)
                    Else
                        Vy(i) = 0
                        Vx(i) = 0
                    End If
                    i += 1
                Loop Until i = n + 1

                i = 0
                soma_x = 0
                soma_s = 0
                Do
                    soma_x = soma_x + Vx(i)
                    soma_s = soma_s + Vs(i)
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    Vx(i) = Vx(i) / soma_x
                    Vs(i) = Vs(i) / soma_s
                    i = i + 1
                Loop Until i = n + 1

                Dim e1 As Double = 0
                Dim e2 As Double = 0
                Dim e3 As Double = 0
                i = 0
                Do
                    e1 = e1 + (Vx(i) - Vx_ant(i))
                    e2 = e2 + (Vs(i) - Vs_ant(i))
                    i = i + 1
                Loop Until i = n + 1

                e3 = (L - Lant)

                If Double.IsNaN(Math.Abs(e1) + Math.Abs(e2)) Then

                    Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashError"))

                ElseIf Math.Abs(e3) < 0.0000000001 And ecount > 0 Then

                    convergiu = 1

                    Exit Do

                Else

                    Lant = L

                    F = 0.0#
                    Dim dF = 0.0#
                    i = 0
                    Do
                        If Vz(i) > 0 Then
                            F = F + Vz(i) * (Ki(i) - 1) / (1 + L * (Ki(i) - 1))
                            dF = dF - Vz(i) * (Ki(i) - 1) ^ 2 / (1 + L * (Ki(i) - 1)) ^ 2
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    If Abs(F) < 0.000001 Then Exit Do

                    L = -0.7 * F / dF + L

                End If

                S = 1 - L

                If L > 1 Then
                    L = 1
                    S = 0
                    i = 0
                    Do
                        Vx(i) = Vz(i)
                        i = i + 1
                    Loop Until i = n + 1
                ElseIf L < 0 Then
                    L = 0
                    S = 1
                    i = 0
                    Do
                        Vs(i) = Vz(i)
                        i = i + 1
                    Loop Until i = n + 1
                End If

                ecount += 1

                If Double.IsNaN(L) Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashTPVapFracError"))
                If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt2"))

                WriteDebugInfo("PT Flash [NL-SLE]: Iteration #" & ecount & ", LF = " & L)

                CheckCalculatorStatus()

            Loop Until convergiu = 1

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & F)

out:        Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Function Flash_SL(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage) As FlashResult
            'This flash is used to calculate the solid/liquid equilibrium at given pressure and temperature
            'Input parameters:  global mole fractions Vz
            'Result Parameters: number of moles in each phase

            Dim Result As New FlashResult
            Dim MaxError As Double = 0.0000001

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date

            d1 = Date.Now
            n = UBound(Vz)

            Dim Vx(n), Vs(n), MaxAct(n), MaxX(n), MaxLiquPhase(n), Tf(n), Hf(n), Tc(n), ActCoeff(n), VnL(n), VnS(n), Vp(n) As Double
            Dim L, L_old, SF, SLP As Double
            Dim cpl(n), cps(n), dCp(n) As Double
            Dim Vn(n) As String
            Dim constprop As ConstantProperties

            Vx = Vz.Clone 'assuming initially only liquids exist
            Tf = PP.RET_VTF 'Fusion temperature
            Hf = PP.RET_VHF 'Enthalpy of fusion
            Tc = PP.RET_VTC 'Critical Temperature

            If Vz.MaxY = 1 Then 'only a single component
                ecount = 0
                For i = 0 To n
                    If Vz(i) = 1 Then
                        If T > Tf(i) Then
                            'above melting temperature, only liquid
                            L = 1
                            L_old = L
                            Vx = Vz.Clone
                            Vs = PP.RET_NullVector
                            GoTo out
                        Else
                            'below melting temperature, only solid
                            L = 0
                            L_old = L
                            Vs = Vz.Clone
                            Vx = PP.RET_NullVector
                            GoTo out
                        End If
                    End If
                Next
            End If

            Vn = PP.RET_VNAMES()
            For i = 0 To n
                constprop = PP.CurrentMaterialStream.Fases(0).Componentes(Vn(i)).ConstantProperties
                cpl(i) = PP.AUX_LIQ_Cpi(constprop, Tf(i))
                cps(i) = PP.AUX_SolidHeatCapacity(constprop, Tf(i))
                dCp(i) = (cpl(i) - cps(i)) * constprop.Molar_Weight
            Next

            'Calculate max activities for solubility of solids
            For i = 0 To n
                MaxAct(i) = Exp(-Hf(i) * 1000 / 8.31446 / T * (1 - T / Tf(i)) - dCp(i) / 8.31446 * ((T - Tf(i)) / T + Log(Tf(i) / T)))
                Vp(i) = PP.AUX_PVAPi(i, T)
            Next

            L = 1
            Do
                ecount += 1

                ActCoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid).MultiplyConstY(P).DivideY(Vp)
                MaxX = MaxAct.DivideY(ActCoeff)
                For i = 0 To n
                    If T > Tc(i) Then MaxX(i) = 1 'Supercritical gases are put to liquid phase
                Next

                MaxLiquPhase = Vz.DivideY(MaxX)
                SF = 0
                For i = 0 To n
                    If MaxLiquPhase(i) > 0.0001 Then SF += MaxX(i)
                Next
                If SF < 1 Then
                    'only solid remaining
                    Vx = PP.RET_NullVector
                    Vs = Vz.Clone
                    L = 0
                    L_old = 0
                    Exit Do
                End If

                VnL = PP.RET_NullVector
                VnS = PP.RET_NullVector
                Vx = PP.RET_NullVector

                L_old = L

                For i = 0 To n
                    If Vz(i) > MaxX(i) Then
                        Vx(i) = MaxX(i) 'Component fraction above max solubility. -> fix fraction to max solubility
                    Else
                        VnL(i) = Vz(i) 'Component fraction below max solubility. -> put to liquid completely
                    End If
                Next

                SLP = VnL.SumY 'Sum moles of components in liquid phase
                SF = Vx.SumY 'Sum mole fractions of components fixed by max solubility
                L = SLP / (1 - SF)

                If L >= 1 Then
                    'all components are below max solubility, only liquid left
                    Vx = Vz.Clone
                    Vs = PP.RET_NullVector
                    Exit Do
                End If

                'calculate moles in liquid phase of components above max solubility
                For i = 0 To n
                    If Vz(i) > MaxX(i) Then
                        VnL(i) = MaxX(i) * L
                    End If
                    VnS(i) = Vz(i) - VnL(i)
                Next

                If L > 0 And MaxX.SumY > 1 Then
                    Vx = VnL.NormalizeY
                    Vs = VnS.NormalizeY
                Else
                    'only solid remaining
                    Vx = PP.RET_NullVector
                    Vs = Vz.Clone
                    L = 0
                    L_old = 0
                    Exit Do
                End If


            Loop Until Abs(L - L_old) < MaxError

out:        d2 = Date.Now
            With Result
                .LF = L
                .SF = 1 - L
                .VF = 0
                .Vx = Vx
                .Vs = Vs
                .Err = L - L_old
                .Counter = ecount
                .dT = d2 - d1
            End With

            Return Result
        End Function

        Public Function Flash_PT_NL(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount, gcount As Integer
            Dim Pb, Pd, Pmin, Pmax, Px As Double
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, S, Vant As Double
            Dim GL_old, GS_old, GV_old As Double
            Dim GlobalConv As Boolean = False

            d1 = Date.Now

            etol = CDbl(PP.Parameters("PP_PTFELT"))
            maxit_e = CInt(PP.Parameters("PP_PTFMEI"))
            itol = CDbl(PP.Parameters("PP_PTFILT"))
            maxit_i = CInt(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            Dim Vx(n), Vy(n), Vs(n), Vmix(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki_ant(n) As Double

            'Calculate Ki`s
            If Not ReuseKI Then
                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(i, T)
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                For i = 0 To n
                    Vp(i) = PP.AUX_PVAPi(i, T)
                    Ki(i) = PrevKi(i)
                Next
            End If

            'initially put all into liquid phase
            Vx = Vz.Clone
            S = 0
            L = 1
            V = 0

            'do flash calculation iterations
            Do
                GL_old = L
                GV_old = V
                GS_old = S
                gcount += 1

                If V < 1 Then
                    'there is some liquid or solid

                    '================================================
                    '== mix solid and liquid phase ==================
                    '================================================
                    Vmix = Vs.MultiplyConstY(S)
                    Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                    Vz = Vmix.NormalizeY

                    '================================================
                    '== Do initial SLE flash to precipitate solids ==
                    '================================================
                    Dim SL_Result As FlashResult
                    SL_Result = Flash_SL(Vz, P, T, PP)
                    Vx = SL_Result.Vx
                    Vs = SL_Result.Vs

                    'calculate global phase fractions
                    L = SL_Result.LF * (1 - V)
                    S = SL_Result.SF * (1 - V)
                End If

                'only solids or vapour left
                If L = 0 Then GoTo out2

                '================================================
                '== mix vapour and liquid phase =================
                '================================================
                Vmix = Vy.MultiplyConstY(V)
                Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                Vz = Vmix.NormalizeY

                '================================================
                '== Do VLE flash ================================
                '================================================

                'Estimate V
                If T > DWSIM.MathEx.Common.Max(PP.RET_VTC, Vz) Then
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
                Pmax = SumY(Vz.MultiplyY(Vp))
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

                V = Vmin + (Vmax - Vmin) / 2
                'V = (P - Pd) / (Pb - Pd)

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
                        If Ki(i) <> 0 Then Vx(i) = Vy(i) / Ki(i) Else Vx(i) = Vz(i)
                        If Vy(i) < 0 Then Vy(i) = 0
                        If Vx(i) < 0 Then Vx(i) = 0
                    Else
                        Vy(i) = 0
                        Vx(i) = 0
                    End If
                    i += 1
                Loop Until i = n + 1

                Vy_ant = Vy.Clone
                Vx_ant = Vx.Clone

                Vy = Vz.MultiplyY(Ki).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))
                For i = 0 To n
                    If Double.IsNaN(Vy(i)) Then Vy(i) = 0
                Next
                Vx = Vy.DivideY(Ki)

                Vx = Vx.NormalizeY
                Vy = Vy.NormalizeY

                Dim convergiu As Integer = 0
                Dim F, dF, e1, e2, e3 As Double
                ecount = 0

                Do

                    Ki_ant = Ki.Clone
                    Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                    'i = 0
                    'Do
                    '    If Vz(i) <> 0 Then
                    '        Vy_ant(i) = Vy(i)
                    '        Vx_ant(i) = Vx(i)
                    '        Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                    '        Vx(i) = Vy(i) / Ki(i)
                    '    Else
                    '        Vy(i) = 0
                    '        Vx(i) = 0
                    '    End If
                    '    i += 1
                    'Loop Until i = n + 1

                    Vy_ant = Vy.Clone
                    Vx_ant = Vx.Clone

                    Vy = Vz.MultiplyY(Ki).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))
                    For i = 0 To n
                        If Double.IsNaN(Vy(i)) Then Vy(i) = 0
                    Next
                    Vx = Vy.DivideY(Ki)

                    Vx = Vx.NormalizeY
                    Vy = Vy.NormalizeY

                    e1 = Vx.SubtractY(Vx_ant).AbsSumY
                    e2 = Vy.SubtractY(Vy_ant).AbsSumY

                    e3 = (V - Vant)

                    If Double.IsNaN(e1 + e2) Then

                        Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashError"))

                    ElseIf Math.Abs(e3) < 0.0000000001 And ecount > 0 Then

                        convergiu = 1

                        Exit Do

                    Else

                        Vant = V

                        F = 0.0#
                        dF = 0.0#
                        i = 0
                        Do
                            If Vz(i) > 0 Then
                                F = F + Vz(i) * (Ki(i) - 1) / (1 + V * (Ki(i) - 1))
                                dF = dF - Vz(i) * (Ki(i) - 1) ^ 2 / (1 + V * (Ki(i) - 1)) ^ 2
                            End If
                            i = i + 1
                        Loop Until i = n + 1

                        'F = Vz.MultiplyY(Ki.AddConstY(-1).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))).SumY
                        'dF = Vz.NegateY.MultiplyY(Ki.AddConstY(-1).MultiplyY(Ki.AddConstY(-1)).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1)).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))).SumY

                        If Abs(F) < etol / 100 Then Exit Do

                        V = -F / dF + Vant

                        'If V >= 1.01 Or V <= -0.01 Then V = -0.1 * F / dF + Vant

                    End If

                    If V < 0.0# Then V = 0.0#
                    If V > 1.0# Then V = 1.0#

                    L = 1 - V

                    ecount += 1

                    If Double.IsNaN(V) Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashTPVapFracError"))
                    If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt2"))


                    WriteDebugInfo("PT Flash [NL-SLE]: Iteration #" & ecount & ", VF = " & V)

                    CheckCalculatorStatus()

                Loop Until convergiu = 1

out:            'calculate global phase fractions
                L = L * (1 - S)
                V = V * (1 - S)

                If gcount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt2"))

out2:           If (Math.Abs(GL_old - L) < 0.0000005) And (Math.Abs(GV_old - V) < 0.0000005) And (Math.Abs(GS_old - S) < 0.0000005) Then GlobalConv = True


            Loop Until GlobalConv

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds)

            Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Function Flash_PT_E(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = CDbl(PP.Parameters("PP_PTFELT"))
            maxit_e = CInt(PP.Parameters("PP_PTFMEI"))
            itol = CDbl(PP.Parameters("PP_PTFILT"))
            maxit_i = CInt(PP.Parameters("PP_PTFMII"))

            'This flash algorithm is for Electrolye/Salt systems with Water as the single solvent.
            'The vapor and solid phases are considered to be ideal.
            'Chemical equilibria is calculated using the reactions enabled in the default reaction set.

            Dim n As Integer = CompoundProperties.Count - 1
            Dim activcoeff(n) As Double
            Dim i As Integer

            'Vnf = feed molar amounts (considering 1 mol of feed)
            'Vnl = liquid phase molar amounts
            'Vnv = vapor phase molar amounts
            'Vns = solid phase molar amounts
            'Vxl = liquid phase molar fractions
            'Vxv = vapor phase molar fractions
            'Vxs = solid phase molar fractions
            'V, S, L = phase molar amounts (F = 1 = V + S + L)
            Dim Vnf(n), Vnl(n), Vxl(n), Vxl_ant(n), Vns(n), Vxs(n), Vnv(n), Vxv(n), V, S, L, L_ant, Vp(n) As Double
            Dim sumN As Double = 0

            Vnf = Vz.Clone

            'calculate SLE.

            Dim ids As New List(Of String)
            For i = 0 To n
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
            Next

            Vxl = Vz.Clone

            'initial estimates for L and S.

            L = 0.0#

            'calculate liquid phase activity coefficients.

            Dim ecount As Integer = 0
            Dim errfunc As Double = 0.0#

            Do

                activcoeff = PP.DW_CalcFugCoeff(Vxl, T, P, State.Liquid)

                For i = 0 To n
                    activcoeff(i) = activcoeff(i) * P / PP.AUX_PVAPi(ids(i), T)
                Next

                Dim Vxlmax(n) As Double

                'calculate maximum solubilities for solids/precipitates.

                For i = 0 To n
                    If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                        Vxlmax(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                        If Vxlmax(i) > 1 Then Vxlmax(i) = 1.0#
                    Else
                        Vxlmax(i) = 1.0#
                    End If
                Next

                'mass balance.

                Dim hassolids As Boolean = False

                S = 0.0#
                For i = 0 To n
                    If Vnf(i) > Vxlmax(i) Then
                        hassolids = True
                        Vxl(i) = Vxlmax(i)
                        S += Vnf(i) - Vxl(i) * L
                    End If
                Next

                'check for vapors
                V = 0.0#
                For i = 0 To n
                    If P < Vp(i) Then
                        V += Vnf(i)
                        Vxl(i) = 0
                        Vnv(i) = Vnf(i)
                    End If
                Next

                L_ant = L
                If hassolids Then L = 1 - S - V Else L = 1 - V

                For i = 0 To n
                    Vns(i) = Vnf(i) - Vxl(i) * L - Vnv(i)
                    Vnl(i) = Vxl(i) * L
                Next

                For i = 0 To n
                    If Sum(Vnl) <> 0.0# Then Vxl(i) = Vnl(i) / Sum(Vnl) Else Vxl(i) = 0.0#
                    If Sum(Vns) <> 0.0# Then Vxs(i) = Vns(i) / Sum(Vns) Else Vxs(i) = 0.0#
                    If Sum(Vnv) <> 0.0# Then Vxv(i) = Vnv(i) / Sum(Vnv) Else Vxv(i) = 0.0#
                Next

                errfunc = Abs(L - L_ant) ^ 2

                If errfunc <= etol Then Exit Do

                If Double.IsNaN(S) Then Throw New Exception(DWSIM.App.GetLocalString("PP_FlashTPSolidFracError"))
                If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PP_FlashMaxIt2"))

                ecount += 1

                CheckCalculatorStatus()

            Loop

            'return flash calculation results.

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & errfunc)

out:        Return New Object() {L, V, Vxl, Vxv, ecount, 0.0#, PP.RET_NullVector, S, Vxs}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1), Vs(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, S, Pf As Double

            d1 = Date.Now

            n = UBound(Vz)

            PP = PP
            Hf = H
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n), Vs(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = CInt(PP.Parameters("PP_PHFMII"))
            Dim maxitEXT As Integer = CInt(PP.Parameters("PP_PHFMEI"))
            Dim tolINT As Double = CDbl(PP.Parameters("PP_PHFILT"))
            Dim tolEXT As Double = CDbl(PP.Parameters("PP_PHFELT"))

            Dim Tsup, Tinf

            If Tref <> 0 Then
                Tinf = Tref - 250
                Tsup = Tref + 250
            Else
                Tinf = 100
                Tsup = 2000
            End If
            If Tinf < 100 Then Tinf = 100

            Dim bo As New BrentOpt.Brent
            bo.DefineFuncDelegate(AddressOf Herror)
            WriteDebugInfo("PH Flash: Starting calculation for " & Tinf & " <= T <= " & Tsup)

            Dim fx, fx2, dfdx, x1 As Double

            Dim cnt As Integer = 0

            If Tref = 0 Then Tref = 100.0#
            x1 = Tref
            Do
                fx = Herror(x1, {P, Vz, PP})
                fx2 = Herror(x1 + 1, {P, Vz, PP})
                If Abs(fx) < etol Then Exit Do
                dfdx = (fx2 - fx)
                x1 = x1 - fx / dfdx
                If x1 < 0 Then GoTo alt
                cnt += 1
            Loop Until cnt > 20 Or Double.IsNaN(x1)
            If Double.IsNaN(x1) Then
alt:            T = bo.BrentOpt(Tinf, Tsup, 100, tolEXT, maxitEXT, {P, Vz, PP})
            Else
                T = x1
            End If

            'End If

            Dim tmp As Object = Flash_PT(Vz, P, T, PP)

            L = tmp(0)
            V = tmp(1)
            S = tmp(7)
            Vx = tmp(2)
            Vy = tmp(3)
            Vs = tmp(8)
            ecount = tmp(4)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim doparallel As Boolean = My.Settings.EnableParallelProcessing

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1), Vs(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, Ss, T, Pf As Double

            d1 = Date.Now

            n = UBound(Vz)

            PP = PP
            Sf = S
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n), Vs(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = CInt(PP.Parameters("PP_PSFMII"))
            Dim maxitEXT As Integer = CInt(PP.Parameters("PP_PSFMEI"))
            Dim tolINT As Double = CDbl(PP.Parameters("PP_PSFILT"))
            Dim tolEXT As Double = CDbl(PP.Parameters("PP_PSFELT"))

            Dim Tsup, Tinf ', Ssup, Sinf

            If Tref <> 0 Then
                Tinf = Tref - 200
                Tsup = Tref + 200
            Else
                Tinf = 100
                Tsup = 2000
            End If
            If Tinf < 100 Then Tinf = 100
            Dim bo As New BrentOpt.Brent
            bo.DefineFuncDelegate(AddressOf Serror)
            WriteDebugInfo("PS Flash: Starting calculation for " & Tinf & " <= T <= " & Tsup)

            Dim fx, fx2, dfdx, x1 As Double

            Dim cnt As Integer = 0

            If Tref = 0 Then Tref = 298.15
            x1 = Tref
            Do
                fx = Serror(x1, {P, Vz, PP})
                fx2 = Serror(x1 + 1, {P, Vz, PP})
                If Abs(fx) < etol Then Exit Do
                dfdx = (fx2 - fx)
                x1 = x1 - fx / dfdx
                If x1 < 0 Then GoTo alt
                cnt += 1
            Loop Until cnt > 50 Or Double.IsNaN(x1)
            If Double.IsNaN(x1) Then
alt:            T = bo.BrentOpt(Tinf, Tsup, 10, tolEXT, maxitEXT, {P, Vz, PP})
            Else
                T = x1
            End If

            Dim tmp As Object = Flash_PT(Vz, P, T, PP)

            L = tmp(0)
            V = tmp(1)
            Ss = tmp(7)
            Vx = tmp(2)
            Vy = tmp(3)
            Vs = tmp(8)
            ecount = tmp(4)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, Ss, Vs}

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("TV Flash [NL-SLE]: Converged in " & 0 & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {0.0#, 0.0#, PP.RET_NullVector, PP.RET_NullVector, 0, 0, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Function SolidFractionError(x As Double, otherargs As Object)
            Dim res As Object = Me.Flash_PT(otherargs(1), otherargs(2), x, otherargs(3))
            Dim val As Double = (1 - otherargs(0)) - res(7)

            Return val

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object, ByVal pp As PropertyPackage) As Object

            Dim tmp As Object
            tmp = Me.Flash_PT(Vz, P, T, pp)
            Dim L, V, S, Vx(), Vy(), Vs(), _Hv, _Hl, _Hs As Double

            Dim n = UBound(Vz)

            L = tmp(0)
            V = tmp(1)
            S = tmp(7)
            Vx = tmp(2)
            Vy = tmp(3)
            Vs = tmp(8)

            _Hv = 0
            _Hl = 0
            _Hs = 0

            Dim mmg, mml, mms As Double
            If V > 0 Then _Hv = pp.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L > 0 Then _Hl = pp.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
            If S > 0 Then _Hs = pp.DW_CalcSolidEnthalpy(T, Vs, CompoundProperties)
            mmg = pp.AUX_MMM(Vy)
            mml = pp.AUX_MMM(Vx)
            mms = pp.AUX_MMM(Vs)

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L + mms * S)) * _Hv - (mml * L / (mmg * V + mml * L + mms * S)) * _Hl - (mms * S / (mmg * V + mml * L + mms * S)) * _Hs
            OBJ_FUNC_PH_FLASH = herr

            WriteDebugInfo("PH Flash [NL-SLE]: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object, ByVal pp As PropertyPackage) As Object

            Dim tmp As Object
            tmp = Me.Flash_PT(Vz, P, T, pp)
            Dim L, V, Ssf, Vx(), Vy(), Vs(), _Sv, _Sl, _Ss As Double

            Dim n = UBound(Vz)

            L = tmp(0)
            V = tmp(1)
            Ssf = tmp(7)
            Vx = tmp(2)
            Vy = tmp(3)
            Vs = tmp(8)

            _Sv = 0
            _Sl = 0
            _Ss = 0
            Dim mmg, mml, mms As Double

            If V > 0 Then _Sv = pp.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L > 0 Then _Sl = pp.DW_CalcEntropy(Vx, T, P, State.Liquid)
            If Ssf > 0 Then _Ss = pp.DW_CalcSolidEnthalpy(T, Vs, CompoundProperties) / (T - 298.15)
            mmg = pp.AUX_MMM(Vy)
            mml = pp.AUX_MMM(Vx)
            mms = pp.AUX_MMM(Vs)

            Dim serr As Double = Sf - (mmg * V / (mmg * V + mml * L + mms * Ssf)) * _Sv - (mml * L / (mmg * V + mml * L + mms * Ssf)) * _Sl - (mms * Ssf / (mmg * V + mml * L + mms * Ssf)) * _Ss
            OBJ_FUNC_PS_FLASH = serr

            WriteDebugInfo("PS Flash [NL-SLE]: Current T = " & T & ", Current S Error = " & serr)

        End Function

        Function Herror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PH_FLASH(Tt, Hf, otherargs(0), otherargs(1), otherargs(2))
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, otherargs(0), otherargs(1), otherargs(2))
        End Function


        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount, gcount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, S, Lf, Vf, Vint, T, Tf, deltaT As Double
            Dim e1 As Double
            Dim AF As Double = 1
            Dim GL_old, GS_old, GV_old As Double

            d1 = Date.Now

            etol = CDbl(PP.Parameters("PP_PTFELT"))
            maxit_e = CInt(PP.Parameters("PP_PTFMEI"))
            itol = CDbl(PP.Parameters("PP_PTFILT"))
            maxit_i = CInt(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            PP = PP
            Vf = V
            L = 1 - V
            Lf = 1 - Vf
            Tf = T
            Vint = V
            GL_old = L
            GV_old = V
            GS_old = 0

            Dim Vn(n) As String, Vx(n), Vy(n), Vs(n), Vmix(n), Vx_ant(n), Vy_ant(n), Vs_ant(n), Vp(n), Ki(n), fi(n) As Double
            Dim Vt(n), VTc(n), Tmin, Tmax, dFdT, Tsat(n) As Double

            Vn = PP.RET_VNAMES()
            VTc = PP.RET_VTC()
            fi = Vz.Clone

            Tmin = 0.0#
            Tmax = 0.0#

            If Tref = 0.0# Then
                i = 0
                Tref = 0.0#
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
                    If Double.IsInfinity(Vy(i)) Then Vy(i) = 0.0#
                Else
                    Vy(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            Vy = Vy.NormalizeY()
            Vx = Vz.SubtractY(Vy.MultiplyConstY(V)).MultiplyConstY(1 / L)

            If PP.AUX_IS_SINGLECOMP(Vz) Then
                WriteDebugInfo("PV Flash [SLE]: Converged in 1 iteration.")
                T = 0
                For i = 0 To n
                    T += Vz(i) * PP.AUX_TSATi(P, i)
                Next
                Return New Object() {L, V, Vx, Vy, T, 0, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            End If

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Tant, fval As Double
            Dim chk As Boolean = False

            If V = 1.0# Then

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
                            stmp4 = Ki.MultiplyY(Vx).SumY
                            'i = 0
                            'stmp4 = 0
                            'Do
                            '    stmp4 = stmp4 + Ki(i) * Vx(i)
                            '    i = i + 1
                            'Loop Until i = n + 1
                        Else
                            stmp4 = Vy.DivideY(Ki).SumY
                            'i = 0
                            'stmp4 = 0
                            'Do
                            '    stmp4 = stmp4 + Vy(i) / Ki(i)
                            '    i = i + 1
                            'Loop Until i = n + 1
                        End If

                        If V = 0 Then
                            Vy_ant = Vy.Clone
                            Vy = Ki.MultiplyY(Vx).MultiplyConstY(1 / stmp4)
                            'i = 0
                            'Do
                            '    Vy_ant(i) = Vy(i)
                            '    Vy(i) = Ki(i) * Vx(i) / stmp4
                            '    i = i + 1
                            'Loop Until i = n + 1
                        Else
                            Vx_ant = Vx.Clone
                            Vx = Vy.DivideY(Ki).MultiplyConstY(1 / stmp4)
                            'i = 0
                            'Do
                            '    Vx_ant(i) = Vx(i)
                            '    Vx(i) = (Vy(i) / Ki(i)) / stmp4
                            '    i = i + 1
                            'Loop Until i = n + 1
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
                    K2 = PP.DW_CalcKvalue(Vx, Vy, T + 0.01, P)

                    dKdT = K2.SubtractY(K1).MultiplyConstY(1 / 0.01)
                    'For i = 0 To n
                    '    dKdT(i) = (K2(i) - K1(i)) / 0.01
                    'Next

                    fval = stmp4 - 1

                    ecount += 1

                    i = 0
                    dFdT = 0
                    Do
                        If V = 0 Then
                            dFdT = Vx.MultiplyY(dKdT).SumY
                            'dFdT = dFdT + Vx(i) * dKdT(i)
                        Else
                            dFdT = -Vy.DivideY(Ki).DivideY(Ki).MultiplyY(dKdT).SumY
                            'dFdT = dFdT - Vy(i) / (Ki(i) ^ 2) * dKdT(i)
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    Tant = T
                    deltaT = -fval / dFdT

                    If Abs(deltaT) > 0.1 * T Then
                        T = T + Sign(deltaT) * 0.1 * T
                    Else
                        T = T + deltaT
                    End If

                    WriteDebugInfo("PV Flash [SLE]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    CheckCalculatorStatus()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(T) = True Or ecount > maxit_e

            Else
                Do
                    ecount = 0

                    '================================================
                    '== mix vapour and liquid phase =================
                    '================================================
                    Vmix = Vy.MultiplyConstY(V)
                    Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                    Vz = Vmix.NormalizeY

                    Do
                        Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        i = 0
                        Do
                            If Vz(i) <> 0 Then
                                Vy_ant(i) = Vy(i)
                                Vx_ant(i) = Vx(i)
                                Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * Vint + 1)
                                Vx(i) = Vy(i) / Ki(i)
                            Else
                                Vy(i) = 0
                                Vx(i) = 0
                            End If
                            i += 1
                        Loop Until i = n + 1

                        Vx = Vx.NormalizeY()
                        Vy = Vy.NormalizeY()

                        If Vint <= 0.5 Then

                            stmp4 = Ki.MultiplyY(Vx).SumY
                            'i = 0
                            'stmp4 = 0
                            'Do
                            '    stmp4 = stmp4 + Ki(i) * Vx(i)
                            '    i = i + 1
                            'Loop Until i = n + 1

                            Dim K1(n), K2(n), dKdT(n) As Double

                            K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                            K2 = PP.DW_CalcKvalue(Vx, Vy, T + 0.1, P)

                            dKdT = K2.SubtractY(K1).MultiplyConstY(1 / 0.1)
                            'For i = 0 To n
                            '    dKdT(i) = (K2(i) - K1(i)) / (0.1)
                            'Next

                            dFdT = Vx.MultiplyY(dKdT).SumY
                            'i = 0
                            'dFdT = 0
                            'Do
                            '    dFdT = dFdT + Vx(i) * dKdT(i)
                            '    i = i + 1
                            'Loop Until i = n + 1

                        Else

                            stmp4 = Vy.DivideY(Ki).SumY
                            'i = 0
                            'stmp4 = 0
                            'Do
                            '    stmp4 = stmp4 + Vy(i) / Ki(i)
                            '    i = i + 1
                            'Loop Until i = n + 1

                            Dim K1(n), K2(n), dKdT(n) As Double

                            K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                            K2 = PP.DW_CalcKvalue(Vx, Vy, T + 1, P)

                            dKdT = K2.SubtractY(K1)
                            'For i = 0 To n
                            '    dKdT(i) = (K2(i) - K1(i)) / (1)
                            'Next

                            dFdT = -Vy.DivideY(Ki).DivideY(Ki).MultiplyY(dKdT).SumY
                            'i = 0
                            'dFdT = 0
                            'Do
                            '    dFdT = dFdT - Vy(i) / (Ki(i) ^ 2) * dKdT(i)
                            '    i = i + 1
                            'Loop Until i = n + 1

                        End If

                        ecount += 1

                        fval = stmp4 - 1

                        Tant = T
                        deltaT = -fval / dFdT * AF
                        AF *= 1.01
                        If Abs(deltaT) > 0.1 * T Then
                            T = T + Sign(deltaT) * 0.1 * T
                        Else
                            T = T + deltaT
                        End If

                        e1 = Vx.SubtractY(Vx_ant).AbsSumY + Vy.SubtractY(Vy_ant).AbsSumY + Math.Abs(T - Tant)

                        WriteDebugInfo("PV Flash [SLE]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                        CheckCalculatorStatus()

                    Loop Until (Math.Abs(fval) < etol And e1 < etol) Or Double.IsNaN(T) = True Or ecount > maxit_e

                    '================================================
                    '== mix solid and liquid phase ==================
                    '================================================
                    Vmix = Vs.MultiplyConstY(S)
                    Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                    Vz = Vmix.NormalizeY

                    '================================================
                    '== Do SLE flash to precipitate solids ==========
                    '================================================
                    Vs_ant = Vs.Clone
                    Dim SL_Result As FlashResult
                    SL_Result = Flash_SL(Vz, P, T, PP)
                    Vx = SL_Result.Vx
                    Vs = SL_Result.Vs

                    '================================================
                    '== Calculate global phase fractions ============
                    '================================================
                    GL_old = L
                    GS_old = S
                    L = SL_Result.LF * (1 - V)
                    S = SL_Result.SF * (1 - V)

                    '===================================================================
                    '== Calculate vapour fraction relative to vapour/liquid ============
                    '===================================================================
                    Vint = V / (1 - S)
                    If Vint > 1 Then
                        'no liquid left, take some solid to vapour phase
                        Vint = 1
                    End If

                    e1 = 1000 * (Abs(GL_old - L) + Abs(GS_old - S))
                    gcount += 1
                Loop Until e1 < etol Or gcount > maxit_e
            End If

           

            d2 = Date.Now

            dt = d2 - d1

            If ecount > maxit_e Then Throw New Exception(DWSIM.App.GetLocalString("PropPack_FlashMaxIt2"))

            If PP.AUX_CheckTrivial(Ki) Then Throw New Exception("PV Flash [SLE]: Invalid result: converged to the trivial solution (T = " & T & " ).")

            WriteDebugInfo("PV Flash [SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides Function Flash_PSF(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object
            'Pressure/Solid fraction flash
            If SolidSolution Then
                Return Flash_PSF_SS(Vz, P, V, Tref, PP)
            Else
                Return Flash_PSF_E(Vz, P, V, Tref, PP)
            End If

        End Function

        Public Function Flash_PSF_SS(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim soma_x, soma_s As Double
            Dim L, S, Lf, Sf, T, Tf As Double
            Dim ids As New List(Of String)

            d1 = Date.Now

            etol = CDbl(PP.Parameters("PP_PTFELT"))
            maxit_e = CInt(PP.Parameters("PP_PTFMEI"))
            itol = CDbl(PP.Parameters("PP_PTFILT"))
            maxit_i = CInt(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            PP = PP
            L = V
            Lf = L
            S = 1 - L
            Lf = 1 - Sf
            Tf = T

            Dim Vn(n) As String, Vx(n), Vs(n), Vx_ant(1), Vs_ant(1), Vp(n), Vp2(n), Ki(n), Ki_ant(n), fi(n), activcoeff(n), activcoeff2(n) As Double
            Dim Vt(n), VTF(n), Tmin, Tmax, dFdT As Double

            Vn = PP.RET_VNAMES()
            VTF = PP.RET_VTF()
            fi = Vz.Clone

            If Tref = 0.0# Then

                i = 0
                Tref = 0
                Do
                    If L = 0 Then
                        Tref = MathEx.Common.Min(VTF)
                    Else
                        Tref += Vz(i) * VTF(i)
                    End If
                    Tmin += 0.1 * Vz(i) * VTF(i)
                    Tmax += 2.0 * Vz(i) * VTF(i)
                    i += 1
                Loop Until i = n + 1

            Else

                Tmin = Tref - 50
                Tmax = Tref + 50

            End If

            T = Tref

            'Calculate Ki`s

            i = 0
            Do
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
                If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                    Ki(i) = Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                Else
                    Ki(i) = 1.0E+20
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0.0# Then
                    Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                    If Ki(i) <> 0 Then Vs(i) = Vx(i) / Ki(i) Else Vs(i) = Vz(i)
                    If Vs(i) < 0 Then Vs(i) = 0
                    If Vx(i) < 0 Then Vx(i) = 0
                Else
                    Vs(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x = 0.0#
            soma_s = 0.0#
            Do
                soma_x = soma_x + Vx(i)
                soma_s = soma_s + Vs(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                Vx(i) = Vx(i) / soma_x
                Vs(i) = Vs(i) / soma_s
                i = i + 1
            Loop Until i = n + 1

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Tant, fval As Double
            Dim chk As Boolean = False

            ecount = 0
            Do

                marcador3 = 0

                Dim cont_int = 0
                Do

                    Ki_ant = Ki.Clone

                    activcoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid)

                    For i = 0 To n
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        activcoeff(i) = activcoeff(i) * P / Vp(i)
                        If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                            Ki(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                        Else
                            Ki(i) = 1.0E+20
                        End If
                    Next

                    marcador = 0
                    If stmp4_ant <> 0 Then
                        marcador = 1
                    End If
                    stmp4_ant = stmp4

                    If L = 0 Then
                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Ki(i) * Vs(i)
                            i = i + 1
                        Loop Until i = n + 1
                    Else
                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Vx(i) / Ki(i)
                            i = i + 1
                        Loop Until i = n + 1
                    End If

                    If L = 0 Then
                        i = 0
                        Do
                            Vx_ant(i) = Vx(i)
                            Vx(i) = Ki(i) * Vs(i) / stmp4
                            i = i + 1
                        Loop Until i = n + 1
                    Else
                        i = 0
                        Do
                            Vs_ant(i) = Vs(i)
                            Vs(i) = (Vx(i) / Ki(i)) / stmp4
                            i = i + 1
                        Loop Until i = n + 1
                    End If

                    marcador2 = 0
                    If marcador = 1 Then
                        If L = 0 Then
                            If Math.Abs(Vx(0) - Vx_ant(0)) < itol Then
                                marcador2 = 1
                            End If
                        Else
                            If Math.Abs(Vs(0) - Vs_ant(0)) < itol Then
                                marcador2 = 1
                            End If
                        End If
                    End If

                    cont_int = cont_int + 1

                Loop Until marcador2 = 1 Or Double.IsNaN(stmp4) Or cont_int > maxit_i

                Dim K1(n), K2(n), dKdT(n) As Double

                activcoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                activcoeff2 = PP.DW_CalcFugCoeff(Vx, T + 0.01, P, State.Liquid)

                For i = 0 To n
                    If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        activcoeff(i) = activcoeff(i) * P / Vp(i)
                        K1(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                        Vp2(i) = PP.AUX_PVAPi(i, T + 0.01)
                        activcoeff2(i) = activcoeff2(i) * P / Vp2(i)
                        K2(i) = (1 / activcoeff2(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * (T + 0.01)) * (1 - (T + 0.01) / CompoundProperties(i).TemperatureOfFusion))
                    Else
                        K1(i) = 1.0E+20
                        K2(i) = 1.0E+20
                    End If
                Next

                For i = 0 To n
                    dKdT(i) = (K2(i) - K1(i)) / 0.01
                Next

                fval = stmp4 - 1

                ecount += 1

                i = 0
                dFdT = 0
                Do
                    If L = 0.0# Then
                        dFdT = dFdT + Vs(i) * dKdT(i)
                    Else
                        dFdT = dFdT - Vx(i) / (Ki(i) ^ 2) * dKdT(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Tant = T
                T = T - fval / dFdT
                'If T < Tmin Then T = Tmin
                'If T > Tmax Then T = Tmax

                WriteDebugInfo("PV Flash [NL-SLE]: Iteration #" & ecount & ", T = " & T & ", LF = " & L)

                CheckCalculatorStatus()

            Loop Until Math.Abs(T - Tant) < 0.01 Or Double.IsNaN(T) = True Or ecount > maxit_e Or Double.IsNaN(T) Or Double.IsInfinity(T)

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PSF Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, PP.RET_NullVector, T, ecount, Ki, 0.0#, PP.RET_NullVector, S, Vs}


        End Function

        Public Function Flash_PSF_E(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, S, Lf, Sf, T, Tf As Double
            Dim ids As New List(Of String)

            d1 = Date.Now

            etol = CDbl(PP.Parameters("PP_PTFELT"))
            maxit_e = CInt(PP.Parameters("PP_PTFMEI"))
            itol = CDbl(PP.Parameters("PP_PTFILT"))
            maxit_i = CInt(PP.Parameters("PP_PTFMII"))

            n = UBound(Vz)

            PP = PP
            L = V
            Lf = L
            S = 1 - L
            Lf = 1 - Sf
            Tf = T

            Dim Vn(n) As String, Vx(n), Vs(n), Vx_ant(1), Vs_ant(1), Vp(n), Vp2(n), Ki(n), Ki_ant(n), fi(n), activcoeff(n), activcoeff2(n) As Double
            Dim Vt(n), VTF(n), Tmin, Tmax As Double

            Vn = PP.RET_VNAMES()
            VTF = PP.RET_VTF()
            fi = Vz.Clone

            If Tref = 0.0# Then

                i = 0
                Tref = 0
                Do
                    If L = 0 Then 'L=0
                        Tref = MathEx.Common.Min(VTF)
                    Else
                        Tref += Vz(i) * VTF(i)
                    End If
                    Tmin += 0.1 * Vz(i) * VTF(i)
                    Tmax += 2.0 * Vz(i) * VTF(i)
                    i += 1
                Loop Until i = n + 1

            Else

                Tmin = Tref - 50
                Tmax = Tref + 50

            End If

            T = Tref

            'Calculate Ki`s

            i = 0
            Do
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
                If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                    Ki(i) = Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                Else
                    Ki(i) = 1.0E+20
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0.0# Then
                    Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                    If Ki(i) <> 0 Then Vs(i) = Vx(i) / Ki(i) Else Vs(i) = Vz(i)
                    If Vs(i) < 0 Then Vs(i) = 0
                    If Vx(i) < 0 Then Vx(i) = 0
                Else
                    Vs(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            Vx = Vx.NormalizeY
            Vs = Vx.NormalizeY

            Dim chk As Boolean = False

            Dim result As Object

            If PP.AUX_IS_SINGLECOMP(Vz) Then
                T = 0
                For i = 0 To n
                    T += Vz(i) * Me.CompoundProperties(i).TemperatureOfFusion
                Next
                result = Me.Flash_PT(Vz, P, T, PP)
                Return New Object() {result(0), result(1), result(2), result(3), T, 0, PP.RET_NullVector, 0.0#, PP.RET_NullVector, result(7), result(8)}
            End If

            T = 0
            For i = 0 To n
                T += Vz(i) * Me.CompoundProperties(i).TemperatureOfFusion - 30
                VTF(i) = Me.CompoundProperties(i).TemperatureOfFusion
            Next

            ecount = 0

            Dim SF0, SF1, T0, T1 As Double
            T0 = Common.Min(VTF) * 0.6
            T1 = Common.Max(VTF) + 10

            SF0 = Flash_PT(Vz, P, T0, PP)(7)
            SF1 = Flash_PT(Vz, P, T1, PP)(7)

            Do
                T = (T0 + T1) / 2
                Sf = Flash_PT(Vz, P, T, PP)(7)

                If Sf > V Then
                    T0 = T
                    SF0 = Sf
                Else
                    T1 = T
                    SF1 = Sf
                End If
                ecount += 1
            Loop Until (T1 - T0) <= itol

            result = Me.Flash_PT_NL(Vz, P, T, PP)

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PSF Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {result(0), result(1), result(2), result(3), T, ecount, PP.RET_NullVector, 0.0#, PP.RET_NullVector, result(7), result(8)}

        End Function

    End Class

End Namespace
