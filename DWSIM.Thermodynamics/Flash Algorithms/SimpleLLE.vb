'    Simplified LLE Flash Algorithm
'    Copyright 2013-2014 Daniel Wagner O. de Medeiros
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

Imports System.Threading.Tasks
Imports System.Linq

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class SimpleLLE

        Inherits FlashAlgorithm

        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Hv0, Hvid, Hlid, Hf, Hv, Hl As Double
        Dim Sv0, Svid, Slid, Sf, Sv, Sl As Double
        Dim cult As Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Public Property InitialEstimatesForPhase1 As Double()
        Public Property UseInitialEstimatesForPhase1 As Boolean = False

        Public Property InitialEstimatesForPhase2 As Double()
        Public Property UseInitialEstimatesForPhase2 As Boolean = False

        Sub New()
            MyBase.New()
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Simple_LLE
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Algoritmo Flash para cálculo de equilíbrio entre duas fases líquidas"
                Else
                    Return "Flash Algorithm for simple Liquid-Liquid equilibrium calculations"
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Simple LLE"
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, j, n, ecount As Integer
            n = Vz.Length - 1

            Dim Vx1(n), Vx2(n), Vy(n), Vn1(n), Vn2(n), Ki(n), fi1(n), fi2(n), gamma1(n), gamma2(n), Vp(n) As Double
            Dim Vx1_ant(n), Vx2_ant(n), Vn1_ant(n), Vn2_ant(n), L1_ant, L2_ant As Double
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L1, L2, V, S As Double
            Dim e1, e2 As Double
            d1 = Date.Now

            etol = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance), cult)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance), cult)
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            If UseInitialEstimatesForPhase1 And UseInitialEstimatesForPhase2 Then
                L1 = 0
                L2 = 0
                For i = 0 To n
                    If Vz(i) > 0 Then
                        j += 1
                        If InitialEstimatesForPhase1(i) = InitialEstimatesForPhase2(i) Then
                            L1 += 0.5
                        Else
                            L1 += (Vz(i) - InitialEstimatesForPhase2(i)) / (InitialEstimatesForPhase1(i) - InitialEstimatesForPhase2(i))
                        End If
                    End If
                Next
                L1 = L1 / j
                If L1 > 0.99 Then L1 = 0.99
                If L1 < 0.01 Then L1 = 0.01
                L2 = 1 - L1
            Else
                Dim minn As Double = Vz(0)
                j = 0
                For i = 0 To n
                    If Vz(i) > 0 And Vz(i) < minn Then
                        j = i
                        minn = Vz(i)
                    End If
                Next
                For i = 0 To n
                    If Vz(i) = minn Then
                        Vn1(i) = Vz(i) * 0.05
                        Vn2(i) = Vz(i) * 0.95
                    Else
                        Vn1(i) = Vz(i) * 0.95
                        Vn2(i) = Vz(i) * 0.05
                    End If
                Next
                L1 = Vn1.Sum
                L2 = Vn2.Sum
            End If

            If UseInitialEstimatesForPhase1 Then
                For i = 0 To n
                    If Vz(i) > 0 Then Vn1(i) = L1 * InitialEstimatesForPhase1(i)
                Next
            End If

            If UseInitialEstimatesForPhase2 Then
                For i = 0 To n
                    If Vz(i) > 0 Then Vn2(i) = L2 * InitialEstimatesForPhase2(i)
                Next
            End If

            'renormalise Vn's
            S = Vn1.Sum() + Vn2.Sum()
            For i = 0 To n
                Vn1(i) /= S
                Vn2(i) /= S
            Next

            'calculate vapour pressures
            For i = 0 To n
                Vp(i) = PP.AUX_PVAPi(i, T)
            Next


            Dim err As Double = 0.0#
            Dim err_ant As Double = 0.0#

            ecount = 0

            Do
                Vx1_ant = Vx1.Clone
                Vx2_ant = Vx2.Clone

                Vn1_ant = Vn1.Clone
                Vn2_ant = Vn2.Clone

                Vx1 = Vn1.MultiplyConstY(1 / L1).NormalizeY
                Vx2 = Vn2.MultiplyConstY(1 / L2).NormalizeY

                fi1 = PP.DW_CalcFugCoeff(Vx1, T, P, State.Liquid)
                fi2 = PP.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)

                For i = 0 To n
                    gamma1(i) = P / Vp(i) * fi1(i)
                    gamma2(i) = P / Vp(i) * fi2(i)
                Next

                err_ant = err
                err = 0.0#
                e1 = 0
                e2 = 0
                S = 0
                For i = 0 To n
                    err += Abs(Vx1(i) * gamma1(i) - Vx2(i) * gamma2(i))
                    e1 += Abs(Vx1(i) - Vx1_ant(i))
                    e2 += Abs(Vx2(i) - Vx2_ant(i))
                    S += Abs(Vx1(i) - Vx2(i))
                Next


                If Double.IsNaN(err) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                If ecount > 0 And (Abs(err) < 0.000001 Or L1 < 0.0001 Or L2 < 0.0001 Or S < 0.0001) Then Exit Do

                For i = 0 To n
                    Vn1(i) = Vz(i) / (1 + gamma1(i) * L2 / (gamma2(i) * L1))
                    Vn2(i) = Vz(i) - Vn1(i)
                Next

                L1 = Vn1.Sum
                L2 = 1 - L1

                If Abs(L1_ant - L2) < 0.0001 And Abs(L2_ant - L1) < 0.0001 Then 'detect oscilating condition
                    'replace with average of both oscilating compositions to stabilise calculations
                    Vn1 = Vn1.AddY(Vn1_ant).MultiplyConstY(0.5)
                    Vn2 = Vz.SubtractY(Vn1)
                    L1 = Vn1.SumY
                    L2 = 1 - L1
                End If

                ecount += 1

                If ecount > 10000 Then Throw New Exception(Calculator.GetLocalString("Nmeromximodeiteraesa3"))

            Loop

out:        d2 = Date.Now
            dt = d2 - d1

            WriteDebugInfo("PT Flash [SimpleLLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & err)

            If L1 < 0.0001 Or L2 < 0.0001 Or S < 0.0001 Then
                'merge phases - both phases are identical
                Return New Object() {1, V, Vz, PP.RET_NullVector, ecount, 0, Vx2, 0.0#, PP.RET_NullVector, gamma1, gamma2}
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
                    Return New Object() {L1, V, Vx1, PP.RET_NullVector, ecount, L2, Vx2, 0.0#, PP.RET_NullVector, gamma1, gamma2}
                Else
                    Return New Object() {L2, V, Vx2, PP.RET_NullVector, ecount, L1, Vx1, 0.0#, PP.RET_NullVector, gamma1, gamma2}
                End If
            End If

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, Pf As Double

            d1 = Date.Now

            n = Vz.Length - 1

            PP = PP
            Hf = H
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance), cult)
            Dim tolEXT As Double = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance), cult)

            Dim Tsup, Tinf ', Hsup, Hinf

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

            If Tref = 0 Then Tref = 298.15
            x1 = Tref
            Do
                If Settings.EnableParallelProcessing Then

                    Dim task1 As Task = New Task(Sub()
                                                     fx = Herror(x1, {P, Vz, PP})
                                                 End Sub)
                    Dim task2 As Task = New Task(Sub()
                                                     fx2 = Herror(x1 + 1, {P, Vz, PP})
                                                 End Sub)
                    task1.Start()
                    task2.Start()
                    Task.WaitAll(task1, task2)

                Else
                    fx = Herror(x1, {P, Vz, PP})
                    fx2 = Herror(x1 + 1, {P, Vz, PP})
                End If
                If Abs(fx) < etol Then Exit Do
                dfdx = (fx2 - fx)
                x1 = x1 - fx / dfdx
                If x1 < 0 Then GoTo alt
                cnt += 1
            Loop Until cnt > 20 Or Double.IsNaN(x1)
            If Double.IsNaN(x1) Then
alt:            T = bo.BrentOpt(Tinf, Tsup, 10, tolEXT, maxitEXT, {P, Vz, PP})
            Else
                T = x1
            End If

            'End If

            Dim tmp As Object = Flash_PT(Vz, P, T, PP)

            L = tmp(0)
            V = tmp(1)
            Vx = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [SimpleLLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, Pf As Double

            d1 = Date.Now

            n = Vz.Length - 1

            PP = PP
            Sf = S
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance), cult)
            Dim tolEXT As Double = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance), cult)

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
                If Settings.EnableParallelProcessing Then

                    Dim task1 As Task = New Task(Sub()
                                                     fx = Serror(x1, {P, Vz, PP})
                                                 End Sub)
                    Dim task2 As Task = New Task(Sub()
                                                     fx2 = Serror(x1 + 1, {P, Vz, PP})
                                                 End Sub)
                    task1.Start()
                    task2.Start()
                    Task.WaitAll(task1, task2)

                Else
                    fx = Serror(x1, {P, Vz, PP})
                    fx2 = Serror(x1 + 1, {P, Vz, PP})
                End If
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
            Vx = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [SimpleLLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim Pmin, Pmax, soma_x, soma_y As Double
            Dim L, Lf, Vf, P, Pf As Double

            d1 = Date.Now

            etol = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance), cult)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance), cult)
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            PP = PP
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

                    WriteDebugInfo("TV Flash [SimpleLLE]: Iteration #" & ecount & ", P = " & P & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

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

                    WriteDebugInfo("TV Flash [SimpleLLE]: Iteration #" & ecount & ", P = " & P & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(P) = True Or ecount > maxit_e

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("TV Flash [SimpleLLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, P, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim soma_x, soma_y As Double
            Dim L, Lf, Vf, T, Tf As Double

            d1 = Date.Now

            etol = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance), cult)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Double.Parse(Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance), cult)
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            PP = PP
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
                    If T < Tmin Then T = Tmin
                    If T > Tmax Then T = Tmax

                    WriteDebugInfo("PV Flash [SimpleLLE]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

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
                    If T < Tmin Then T = Tmin
                    If T > Tmax Then T = Tmax

                    WriteDebugInfo("PV Flash [SimpleLLE]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(T) = True Or ecount > maxit_e

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PV Flash [SimpleLLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object, ByVal pp As PropertyPackage) As Object

            Dim tmp As Object
            tmp = Me.Flash_PT(Vz, P, T, pp)
            Dim L, V, Vx(), Vy(), _Hv, _Hl As Double

            Dim n = Vz.Length - 1

            L = tmp(0)
            V = tmp(1)
            Vx = tmp(2)
            Vy = tmp(3)

            _Hv = 0
            _Hl = 0

            Dim mmg, mml As Double
            If V > 0 Then _Hv = pp.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L > 0 Then _Hl = pp.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
            mmg = pp.AUX_MMM(Vy)
            mml = pp.AUX_MMM(Vx)

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L)) * _Hv - (mml * L / (mmg * V + mml * L)) * _Hl
            OBJ_FUNC_PH_FLASH = herr

            WriteDebugInfo("PH Flash [SimpleLLE]: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object, ByVal pp As PropertyPackage) As Object

            Dim tmp = Me.Flash_PT(Vz, P, T, pp)
            Dim L, V, Vx(), Vy(), _Sv, _Sl As Double

            Dim n = Vz.Length - 1

            L = tmp(0)
            V = tmp(1)
            Vx = tmp(2)
            Vy = tmp(3)

            _Sv = 0
            _Sl = 0
            Dim mmg, mml As Double

            If V > 0 Then _Sv = pp.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L > 0 Then _Sl = pp.DW_CalcEntropy(Vx, T, P, State.Liquid)
            mmg = pp.AUX_MMM(Vy)
            mml = pp.AUX_MMM(Vx)

            Dim serr As Double = Sf - (mmg * V / (mmg * V + mml * L)) * _Sv - (mml * L / (mmg * V + mml * L)) * _Sl
            OBJ_FUNC_PS_FLASH = serr

            WriteDebugInfo("PS Flash [SimpleLLE]: Current T = " & T & ", Current S Error = " & serr)

        End Function

        Function Herror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PH_FLASH(Tt, Sf, otherargs(0), otherargs(1), otherargs(2))
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, otherargs(0), otherargs(1), otherargs(2))
        End Function

    End Class

End Namespace

