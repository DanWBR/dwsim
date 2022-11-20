'    Forced Phase Flash Algorithm
'    Copyright 2021 Daniel Wagner O. de Medeiros  
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
Imports DotNumerics.Optimization
Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.BrentOpt
Imports DWSIM.MathOps.MathEx.Interpolation
Imports DWSIM.SharedClasses
Imports Eto.Forms
Imports IronPython.Runtime.Operations
Imports MathNet.Numerics

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class ForcedPhaseFlash

        Inherits FlashAlgorithm

        Dim Hf, Sf As Double

        Public Property ForcePhase As Interfaces.Enums.ForcedPhase = Interfaces.Enums.ForcedPhase.None

        Sub New()
            MyBase.New()
            Order = 1
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Custom
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String = "Forced Phase Flash"

        Public Overrides ReadOnly Property Name As String = "Forced Phase Flash"

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Select Case ForcePhase
                Case Interfaces.Enums.ForcedPhase.Vapor
                    Return New Object() {0.0, 1.0, Vz, Vz, 0, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector, Vz}
                Case Interfaces.Enums.ForcedPhase.Liquid
                    Return New Object() {1.0, 0.0, Vz, Vz, 0.0, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector, Vz}
                Case Interfaces.Enums.ForcedPhase.Solid
                    Return New Object() {0.0, 0.0, Vz, Vz, 0, 0.0#, PP.RET_NullVector, 1.0, Vz, Vz}
                Case Else
                    Return New Object() {0.0, 0.0, Vz, Vz, 0, 0.0#, PP.RET_NullVector, 0.0, Vz, Vz}
            End Select

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, j, n, ecount As Integer
            Dim L1, L2, V, T, Pf, Sx As Double

            n = Vz.Length - 1

            PP = PP
            Hf = H
            Pf = P

            Dim Vn(n) As String, Vx1(n), Vx2(n), Vy(n), Vs(n), Ki(n), Ki_ant(n), fi(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim Tmin, Tmax, epsilon(4), maxDT As Double

            Tmax = 10000.0#
            Tmin = 20.0#
            maxDT = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_MaximumTemperatureChange).ToDoubleFromInvariant

            epsilon(0) = 1
            epsilon(1) = 0.1
            epsilon(2) = 0.01

            Dim fx, fx1, fx2, fx_ant, dfdx, x1, x0, dx As Double

            Dim cnt As Integer

            If Tref = 0.0# Then Tref = 298.15
            T = Tref

            For j = 0 To 2

                cnt = 0
                x1 = Tref

                Dim fxvals, xvals As New List(Of Double)

                Do

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PH", "PH Flash Newton Iteration", "Pressure-Enthalpy Flash Algorithm (Fast Mode) Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    If cnt > 20 Then xvals.Add(x1)
                    fx_ant = fx

                    Dim herrobj As Object

                    If Settings.EnableParallelProcessing Then

                        Dim task0 = TaskHelper.Run(Sub()
                                                       herrobj = Herror("PT", x1, P, Vz, PP, False, Nothing)
                                                       fx = herrobj(0)
                                                   End Sub, Settings.TaskCancellationTokenSource.Token)
                        Dim task1 = TaskHelper.Run(Sub()
                                                       fx1 = Herror("PT", x1 - epsilon(j), P, Vz, PP, False, Nothing)(0)
                                                   End Sub, Settings.TaskCancellationTokenSource.Token)
                        Dim task2 = TaskHelper.Run(Sub()
                                                       fx2 = Herror("PT", x1 + epsilon(j), P, Vz, PP, False, Nothing)(0)
                                                   End Sub, Settings.TaskCancellationTokenSource.Token)
                        Task.WaitAll(task0, task1, task2)

                    Else

                        IObj2?.SetCurrent()
                        herrobj = Herror("PT", x1, P, Vz, PP, False, Nothing)
                        fx = herrobj(0)
                        IObj2?.SetCurrent()
                        herrobj = Herror("PT", x1 - epsilon(j), P, Vz, PP, False, Nothing)
                        fx1 = herrobj(0)
                        IObj2?.SetCurrent()
                        herrobj = Herror("PT", x1 + epsilon(j), P, Vz, PP, False, Nothing)
                        fx2 = herrobj(0)

                    End If

                    IObj2?.Paragraphs.Add(String.Format("Current Enthalpy error: {0}", fx2))

                    dfdx = (fx2 - fx1) / (2 * epsilon(j))

                    If Double.IsNaN(fx) Then
                        Dim ex As New Exception("PH Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                        ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                        ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                        Throw ex
                    End If

                    If cnt > 20 Then fxvals.Add(fx)

                    If Abs(fx) <= 0.000001 Then Exit Do

                    dx = fx / dfdx

                    If Abs(dx) > maxDT Then dx = maxDT * Sign(dx)

                    x0 = x1

                    If cnt > 30 And Math.Sign(fx) <> Math.Sign(fx_ant) Then

                        'oscillating around the solution.

                        Dim bmin As New Brent

                        Dim interp As New MathNet.Numerics.Interpolation.BulirschStoerRationalInterpolation(xvals.ToArray(), fxvals.ToArray())

                        x1 = bmin.BrentOpt2(xvals.Min, xvals.Max, 5, 0.01, 100,
                                            Function(tval)
                                                Return interp.Interpolate(tval)
                                            End Function)

                        Exit Do

                    Else

                        x1 = x1 - dx

                    End If

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", x1))

                    cnt += 1

                    IObj2?.Close()

                Loop Until cnt > 100 Or Double.IsNaN(x1) Or x1 < 0.0#

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > 100 Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            Dim tmp = Me.Flash_PT(Vz, P, T, PP, ReuseKI, Ki)
            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, Sx, Vs}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, j, n, ecount As Integer
            Dim L1, L2, V, T, Pf, Sx As Double

            n = Vz.Length - 1

            PP = PP
            Sf = S
            Pf = P

            Dim Vn(n) As String, Vx1(n), Vx2(n), Vy(n), Vs(n), Ki(n), Ki_ant(n), fi(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim Tmin, Tmax, epsilon(4), maxDT As Double

            Tmax = 10000.0#
            Tmin = 20.0#
            maxDT = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_MaximumTemperatureChange).ToDoubleFromInvariant

            epsilon(0) = 1
            epsilon(1) = 0.1
            epsilon(2) = 0.01

            Dim fx, fx1, fx2, fx_ant, dfdx, x1, x0, dx As Double

            Dim cnt As Integer

            If Tref = 0 Then Tref = 298.15

            For j = 0 To 2

                cnt = 0
                x1 = Tref

                Dim fxvals, xvals As New List(Of Double)

                Dim serrobj As Object

                Do

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PS", "PS Flash Newton Iteration", "Pressure-Entropy Flash Algorithm (Fast Mode) Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    If cnt > 20 Then xvals.Add(x1)
                    fx_ant = fx

                    If Settings.EnableParallelProcessing Then

                        Dim task0 = TaskHelper.Run(Sub()
                                                       serrobj = Serror("PT", x1, P, Vz, PP, False, Nothing)
                                                       fx = serrobj(0)
                                                   End Sub, Settings.TaskCancellationTokenSource.Token)
                        Dim task1 = TaskHelper.Run(Sub()
                                                       fx1 = Serror("PT", x1 - epsilon(j), P, Vz, PP, False, Nothing)(0)
                                                   End Sub, Settings.TaskCancellationTokenSource.Token)
                        Dim task2 = TaskHelper.Run(Sub()
                                                       fx2 = Serror("PT", x1 + epsilon(j), P, Vz, PP, False, Nothing)(0)
                                                   End Sub, Settings.TaskCancellationTokenSource.Token)
                        Task.WaitAll(task0, task1, task2)

                    Else

                        IObj2?.SetCurrent()
                        serrobj = Serror("PT", x1, P, Vz, PP, False, Nothing)
                        fx = serrobj(0)
                        IObj2?.SetCurrent()
                        serrobj = Serror("PT", x1 - epsilon(j), P, Vz, PP, False, Nothing)
                        fx1 = serrobj(0)
                        IObj2?.SetCurrent()
                        serrobj = Serror("PT", x1 + epsilon(j), P, Vz, PP, False, Nothing)
                        fx2 = serrobj(0)

                    End If

                    IObj2?.Paragraphs.Add(String.Format("Current Entropy error: {0}", fx2))

                    dfdx = (fx2 - fx1) / (2 * epsilon(j))


                    If Double.IsNaN(fx) Then
                        Dim ex As New Exception("PS Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                        ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                        ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                        Throw ex
                    End If

                    If Abs(fx) < 0.000001 Then Exit Do

                    If cnt > 20 Then fxvals.Add(fx)

                    dx = fx / dfdx

                    If Abs(dx) > maxDT Then dx = maxDT * Sign(dx)

                    x0 = x1

                    If cnt > 30 And Math.Sign(fx) <> Math.Sign(fx_ant) Then

                        'oscillating around the solution.

                        Dim bmin As New Brent

                        Dim interp As New MathNet.Numerics.Interpolation.BulirschStoerRationalInterpolation(xvals.ToArray(), fxvals.ToArray())

                        x1 = bmin.BrentOpt2(xvals.Min, xvals.Max, 5, 0.01, 100,
                                            Function(tval)
                                                Return interp.Interpolate(tval)
                                            End Function)

                        Exit Do

                    Else

                        x1 = x1 - dx

                    End If

                    If x1 < 0 Then
                        Throw New Exception("PS Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                    End If

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", x1))

                    cnt += 1

                    IObj2?.Close()

                Loop Until cnt > 100 Or Double.IsNaN(x1)

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > 100 Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            If Double.IsNaN(T) Or T <= Tmin Or T >= Tmax Then
                Dim ex As New Exception("PS Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                Throw ex
            End If

            Dim tmp = Me.Flash_PT(Vz, P, T, PP, ReuseKI, Ki)
            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, Sx, Vs}

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Throw New Exception("Unsupported Flash Specification (TVF) for Material Streams with a Forced Phase/State.")

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Throw New Exception("Unsupported Flash Specification (PVF) for Material Streams with a Forced Phase/State.")

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal Type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", "PH Flash Objective Function (Error)", "Pressure-Enthalpy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified enthalpies.")

            IObj?.SetCurrent()

            Dim n As Integer = Vz.Length - 1
            Dim L1, L2, V, Vx1(), Vx2(), Vy(), Sx, Vs(), T As Double

            Dim tmp = Me.Flash_PT(Vz, P, X, PP, ReuseKi, Ki)
            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)
            T = X

            Dim _Hv, _Hl1, _Hl2, _Hs As Double

            _Hv = 0.0#
            _Hl1 = 0.0#
            _Hl2 = 0.0#
            _Hs = 0.0

            If V > 0 Then _Hv = PP.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L1 > 0 Then _Hl1 = PP.DW_CalcEnthalpy(Vx1, T, P, State.Liquid)
            If L2 > 0 Then _Hl2 = PP.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
            If Sx > 0 Then _Hs = PP.DW_CalcEnthalpy(Vs, T, P, State.Solid)

            Dim mmg, mml, mml2, mms As Double
            mmg = PP.AUX_MMM(Vy)
            mml = PP.AUX_MMM(Vx1)
            mml2 = PP.AUX_MMM(Vx2)
            mms = PP.AUX_MMM(Vs)

            Dim herr = Hf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hv -
                (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl1 -
                (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl2 -
                (mms * Sx / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hs

            OBJ_FUNC_PH_FLASH = {herr, T, V, L1, Vy, Vx1}

            IObj?.Paragraphs.Add(String.Format("Specified Enthalpy: {0} kJ/kg", Hf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/kg", herr))

            IObj?.Close()

            WriteDebugInfo("PH Flash [NL]: Current T = " & T & ", Current H Error = " & herr)

            If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal Type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", "PS Flash Objective Function (Error)", "Pressure-Entropy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified entropies.")

            IObj?.SetCurrent()

            Dim n = Vz.Length - 1
            Dim L1, L2, V, Vx1(), Vx2(), Vy(), Sx, Vs(), T As Double

            Dim tmp = Me.Flash_PT(Vz, P, X, PP, ReuseKi, Ki)
            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)
            T = X

            Dim _Sv, _Sl1, _Sl2, _Ss As Double

            _Sv = 0.0#
            _Sl1 = 0.0#
            _Sl2 = 0.0#
            _Ss = 0.0

            If V > 0 Then _Sv = PP.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L1 > 0 Then _Sl1 = PP.DW_CalcEntropy(Vx1, T, P, State.Liquid)
            If L2 > 0 Then _Sl2 = PP.DW_CalcEntropy(Vx2, T, P, State.Liquid)
            If Sx > 0 Then _Ss = PP.DW_CalcEntropy(Vs, T, P, State.Solid)

            Dim mmg, mml, mml2, mms As Double

            mmg = PP.AUX_MMM(Vy)
            mml = PP.AUX_MMM(Vx1)
            mml2 = PP.AUX_MMM(Vx2)
            mms = PP.AUX_MMM(Vs)

            Dim serr = Sf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sv -
                (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl1 -
                (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl2 -
                (mms * Sx / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Ss

            OBJ_FUNC_PS_FLASH = {serr, T, V, L1, Vy, Vx1}

            IObj?.Paragraphs.Add(String.Format("Specified Entropy: {0} kJ/[kg.K]", Sf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", serr))

            IObj?.Close()

            WriteDebugInfo("PS Flash [NL]: Current T = " & T & ", Current S Error = " & serr)

            If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

        End Function

        Function Herror(ByVal type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object
            Return OBJ_FUNC_PH_FLASH(type, X, P, Vz, PP, ReuseKi, Ki)
        End Function

        Function Serror(ByVal type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object
            Return OBJ_FUNC_PS_FLASH(type, X, P, Vz, PP, ReuseKi, Ki)
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace
