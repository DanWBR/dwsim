'    DWSIM Nested Loops Global Flash Algorithm (SVLLE)
'    Copyright 2018 Daniel Wagner O. de Medeiros
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

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    Public Class NestedLoopsSVLLE

        Inherits FlashAlgorithm

        Dim nl1 As New NestedLoops
        Dim nl2 As New NestedLoops3PV3
        Dim nl3 As New NestedLoopsSLE With {.SolidSolution = False}

        Public Sub New()
            MyBase.New
            Order = 0
        End Sub
        Public Sub ClearEstimates()
            nl2?.ClearEstimates()
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Nested_Loops_SVLLE
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "Global Equilibrium Flash Algorithm, can calculate equilibria between one solid, one vapor and two liquid phases (SVLLE)."
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Nested Loops SVLLE"
            End Get
        End Property

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function Flash_PT(Vz() As Double, P As Double, T As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            nl1.FlashSettings = FlashSettings
            nl2.FlashSettings = FlashSettings

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT", Name & " (PT Flash)", "Pressure-Temperature Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            Dim V, L1, L2, S, Vy(), Vx1(), Vx2(), Vs() As Double

            'Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

            Dim result As Object = nl1.Flash_PT(Vz, P, T, PP)

            IObj?.SetCurrent

            L1 = result(0)
            V = result(1)
            Vx1 = result(2)
            Vy = result(3)
            Vx2 = PP.RET_NullVector
            Vs = PP.RET_NullVector

            Dim GoneThrough As Boolean = False

            If L1 = 0 And (FlashSettings(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability)) Then

                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    Dim n = Vz.Length - 1

                    Dim nlflash As New NestedLoops()

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
                            Dim r2 = nlflash.Flash_PT(Vz, P, T, PP, True, Vy.DivideY(tcomp))
                            results.Add(r2)
                        Catch ex As Exception
                        End Try
                    Next

                    If results.Where(Function(r) r(0) > 0.0).Count > 0 Then

                        Dim validresult = results.Where(Function(r) r(0) > 0.0).First

                        L1 = validresult(0)
                        V = validresult(1)
                        Vx1 = validresult(2)
                        Vy = validresult(3)

                        result = New Object() {L1, V, Vx1, Vy, 0, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                        GoneThrough = True

                    End If

                End If

            End If

            If L1 > 0 Then

                Dim stresult = StabTest(T, P, Vx1, PP.RET_VTC, PP)

                IObj?.SetCurrent

                If stresult(0) = False And Not GoneThrough Then

                    ' liquid phase NOT stable. proceed to three-phase flash.

                    Dim n As Integer = Vz.Length - 1
                    Dim k As Integer = 0

                    Dim vx2est(n), fcl(n), fcv(n) As Double
                    Dim m As Double = UBound(stresult(1), 1)
                    Dim gl, gli As Double

                    gli = 0
                    For j = 0 To m
                        For i = 0 To n
                            vx2est(i) = stresult(1)(j, i)
                        Next
                        IObj?.SetCurrent
                        fcl = PP.DW_CalcFugCoeff(vx2est, T, P, State.Liquid)
                        gl = 0.0#
                        For i = 0 To n
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

                    Dim vx1e(Vz.Length - 1), vx2e(Vz.Length - 1) As Double

                    Dim maxl As Double = MathEx.Common.Max(vx2est)
                    Dim imaxl As Integer = Array.IndexOf(vx2est, maxl)

                    V = result(1)
                    L2 = result(3)(imaxl)
                    L1 = 1 - L2 - V

                    If L1 < 0.0# Then
                        L1 = Abs(L1)
                        L2 = 1 - L1 - V
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

                    Dim sumvx2 As Double
                    For i = 0 To n
                        sumvx2 += Abs(vx1e(i))
                    Next

                    For i = 0 To n
                        vx1e(i) = Abs(vx1e(i)) / sumvx2
                    Next

                    result = nl2.Flash_PT_3P(Vz, V, L1, L2, Vy, vx1e, vx2est, P, T, PP)

                    IObj?.SetCurrent

                    L1 = result(0)
                    V = result(1)
                    Vx1 = result(2)
                    Vy = result(3)
                    L2 = result(5)
                    Vx2 = result(6)

                    If L1 = 0.0 And L2 > 0.0 Then
                        L1 = L2
                        L2 = 0.0
                        Vx1 = Vx2
                        Vx2 = PP.RET_NullVector
                    End If

                End If

                IObj?.SetCurrent

                If PP.RET_VTF.SumY > 0.0 OrElse PP.ForcedSolids.Count > 0 Then

                    result = nl3.Flash_SL(Vx1, P, T, PP)

                    IObj?.SetCurrent

                    'Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}

                    S = result(1) * L1
                    L1 = result(0) * L1

                    Vx1 = result(3)
                    Vs = result(4)

                End If

                If L2 > 0 Then

                    If PP.RET_VTF.SumY > 0.0 OrElse PP.ForcedSolids.Count > 0 Then

                        result = nl3.Flash_SL(Vx2, P, T, PP)

                        IObj?.SetCurrent

                        'Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}

                        Vx2 = result(3)
                        Vs = Vs.MultiplyConstY(S).AddY(DirectCast(result(4), Double()).MultiplyConstY(result(1))).NormalizeY()

                        S = S + result(1) * L2
                        L2 = result(0) * L2

                    End If

                End If


            Else

                IObj?.SetCurrent

                'Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                result = nl3.Flash_PT(Vz, P, T, PP)

                IObj?.SetCurrent

                L1 = result(0)
                V = result(1)
                Vx1 = result(2)
                Vy = result(3)
                Vs = result(8)
                S = result(7)

            End If

            d2 = Date.Now

            dt = d2 - d1

            IObj?.Paragraphs.Add("PT Flash [NL-SVLLE]: Converged successfully. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, 0, L2, Vx2, S, Vs}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            nl1 = New NestedLoops
            nl2 = New NestedLoops3PV3
            nl3 = New NestedLoopsSLE With {.SolidSolution = False}

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", Name & " (PH Flash)", "Pressure-Enthalpy Flash Algorithm Routine")

            IObj?.Paragraphs.Add("The PH Flash uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall enthalpy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim d1, d2 As Date, dt As TimeSpan
            Dim j, n, ecount As Integer

            d1 = Date.Now

            n = Vz.Length - 1

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax, epsilon(2) As Double

            Tmax = 10000.0#
            Tmin = 20.0#

            epsilon(0) = 0.1
            epsilon(1) = 1
            epsilon(2) = 10

            Dim fx, fx2, dfdx, x1, dx, T As Double

            Dim cnt As Integer

            If Tref = 0 Then Tref = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} kJ/kg", H))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", Tref))

            For j = 0 To 1

                cnt = 0
                x1 = Tref

                Do

                    IObj?.SetCurrent()

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PH", "PH Flash Newton Iteration", "Pressure-Enthalpy Flash Algorithm Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    IObj2?.SetCurrent()
                    fx = H_Error(x1, H, P, Vz, PP)
                    IObj2?.SetCurrent()
                    fx2 = H_Error(x1 + epsilon(j), H, P, Vz, PP)

                    IObj2?.Paragraphs.Add(String.Format("Current Enthalpy error: {0}", fx))

                    If Abs(fx) < tolEXT Then Exit Do

                    dfdx = (fx2 - fx) / epsilon(j)
                    dx = fx / dfdx

                    x1 = x1 - dx

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", T))

                    cnt += 1

                    IObj2?.Close()

                Loop Until cnt > maxitEXT Or Double.IsNaN(x1)

                IObj?.Paragraphs.Add(String.Format("The PH Flash algorithm converged in {0} iterations. Final Temperature value: {1} K", cnt, T))

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > maxitEXT Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            If Double.IsNaN(T) Or cnt > maxitEXT Then Throw New Exception("PH Flash [NL3PV3]: Invalid result: Temperature did not converge.")

            IObj?.SetCurrent()

            Dim V, L1, L2, Vy(), Vx1(), Vx2(), Vs(), Ki(), S As Double
            Dim tmp As Object = Flash_PT(Vz, P, T, PP)
            'Return New Object() {L1, V, Vx1, Vy, 0, L2, Vx2, S, Vs}

            V = tmp(1)
            L1 = tmp(0)
            L2 = tmp(5)
            S = tmp(7)
            Vy = tmp(3)
            Vx1 = tmp(2)
            Vx2 = tmp(6)
            Vs = tmp(8)
            ecount = tmp(4)

            Ki = Vy.DivideY(Vx1)

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [NL-SVLLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, S, Vs}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            nl1 = New NestedLoops
            nl2 = New NestedLoops3PV3
            nl3 = New NestedLoopsSLE With {.SolidSolution = False}

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", Name & " (PS Flash)", "Pressure-Entropy Flash Algorithm Routine")

            IObj?.Paragraphs.Add("The PS Flash in fast mode uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall entropy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim d1, d2 As Date, dt As TimeSpan
            Dim j, n, ecount As Integer

            d1 = Date.Now

            n = Vz.Length - 1

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax, epsilon(1) As Double

            Tmax = 10000.0#
            Tmin = 20.0#

            epsilon(0) = 0.1
            epsilon(1) = 1

            Dim fx, fx2, dfdx, x1, dx, T As Double

            Dim cnt As Integer

            If Tref = 0 Then Tref = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Entropy: {0} kJ/kg", S))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", Tref))

            For j = 0 To 4

                cnt = 0
                x1 = Tref

                Do

                    IObj?.SetCurrent()

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PS", "PS Flash Newton Iteration", "Pressure-Entropy Flash Algorithm Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    IObj2?.SetCurrent()
                    fx = S_Error(x1, S, P, Vz, PP)
                    IObj2?.SetCurrent()
                    fx2 = S_Error(x1 + epsilon(j), S, P, Vz, PP)

                    IObj2?.Paragraphs.Add(String.Format("Current Entropy error: {0}", fx))

                    If Abs(fx) < tolEXT Then Exit Do

                    dfdx = (fx2 - fx) / epsilon(j)
                    dx = fx / dfdx

                    'If Abs(dx) > 10.0 Then dx = Sign(dx) * 10.0

                    x1 = x1 - dx

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", x1))

                    IObj2?.Close()

                    cnt += 1

                Loop Until cnt > maxitEXT Or Double.IsNaN(x1)

                IObj?.Paragraphs.Add(String.Format("The PS Flash algorithm converged in {0} iterations. Final Temperature value: {1} K", cnt, x1))

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > maxitEXT Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            If Double.IsNaN(T) Or cnt > maxitEXT Then Throw New Exception("PS Flash [NL-3PV3]: Invalid result: Temperature did not converge.")

            IObj?.SetCurrent()

            Dim V, L1, L2, Vy(), Vx1(), Vx2(), Vs(), Ki(), Ss As Double
            Dim tmp As Object = Flash_PT(Vz, P, T, PP)
            'Return New Object() {L1, V, Vx1, Vy, 0, L2, Vx2, S, Vs}

            V = tmp(1)
            L1 = tmp(0)
            L2 = tmp(5)
            Ss = tmp(7)
            Vy = tmp(3)
            Vx1 = tmp(2)
            Vx2 = tmp(6)
            Vs = tmp(8)
            ecount = tmp(4)

            Ki = Vy.DivideY(Vx1)

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [NL-SVLLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, Ss, Vs}

        End Function

        Function H_Error(ByVal T As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Double(), PP As PropertyPackage) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", "PH Flash Objective Function (Error)", "Pressure-Enthalpy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified enthalpies.")

            IObj?.SetCurrent()

            Dim tmp = Me.Flash_PT(Vz, P, T, PP)

            Dim n = Vz.Length - 1

            Dim L1, L2, V, Vx1(), Vx2(), Vy(), Sx, Vs() As Double

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)

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

            Dim e1 = H - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hv - (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl1 - (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl2 - (mms * Sx / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hs

            IObj?.Paragraphs.Add(String.Format("Specified Enthalpy: {0} kJ/[kg]", H))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", e1))

            IObj?.Close()

            WriteDebugInfo("PH Flash [NL-3PV3]: Current T = " & T & ", Current H Error = " & e1)

            Return e1

        End Function

        Function S_Error(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Double(), PP As PropertyPackage) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", "PS Flash Objective Function (Error)", "Pressure-Entropy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified entropies.")

            IObj?.SetCurrent()

            Dim tmp = Me.Flash_PT(Vz, P, T, PP)

            Dim n = Vz.Length - 1

            Dim L1, L2, V, Vx1(), Vx2(), Vy(), Sx, Vs() As Double

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)

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

            Dim e1 = S - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sv - (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl1 - (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl2 - (mms * Sx / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Ss

            IObj?.Paragraphs.Add(String.Format("Specified Entropy: {0} kJ/[kg.K]", S))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", e1))

            IObj?.Close()

            WriteDebugInfo("PS Flash [NL-3PV3]: Current T = " & T & ", Current S Error = " & e1)

            Return e1

        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, V As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            Return nl2.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            Return nl2.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
        End Function

    End Class

End Namespace


