'    DWSIM Three-Phase Nested Loops Flash Algorithms
'    Copyright 2014 Daniel Wagner O. de Medeiros
'    Copyright 2015 Gregor Reichert
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

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    ''' <summary>
    ''' The Flash algorithms in this class are based on the Nested Loops approach to solve equilibrium calculations.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class NestedLoops3PV3

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

        Public prevres As PreviousResults

        Public Class PreviousResults
            Property V As Double
            Property L1 As Double
            Property L2 As Double
            Property Vy As Double()
            Property Vx1 As Double()
            Property Vx2 As Double()
        End Class

        Public Sub ClearEstimates()
            prevres = Nothing
        End Sub

        Sub New()
            MyBase.New()
            Order = 2
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Nested_Loops_VLLE
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Algoritmo Nested Loops estendido para calcular equilíbrio LLV."
                Else
                    Return "Extended Nested Loops Flash Algorithm for VLLE calculations."
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Nested Loops (VLLE)"
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT", Name & " (PT Flash)", "Pressure-Temperature Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add("This routine tries to find the compositions of a liquid and a vapor phase at equilibrium by solving the Rachford-Rice equation using a newton convergence approach.")

            IObj?.Paragraphs.Add("The Rachford-Rice equation is")

            IObj?.Paragraphs.Add("<math>\sum_i\frac{z_i \, (K_i - 1)}{1 + \beta \, (K_i - 1)}= 0</math>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<math_inline>z_{i}</math_inline> is the mole fraction of component i in the feed liquid (assumed to be known);")
            IObj?.Paragraphs.Add("<math_inline>\beta</math_inline> is the fraction of feed that is vaporised;")
            IObj?.Paragraphs.Add("<math_inline>K_{i}</math_inline> is the equilibrium constant of component i.")

            IObj?.Paragraphs.Add("The equilibrium constants K<sub>i</sub> are in general functions of many parameters, though the most important is arguably temperature; they are defined as:")

            IObj?.Paragraphs.Add("<math>y_i = K_i \, x_i</math>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<math_inline>x_i</math_inline> is the mole fraction of component i in liquid phase;")
            IObj?.Paragraphs.Add("<math_inline>y_i</math_inline> is the mole fraction of component i in gas phase.")

            IObj?.Paragraphs.Add("Once the Rachford-Rice equation has been solved for <math_inline>\beta</math_inline>, the compositions x<sub>i</sub> and y<sub>i</sub> can be immediately calculated as:")

            IObj?.Paragraphs.Add("<math>x_i =\frac{z_i}{1+\beta(K_i-1)}\\y_i=K_i\,x_i</math>")

            IObj?.Paragraphs.Add("The Rachford - Rice equation can have multiple solutions for <math_inline>\beta</math_inline>, at most one of which guarantees that all <math_inline>x_i</math_inline> and <math_inline>y_i</math_inline> will be positive. In particular, if there is only one <math_inline>\beta</math_inline> for which:")
            IObj?.Paragraphs.Add("<math>\frac{1}{1-K_\text{max}}=\beta_\text{min}<\beta<\beta_\text{max}=\frac{1}{1-K_\text{min}}</math>")
            IObj?.Paragraphs.Add("then that <math_inline>\beta</math_inline> is the solution; if there are multiple  such <math_inline>\beta</math_inline>s, it means that either <math_inline>K_{max}<1</math_inline> or <math_inline>K_{min}>1</math_inline>, indicating respectively that no gas phase can be sustained (and therefore <math_inline>\beta=0</math_inline>) or conversely that no liquid phase can exist (and therefore <math_inline>\beta=1</math_inline>).")

            IObj?.Paragraphs.Add("DWSIM initializes the current calculation with ideal K-values estimated from vapor pressure data for each compound, or by using previously calculated values from an earlier solution.")

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, k As Integer

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

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
                    Vp(i) = Vpc(i) * Exp(5.37 * (1 + Vw(i)) * (1 - VTc(i) / T))
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                For i = 0 To n
                    IObj?.SetCurrent()
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
                IObj?.SetCurrent()
                Px = PP.AUX_PVAPM(T)
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

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimates for K: {0}", Ki.ToMathArrayString))

            Dim result As Object = Nothing

            If Not prevres Is Nothing AndAlso prevres.L2 = 0.0 Then

                V = prevres.V
                L = prevres.L1
                Vy = prevres.Vy
                Vx = prevres.Vx1

            Else

                If Not prevres Is Nothing Then

                    'jump to 3pflash

                    IObj?.Paragraphs.Add("The algorithm will now move to the VLLE part. First it checks if there is a liquid phase. If yes, then it calls the liquid phase stability test algorithm to see if a second liquid phase can form at the current conditions.")

                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Vapor Phase Molar Fraction (V): {0}", prevres.V))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 1 Molar Fraction (L1): {0}", prevres.L1))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 2 Molar Fraction (L2): {0}", prevres.L2))

                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Vapor Phase Molar Composition: {0}", prevres.Vy.ToMathArrayString))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 1 Molar Composition: {0}", prevres.Vx1.ToMathArrayString))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 2 Molar Composition: {0}", prevres.Vx2.ToMathArrayString))

                    IObj?.SetCurrent

                    result = Flash_PT_3P(Vz, prevres.V, prevres.L1, prevres.L2, prevres.Vy, prevres.Vx1, prevres.Vx2, P, T, PP)

                    GoTo out2

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

            End If

            IObj?.Paragraphs.Add(String.Format("Initial estimate for V: {0}", V))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for L (1-V): {0}", L))

            IObj?.Paragraphs.Add(String.Format("Initial estimate for y: {0}", Vy.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for x: {0}", Vx.ToMathArrayString))

            ecount = 0
            Dim convergiu = 0

            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PT", "PT Flash Newton Iteration", "Pressure-Temperature Flash Algorithm Convergence Iteration Step")

                IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current values of y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                IObj2?.SetCurrent()

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

                IObj2?.Paragraphs.Add(String.Format("y values (vapor phase composition) where updated. Current values: {0}", Vy.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("x values (liquid phase composition) where updated. Current values: {0}", Vx.ToMathArrayString))

                Dim e1 As Double = 0
                Dim e2 As Double = 0
                Dim e3 As Double = 0
                i = 0
                Do
                    e1 = e1 + Math.Abs(Vx(i) - Vx_ant(i))
                    e2 = e2 + Math.Abs(Vy(i) - Vy_ant(i))
                    i = i + 1
                Loop Until i = n + 1

                e3 = (V - Vant)

                IObj2?.Paragraphs.Add(String.Format("Current Vapor Fraction (<math_inline>\beta</math_inline>) error: {0}", e3))

                If Double.IsNaN(Math.Abs(e1) + Math.Abs(e2)) Then

                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                ElseIf Math.Abs(e3) < itol And (e1 + e2) < itol And ecount > 0 Then

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

                    IObj2?.Paragraphs.Add(String.Format("Current value of the Rachford-Rice error function: {0}", F))

                    If Abs(F) < etol Then Exit Do

                    Dim af, dfmin As Double

                    If ecount = 0 Then
                        dfmin = 0.1
                    ElseIf ecount = 1 Then
                        dfmin = 0.3
                    ElseIf ecount = 2 Then
                        dfmin = 0.5
                    Else
                        dfmin = 0.7
                    End If

                    af = MinimizeGibbs(dfmin, PP, T, P, L, 0, F / dF, 0, Vy, Vx, PP.RET_NullVector)

                    V = -F / dF * af + V

                    IObj2?.Paragraphs.Add(String.Format("Updated Vapor Fraction (<math_inline>\beta</math_inline>) value: {0}", V))

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

                WriteDebugInfo("PT Flash [NL-3PV3]: Iteration #" & ecount & ", VF = " & V)

                If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                IObj2?.Close()

            Loop Until convergiu = 1

            IObj?.Paragraphs.Add("The two-phase algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & F)

            Dim g2 = Gibbs(PP, T, P, L1, 0.0, Vy, Vx, PP.RET_NullVector)

            IObj?.Paragraphs.Add(String.Format("Gibbs Energy Value: {0}", g2))

            IObj?.Paragraphs.Add(String.Format("Final two-phase converged values for K: {0}", Ki.ToMathArrayString))

out:

            prevres = New PreviousResults With {.L1 = L1, .L2 = 0, .V = V, .Vy = Vy, .Vx1 = Vx, .Vx2 = PP.RET_NullVector}

            result = New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

            Dim GoneThrough As Boolean = False

            If L = 0 And (FlashSettings(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability)) Then

                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

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

                        L = validresult(0)
                        V = validresult(1)
                        Vx = validresult(2)
                        Vy = validresult(3)
                        ecount = validresult(4)

                        prevres = New PreviousResults With {.L1 = L, .L2 = 0, .V = V, .Vy = Vy, .Vx1 = Vx, .Vx2 = PP.RET_NullVector}

                        result = New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                        GoneThrough = True

                    End If

                End If

            End If

            ' check if there is a liquid phase

            IObj?.Paragraphs.Add("The algorithm will now move to the VLLE part. First it checks if there is a liquid phase. If yes, then it calls the liquid phase stability test algorithm to see if a second liquid phase can form at the current conditions.")

            If L > 0 And Not GoneThrough Then

                IObj?.Paragraphs.Add("We have a liquid phase. Checking its stability according to user specifications...")

                IObj?.Paragraphs.Add("Calling Liquid Phase Stability Test algorithm...")

                ' do a stability test in the liquid phase

                IObj?.SetCurrent

                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    IObj?.Paragraphs.Add("The liquid phase is not stable. Proceed to three-phase flash.")

                    ' liquid phase NOT stable. proceed to three-phase flash.

                    Dim vx2est(n), fcl(n), fcv(n) As Double
                    Dim m As Double = UBound(stresult(1), 1)
                    Dim gl, gli As Double

                    If StabSearchSeverity = 2 Then
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

                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Vapor Phase Molar Fraction (V): {0}", V))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 1 Molar Fraction (L1): {0}", L1))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 2 Molar Fraction (L2): {0}", L2))

                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Vapor Phase Molar Composition: {0}", Vy.ToMathArrayString))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 1 Molar Composition: {0}", vx1e.ToMathArrayString))
                    IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 2 Molar Composition: {0}", vx2est.ToMathArrayString))

                    IObj?.SetCurrent

                    result = Flash_PT_3P(Vz, V, L1, L2, Vy, vx1e, vx2est, P, T, PP)

                End If

            End If

out2:       d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [NL-3PV3]: Converged in " & result(4) & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The three-phase algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & F)

            IObj?.Close()

            Return result

        End Function

        Public Function Flash_PT_3P(ByVal Vz As Double(), ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT_3P", Name & " (Three-Phase PT Flash)", "Pressure-Temperature Three-Phase VLLE Flash Algorithm Routine")

            IObj?.Paragraphs.Add("In general VLLE with three or more components, the phase
                                rule shows that at a set temperature there will be a three-phase
                                region extending from the onset of the second liquid-phase, the
                                second or upper dew-point pressure, to the bubble-point pressure.
                                Whether the first dew-point liquid is identified as LI or as LII
                                depends upon a number of factors including pure-component
                                vapor pressures.")

            IObj?.Paragraphs.Add("As in VLE flash equations, we start with")

            IObj?.Paragraphs.Add("<m>\sum y_i=1; \sum x^I_i=1; \sum x^{II}_i=1;</m>")

            IObj?.Paragraphs.Add("and consider Rachford–Rice combinations where any one
                                of the sums is subtracted from another one of the sums. There
                                are three ways of doing this, but only two are independent. We will form two equations by first subtracting the second sum from the first and second by subtracting the third sum from the
                                first:")

            IObj?.Paragraphs.Add("<m>\sum (y_i-x^I_i)=0; \sum (y_i-x^{II}_i)=0;</m>")

            IObj?.Paragraphs.Add("These equations can be written as:")

            IObj?.Paragraphs.Add("<m>\sum y_i \left[1-\frac{1}{K^I_i})\right]=0;\sum y_i \left[1-\frac{1}{K^{II}_i})\right]=0</m>")

            IObj?.Paragraphs.Add("so we define")

            IObj?.Paragraphs.Add("<m>\beta ^j_i \equiv [1-(K^j_i)^{-1}]</m>")

            IObj?.Paragraphs.Add("so now the equilibrium equations read")

            IObj?.Paragraphs.Add("<m>\sum \beta^I_iy_i = 0;\sum \beta^{II}_iy_i = 0</m>")

            IObj?.Paragraphs.Add("Think of <mi>\beta ^j_i</mi> as a redefined K-value being <mi>\left[\frac{y_i-x_i}{y_i}\right]</mi>.")

            IObj?.Paragraphs.Add("The above equation is now combined with the material balances, which can be written conveniently as:")

            IObj?.Paragraphs.Add("<m>z_i = y_i[1-\beta ^I_iL^I-\beta ^{II}_iL^{II}]</m>")

            IObj?.Paragraphs.Add("Now the equilibrium equations become")

            IObj?.Paragraphs.Add("<m>\sum\limits_{i}{\left[\frac{\beta ^I_iz_i}{1-\beta^I_iL^I-\beta ^{II}_iL^{II}}\right] }=0</m>")

            IObj?.Paragraphs.Add("<m>\sum\limits_{i}{\left[\frac{\beta ^{II}_iz_i}{1-\beta^I_iL^I-\beta ^{II}_iL^{II}}\right] }=0</m>")

            IObj?.Paragraphs.Add("These two equations are solved for <mi>L^I</mi> and <mi>L^{II}</mi>.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vx.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Initial Estimate for Vapor Phase Molar Fraction (V): {0}", Vest))
            IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 1 Molar Fraction (L1): {0}", L1est))
            IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 2 Molar Fraction (L2): {0}", L2est))

            IObj?.Paragraphs.Add(String.Format("Initial Estimate for Vapor Phase Molar Composition: {0}", VyEST.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 1 Molar Composition: {0}", Vx1EST.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial Estimate for Liquid Phase 2 Molar Composition: {0}", Vx2EST.ToMathArrayString))

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), ui1(n), ui2(n), uic1(n), uic2(n), pi(n), Ki1(n), Ki2(n), fi(n)
            Dim b1(n), b2(n), CFL1(n), CFL2(n), CFV(n), Kil(n), L1ant, L2ant As Double
            Dim i As Integer

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            'Calculate Ki`s

            Dim alreadymt As Boolean = False

            If Settings.EnableParallelProcessing Then

                Dim task1 As Task = New Task(Sub()
                                                 Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P)
                                             End Sub)
                task1.Start()
                task2.Start()
                Task.WaitAll(task1, task2)

            Else
                IObj?.SetCurrent
                Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P)
                IObj?.SetCurrent
                Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P)
            End If

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

            Vant = 0.0#
            L1ant = 0.0#
            L2ant = 0.0#

            ecount = 0

            V = Vest
            L1 = L1est
            L2 = L2est

            WriteDebugInfo("PT Flash [NL-3PV3]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", L2 = " & L2)

            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PT", "PT VLLE Flash Newton Iteration", "Pressure-Temperature VLLE Flash Algorithm Convergence Iteration Step")

                IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current values of y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                IObj2?.Paragraphs.Add(String.Format("Current Estimate for Vapor Phase Molar Fraction (V): {0}", V))
                IObj2?.Paragraphs.Add(String.Format("Current Estimate for Liquid Phase 1 Molar Fraction (L1): {0}", L1))
                IObj2?.Paragraphs.Add(String.Format("Current Estimate for Liquid Phase 2 Molar Fraction (L2): {0}", L2))

                IObj2?.Paragraphs.Add(String.Format("Current Estimate for Vapor Phase Molar Composition: {0}", Vy.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Current Estimate for Liquid Phase 1 Molar Composition: {0}", Vx1.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Current Estimate for Liquid Phase 2 Molar Composition: {0}", Vx2.ToMathArrayString))

                IObj2?.SetCurrent()

                If Settings.EnableParallelProcessing Then
                    Dim task1 As Task = New Task(Sub() CFL1 = proppack.DW_CalcFugCoeff(Vx1, T, P, State.Liquid))
                    Dim task2 As Task = New Task(Sub() CFL2 = proppack.DW_CalcFugCoeff(Vx2, T, P, State.Liquid))
                    Dim task3 As Task = New Task(Sub() CFV = proppack.DW_CalcFugCoeff(Vy, T, P, State.Vapor))
                    task1.Start()
                    task2.Start()
                    task3.Start()
                    Task.WaitAll(task1, task2, task3)
                Else
                    IObj2?.SetCurrent()
                    CFL1 = proppack.DW_CalcFugCoeff(Vx1, T, P, State.Liquid)
                    IObj2?.SetCurrent()
                    CFL2 = proppack.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                    IObj2?.SetCurrent()
                    CFV = proppack.DW_CalcFugCoeff(Vy, T, P, State.Vapor)
                End If

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
                soma_x1 = 0.0#
                soma_x2 = 0.0#
                soma_y = 0.0#
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

                IObj2?.Paragraphs.Add(String.Format("Current phase fraction error: {0}", e3))

                If (Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e3) + Math.Abs(e2) + Math.Abs(L1ant - L1) + Math.Abs(L2ant - L2)) < 0.0000000001 Then

                    Exit Do

                ElseIf Double.IsNaN(Math.Abs(e1) + Math.Abs(e4) + Math.Abs(e2)) Then

                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))

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

                    IObj2?.Paragraphs.Add(String.Format("Equilibrium Equation 1 error: {0}", F1))
                    IObj2?.Paragraphs.Add(String.Format("Equilibrium Equation 2 error: {0}", F2))

                    If Abs(L1 * F1) + Abs(L2 * F2) < etol Then Exit Do

                    Dim MA As Mapack.Matrix = New Mapack.Matrix(2, 2)
                    Dim MB As Mapack.Matrix = New Mapack.Matrix(2, 1)
                    Dim MX As Mapack.Matrix = New Mapack.Matrix(1, 2)

                    MA(0, 0) = dF1dL1
                    MA(0, 1) = dF1dL2
                    MA(1, 0) = dF2dL1
                    MA(1, 1) = dF2dL2
                    MB(0, 0) = -F1
                    MB(1, 0) = -F2

                    Try
                        MX = MA.Solve(MB)
                    Catch ex As Exception
                        Throw New Exception("PT Flash: error calculating liquid phase fractions. Please try another flash algorithm.")
                    End Try
                    dL1 = MX(0, 0)
                    dL2 = MX(1, 0)

                    L2ant = L2
                    L1ant = L1
                    Vant = V

                    Dim df, dfmin As Double

                    If ecount < 5 Then
                        dfmin = 0.0
                    ElseIf ecount < 10 Then
                        dfmin = 0.1
                    ElseIf ecount < 15 Then
                        dfmin = 0.3
                    Else
                        dfmin = 0.5
                    End If

                    df = MinimizeGibbs(dfmin, PP, T, P, L1, L2, dL1, dL2, Vy, Vx1, Vx2)

                    L1 += -dL1 * df
                    L2 += -dL2 * df

                    If Abs(dL1 * df) > 1.0 Then L1 = L1ant
                    If Abs(dL2 * df) > 1.0 Then L2 = L2ant

                    If L1 < 0 Then L1 = 0.0
                    If L2 < 0 Then L2 = 0.0

                    V = 1 - L1 - L2

                    IObj2?.Paragraphs.Add(String.Format("Updated Estimate for Vapor Phase Molar Fraction (V): {0}", V))
                    IObj2?.Paragraphs.Add(String.Format("Updated Estimate for Liquid Phase 1 Molar Fraction (L1): {0}", L1))
                    IObj2?.Paragraphs.Add(String.Format("Updated Estimate for Liquid Phase 2 Molar Fraction (L2): {0}", L2))

                    If V <= 0.0# Or Abs(L1) > 1.0# Or Abs(L2) > 1.0# Then
                        IObj2?.Paragraphs.Add("No vapor phase in the current estimate. Switching to Simple LLE FLash Algorithm...")
                        'switch to simple LLE flash procedure.
                        Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = Vx1EST, .InitialEstimatesForPhase2 = Vx2EST, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                        IObj2?.SetCurrent
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

                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))

                IObj2?.Close()

                ecount += 1

                WriteDebugInfo("PT Flash [NL-3PV3]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", L2 = " & L2)

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

            Dim g3 = Gibbs(PP, T, P, L1, L2, Vy, Vx1, Vx2)

            IObj?.Paragraphs.Add("The three-phase algorithm converged in " & ecount & " iterations.")

            IObj?.Paragraphs.Add(String.Format("Gibbs Energy Value: {0}", g3))

            IObj?.Paragraphs.Add(String.Format("Converged Value for Vapor Phase Molar Fraction (V): {0}", V))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 1 Molar Fraction (L1): {0}", L1))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 2 Molar Fraction (L2): {0}", L2))

            IObj?.Paragraphs.Add(String.Format("Converged Value for Vapor Phase Molar Composition: {0}", Vy.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 1 Molar Composition: {0}", Vx1.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 2 Molar Composition: {0}", Vx2.ToMathArrayString))

            IObj?.Close()


            If nbp1 >= nbp2 Then
                prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}
                Return New Object() {L1, V, Vx1, Vy, ecount, L2, Vx2, 0.0#, PP.RET_NullVector}
            Else
                prevres = New PreviousResults With {.L1 = L2, .L2 = L1, .V = V, .Vy = Vy, .Vx1 = Vx2, .Vx2 = Vx1}
                Return New Object() {L2, V, Vx2, Vy, ecount, L1, Vx1, 0.0#, PP.RET_NullVector}
            End If

        End Function

        Private Function MinimizeGibbs(ByVal dfmin As Double, pp As PropertyPackage, T As Double, P As Double, _L1 As Double, _L2 As Double, dL1 As Double, dl2 As Double, _Vy As Double(), _Vx1 As Double(), _Vx2 As Double()) As Double

            Dim fcv(n), fcl(n), fcl2(n), Gm, Gv, Gl1, Gl2, _V As Double

            Dim brent As New BrentOpt.BrentMinimize

            brent.DefineFuncDelegate(Function(x)
                                         Dim l1x, l2x As Double
                                         If Settings.EnableParallelProcessing Then
                                             Dim task1 As Task = New Task(Sub() fcv = proppack.DW_CalcFugCoeff(_Vy, T, P, State.Vapor))
                                             Dim task2 As Task = New Task(Sub() fcl = proppack.DW_CalcFugCoeff(_Vx1, T, P, State.Liquid))
                                             Dim task3 As Task = New Task(Sub() fcl2 = proppack.DW_CalcFugCoeff(_Vx2, T, P, State.Liquid))
                                             task1.Start()
                                             task2.Start()
                                             task3.Start()
                                             Task.WaitAll(task1, task2, task3)
                                         Else
                                             fcv = proppack.DW_CalcFugCoeff(_Vy, T, P, State.Vapor)
                                             fcl = proppack.DW_CalcFugCoeff(_Vx1, T, P, State.Liquid)
                                             fcl2 = proppack.DW_CalcFugCoeff(_Vx2, T, P, State.Liquid)
                                         End If
                                         Gv = 0
                                         Gl1 = 0
                                         Gl2 = 0
                                         l1x = _L1 - x * dL1
                                         If l1x < 0 Then l1x = 0
                                         If l1x > 1 Then l1x = 1
                                         l2x = _L2 - x * dl2
                                         If l2x < 0 Then l2x = 0
                                         If l2x > 1 Then l2x = 1 - l1x
                                         _V = 1 - l1x - l2x
                                         For i = 0 To n
                                             If _Vy(i) <> 0 Then Gv += _Vy(i) * _V * Log(fcv(i) * _Vy(i))
                                             If _Vx1(i) <> 0 Then Gl1 += _Vx1(i) * _L1 * Log(fcl(i) * _Vx1(i))
                                             If _Vx2(i) <> 0 Then Gl2 += _Vx2(i) * _L2 * Log(fcl2(i) * _Vx2(i))
                                         Next
                                         Gm = Gv + Gl1 + Gl2
                                         Return Gm
                                     End Function)

            Dim df As Double

            Dim gmin As Double = brent.brentoptimize(dfmin, 1.0, 1.0E-20, df)

            Return df

        End Function

        Private Function Gibbs(pp As PropertyPackage, T As Double, P As Double, _L1 As Double, _L2 As Double, _Vy As Double(), _Vx1 As Double(), _Vx2 As Double()) As Double

            Dim fcv(n), fcl(n), fcl2(n), Gm, Gv, Gl1, Gl2, _V As Double

            If Settings.EnableParallelProcessing Then
                Dim task1 As Task = New Task(Sub() fcv = proppack.DW_CalcFugCoeff(_Vy, T, P, State.Vapor))
                Dim task2 As Task = New Task(Sub() fcl = proppack.DW_CalcFugCoeff(_Vx1, T, P, State.Liquid))
                Dim task3 As Task = New Task(Sub() fcl2 = proppack.DW_CalcFugCoeff(_Vx2, T, P, State.Liquid))
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)
            Else
                fcv = proppack.DW_CalcFugCoeff(_Vy, T, P, State.Vapor)
                fcl = proppack.DW_CalcFugCoeff(_Vx1, T, P, State.Liquid)
                fcl2 = proppack.DW_CalcFugCoeff(_Vx2, T, P, State.Liquid)
            End If
            Gv = 0
            Gl1 = 0
            Gl2 = 0
            _V = 1 - _L1 - _L2
            For i = 0 To n
                If _Vy(i) <> 0 Then Gv += _Vy(i) * _V * Log(fcv(i) * _Vy(i))
                If _Vx1(i) <> 0 Then Gl1 += _Vx1(i) * _L1 * Log(fcl(i) * _Vx1(i))
                If _Vx2(i) <> 0 Then Gl2 += _Vx2(i) * _L2 * Log(fcl2(i) * _Vx2(i))
            Next
            Gm = Gv + Gl1 + Gl2

            Return Gm

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            prevres = Nothing

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", Name & " (PH Flash)", "Pressure-Enthalpy Flash Algorithm Routine")

            IObj?.Paragraphs.Add("The PH Flash uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall enthalpy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, n, ecount As Integer

            d1 = Date.Now

            n = Vz.Length - 1

            proppack = PP
            Hf = H
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

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

            Dim fx, fx2, dfdx, x1, dx As Double

            Dim cnt As Integer

            If Tref = 0 Then Tref = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} kJ/kg", H))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", T))

            For j = 0 To 1

                cnt = 0
                x1 = Tref

                Do

                    IObj?.SetCurrent()

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PH", "PH Flash Newton Iteration", "Pressure-Enthalpy Flash Algorithm Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    IObj2?.SetCurrent()
                    fx = Herror(x1, {P, Vz, PP})
                    IObj2?.SetCurrent()
                    fx2 = Herror(x1 + epsilon(j), {P, Vz, PP})

                    IObj2?.Paragraphs.Add(String.Format("Current Enthalpy error: {0}", fx))

                    If Abs(fx) < tolEXT Then Exit Do

                    dfdx = (fx2 - fx) / epsilon(j)
                    dx = fx / dfdx

                    'If Abs(dx) > 10.0 Then dx = Sign(dx) * 10.0

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

            If Double.IsNaN(T) Or cnt > maxitEXT Then

alt:
                Dim bo As New BrentOpt.Brent
                bo.DefineFuncDelegate(AddressOf Herror)
                WriteDebugInfo("PH Flash [NL3PV3]: Newton's method failed. Starting fallback Brent's method calculation for " & Tmin & " <= T <= " & Tmax)

                T = bo.BrentOpt(Tmin, Tmax, 25, tolEXT, maxitEXT, {P, Vz, PP})

            End If

            If T <= Tmin Or T >= Tmax Then Throw New Exception("PH Flash [NL3PV3]: Invalid result: Temperature did not converge.")

            IObj?.SetCurrent()
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

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            prevres = Nothing

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", Name & " (PS Flash)", "Pressure-Entropy Flash Algorithm Routine")

            IObj?.Paragraphs.Add("The PS Flash in fast mode uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall entropy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, j, n, ecount As Integer

            d1 = Date.Now

            n = Vz.Length - 1

            proppack = PP
            Sf = S
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax, epsilon(4) As Double

            Tmax = 10000.0#
            Tmin = 20.0#

            epsilon(0) = 0.001
            epsilon(1) = 0.01
            epsilon(2) = 0.1
            epsilon(3) = 1
            epsilon(4) = 10

            Dim fx, fx2, dfdx, x1, dx As Double

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
                    fx = Serror(x1, {P, Vz, PP})
                    IObj2?.SetCurrent()
                    fx2 = Serror(x1 + epsilon(j), {P, Vz, PP})

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

            If Double.IsNaN(T) Or cnt > maxitEXT Then

alt:
                Dim bo As New BrentOpt.Brent
                bo.DefineFuncDelegate(AddressOf Serror)
                WriteDebugInfo("PS Flash [NL-3PV3]: Newton's method failed. Starting fallback Brent's method calculation for " & Tmin & " <= T <= " & Tmax)

                T = bo.BrentOpt(Tmin, Tmax, 25, tolEXT, maxitEXT, {P, Vz, PP})

            End If

            If T <= Tmin Or T >= Tmax Then Throw New Exception("PS Flash [NL-3PV3]: Invalid result: Temperature did not converge.")


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

            WriteDebugInfo("PS Flash [NL-3PV3]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", "PH Flash Objective Function (Error)", "Pressure-Enthalpy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified enthalpies.")

            IObj?.SetCurrent()

            Dim tmp = Me.Flash_PT(Vz, Pf, T, proppack)

            Dim n = Vz.Length - 1

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

            IObj?.Paragraphs.Add(String.Format("Specified Enthalpy: {0} kJ/kg", Hf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/kg", herr))

            IObj?.Close()

            WriteDebugInfo("PH Flash [NL3P]: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", "PS Flash Objective Function (Error)", "Pressure-Entropy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified entropies.")

            IObj?.SetCurrent()

            Dim tmp = Me.Flash_PT(Vz, Pf, T, proppack)

            Dim n = Vz.Length - 1

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

            IObj?.Paragraphs.Add(String.Format("Specified Entropy: {0} kJ/[kg.K]", Sf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", serr))

            IObj?.Close()

            WriteDebugInfo("PS Flash [NL-3PV3]: Current T = " & T & ", Current S Error = " & serr)

        End Function

        Function Herror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PH_FLASH(Tt, Hf, Pf, fi)
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, Pf, fi)
        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_TV", Name & " (TV Flash)", "Temperature/Vapor Fraction Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add("This routine calculates the pressure at which the specified mixture composition finds itself in vapor-liquid equilibrium with a vapor phase mole fraction equal to V at the specified T.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} Pa", T))
            IObj?.Paragraphs.Add(String.Format("Vapor Mole Fraction: {0} ", V))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i As Integer

            n = Vz.Length - 1

            d1 = Date.Now

            Dim _nl As New NestedLoops

            Dim result As Object = _nl.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)

            P = result(4)

            If result(0) > 0 Then

                IObj?.SetCurrent
                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    If Not prevres Is Nothing Then

                        result = Flash_TV_3P(Vz, prevres.V, prevres.L1, prevres.L2, prevres.Vy, prevres.Vx1, prevres.Vx2, T, V, P, PP)

                    Else

                        Dim vx2est(n), fcl(n), fcv(n) As Double
                        Dim m As Double = UBound(stresult(1), 1)

                        For i = 0 To Vz.Length - 1
                            vx2est(i) = stresult(1)(m, i)
                        Next

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

                        'do a simple LLE calculation to get initial estimates.
                        Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = result(2), .InitialEstimatesForPhase2 = vx2est, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                        IObj?.SetCurrent
                        Dim resultL As Object = slle.Flash_PT(result(2), P, T, PP)

                        L1 = resultL(0)
                        L2 = resultL(5)
                        Vx1 = resultL(2)
                        Vx2 = resultL(6)

                        IObj?.SetCurrent
                        result = Flash_TV_3P(Vz, result(1), result(0) * L1, result(0) * L2, result(3), Vx1, Vx2, T, V, result(4), PP)

                    End If

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("TV Flash [NL-3PV3]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add(String.Format("Final converged value for P: {0}", P))

            IObj?.Close()

            Return result

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PV", Name & " (PV Flash)", "Pressure/Vapor Fraction Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add("This routine calculates the temperature at which the specified mixture composition finds itself in vapor-liquid equilibrium with a vapor phase mole fraction equal to V at the specified P.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Vapor Mole Fraction: {0} ", V))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i As Integer

            n = Vz.Length - 1

            d1 = Date.Now

            Dim _nl As New NestedLoops

            IObj?.SetCurrent
            Dim result As Object = _nl.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)

            T = result(4)

            If result(0) > 0.0 Then

                IObj?.SetCurrent
                Dim stresult As Object = StabTest(T, P, result(2), PP.RET_VTC, PP)

                If stresult(0) = False Then

                    If Not prevres Is Nothing Then

                        result = Flash_PV_3P(Vz, prevres.V, prevres.L1, prevres.L2, prevres.Vy, prevres.Vx1, prevres.Vx2, P, V, T, PP)

                    Else

                        Dim vx2est(n), fcl(n), fcv(n) As Double
                        Dim m As Double = UBound(stresult(1), 1)

                        For i = 0 To Vz.Length - 1
                            vx2est(i) = stresult(1)(m, i)
                        Next

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

                        'do a simple LLE calculation to get initial estimates.
                        Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = result(2), .InitialEstimatesForPhase2 = vx2est, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                        IObj?.SetCurrent
                        Dim resultL As Object = slle.Flash_PT(result(2), P, T, PP)

                        L1 = resultL(0)
                        L2 = resultL(5)
                        Vx1 = resultL(2)
                        Vx2 = resultL(6)
                        IObj?.SetCurrent
                        result = Flash_PV_3P(Vz, result(1), result(0) * L1, result(0) * L2, result(3), Vx1, Vx2, P, V, T, PP)

                    End If

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PV Flash [NL-3PV3]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add(String.Format("Final converged value for T: {0}", T))

            IObj?.Close()

            Return result

        End Function
        Public Function Flash_PV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim i As Integer

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vx1(n), Vx2(n), Vy(n), Ki1(n)
            Dim Tant, dTant, dT, DF, L1ant, L2ant, gamma1(n), gamma2(n), VL(n) As Double
            Dim Vx1ant(n), Vx2ant(n), Vyant(n), e1, e2, e3, e4 As Double

            Tant = Tref
            T = Tref
            ecount = 0
            DF = 1 'Damping Factor

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

            Vant = 0.0#
            L1ant = 0.0#
            L2ant = 0.0#

            ecount = 0

            If V < 1.0# Then L1 = L1est / (1 - V)
            If V < 1.0# Then L2 = L2est / (1 - V)

            'VL: composition of total liquid -> VX for LLE flash
            For i = 0 To n
                If L1 = 0.0# Or L2 = 0.0# Then
                    VL(i) = (Vx1(i) + Vx2(i))
                Else
                    VL(i) = (L1 * Vx1(i) + L2 * Vx2(i)) / (L1 + L2)
                End If
            Next

            VL.NormalizeY()

            Do
                L1ant = L1
                L2ant = L2
                Tant = T
                dTant = dT

                For i = 0 To n
                    Vx1ant(i) = Vx1(i)
                    Vx2ant(i) = Vx2(i)
                    Vyant(i) = Vy(i)
                    Vx1EST(i) = Vx1(i)
                    Vx2EST(i) = Vx2(i)
                Next

                'estimate liquid composiiton
                Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = Vx1EST, .InitialEstimatesForPhase2 = Vx2EST, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                Dim resultL As Object = slle.Flash_PT(VL, P, T, PP)
                L1 = resultL(0) 'phase fraction liquid/liquid
                L2 = resultL(5)
                Vx1 = resultL(2)
                Vx2 = resultL(6)
                gamma1 = resultL(9)
                gamma2 = resultL(10)

                'adjust boiling point by logarithmic interpolation
                Dim cnt As Integer
                Dim Pn, P1, P2, lnP, lnP1, lnP2, T1, T2 As Double

                lnP = Log(P)
                cnt = 0
                For i = 0 To n
                    P1 = P1 + Vx1(i) * gamma1(i) * PP.AUX_PVAPi(i, T)
                Next
                lnP1 = Log(P1)
                Do
                    cnt += 1
                    T1 = T
                    T2 = T1 + 1
                    P2 = 0
                    For i = 0 To n
                        P2 = P2 + Vx1(i) * gamma1(i) * PP.AUX_PVAPi(i, T2)
                    Next
                    lnP2 = Log(P2)
                    T = T1 + (T2 - T1) * (lnP - lnP1) / (lnP2 - lnP1)
                    Pn = 0
                    For i = 0 To n
                        Pn = Pn + Vx1(i) * gamma1(i) * PP.AUX_PVAPi(i, T)
                    Next
                    lnP1 = Log(Pn)
                    F = P - Pn
                Loop While Abs(P - Pn) > 1

                'Detect symetric oscillations in vicinity to critical point of a component
                'Do damping for new temperature in this case to avoid problems.
                dT = T - Tant
                T = Tant + DF * dT
                If Abs(dTant + dT) < 0.05 Then
                    DF *= 0.8
                End If

                'calculate new Ki's and vapour composition
                For i = 0 To n
                    Ki1(i) = gamma1(i) * PP.AUX_PVAPi(i, T) / P
                    Vy(i) = Ki1(i) * Vx1(i)
                    If VL(i) > 0 Then VL(i) = Vz(i) / (1 + V * (Vy(i) / VL(i) - 1))
                Next

                e1 = 0.0#
                e2 = 0.0#
                e3 = 0.0#
                e4 = 0.0#
                i = 0.0#
                Do
                    e1 = e1 + Math.Abs(Vx1(i) - Vx1ant(i))
                    e4 = e4 + Math.Abs(Vx2(i) - Vx2ant(i))
                    e2 = e2 + Math.Abs(Vy(i) - Vyant(i))
                    i = i + 1
                Loop Until i = n + 1
                e3 = Math.Abs(T - Tant) + Math.Abs(L1 - L1ant) + Math.Abs(L2 - L2ant)

                ecount += 1
                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))
            Loop Until (e1 + e2 + e3 + e4) < etol

out:        L1 = L1 * (1 - V) 'calculate global phase fractions
            L2 = L2 * (1 - V)

            WriteDebugInfo("PV Flash [NL-3PV3]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", T = " & T)

            prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Function Flash_TV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackage) As Object

            Dim i As Integer

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            ReDim Vx1(n), Vx2(n), Vy(n), Ki1(n)
            Dim Pant, L1ant, L2ant, gamma1(n), gamma2(n), VL(n), VP(n) As Double
            Dim Vx1ant(n), Vx2ant(n), Vyant(n), e1, e2, e3, e4 As Double

            For i = 0 To n
                VP(i) = PP.AUX_PVAPi(i, T)

                If Vz(i) <> 0 Then
                    Vy(i) = VyEST(i)
                    Vx1(i) = Vx1EST(i)
                    Vx2(i) = Vx2EST(i)
                Else
                    Vy(i) = 0
                    Vx1(i) = 0
                    Vx2(i) = 0
                End If
            Next

            Vant = 0.0#
            L1ant = 0.0#
            L2ant = 0.0#

            ecount = 0

            L1 = L1est / (1 - V)
            L2 = L2est / (1 - V)

            'VL: composition of total liquid -> VX for LLE flash
            For i = 0 To n
                VL(i) = (L1 * Vx1(i) + L2 * Vx2(i)) / (L1 + L2)
            Next

            Do
                L1ant = L1
                L2ant = L2
                Pant = P
                For i = 0 To n
                    Vx1ant(i) = Vx1(i)
                    Vx2ant(i) = Vx2(i)
                    Vyant(i) = Vy(i)
                    Vx1EST(i) = Vx1(i)
                    Vx2EST(i) = Vx2(i)
                Next

                'estimate liquid composiiton
                Dim slle As New SimpleLLE() With {.InitialEstimatesForPhase1 = Vx1EST, .InitialEstimatesForPhase2 = Vx2EST, .UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True}
                Dim resultL As Object = slle.Flash_PT(VL, P, T, PP)
                L1 = resultL(0) 'phase fraction liquid/liquid
                L2 = resultL(5)
                Vx1 = resultL(2)
                Vx2 = resultL(6)
                gamma1 = resultL(9)
                gamma2 = resultL(10)

                'calculate new Ki's and vapour composition
                S = 0
                P = 0
                For i = 0 To n
                    Ki1(i) = gamma1(i) * VP(i) / Pant
                    Vy(i) = Ki1(i) * Vx1(i)
                    If VL(i) > 0 Then VL(i) = Vz(i) / (1 + V * (Vy(i) / VL(i) - 1))
                    P += Vx1(i) * gamma1(i) * VP(i)
                    S += VL(i)
                Next

                'adjust total liquid composition
                For i = 0 To n
                    VL(i) /= S
                Next

                'calculate error
                e1 = 0.0#
                e2 = 0.0#
                e3 = 0.0#
                e4 = 0.0#
                For i = 0 To n
                    e1 += Math.Abs(Vx1(i) - Vx1ant(i))
                    e2 += Math.Abs(Vx2(i) - Vx2ant(i))
                    e3 += Math.Abs(Vy(i) - Vyant(i))
                Next
                e4 = Math.Abs(L1 - L1ant) + Math.Abs(L2 - L2ant)

                ecount += 1
                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))
            Loop Until (e1 + e2 + e3 + e4) < etol And Math.Abs(P - Pant) < 1

out:        L1 = L1 * (1 - V) 'calculate global phase fractions
            L2 = L2 * (1 - V)

            WriteDebugInfo("TV Flash [NL-3PV3]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", P = " & P)

            prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}

            Return New Object() {L1, V, Vx1, Vy, P, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property
    End Class

End Namespace
