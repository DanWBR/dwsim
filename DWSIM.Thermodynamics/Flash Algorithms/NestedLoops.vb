'    DWSIM Nested Loops Flash Algorithms
'    Copyright 2010-2015 Daniel Wagner O. de Medeiros, Gregor Reichert    
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

    ''' <summary>
    ''' The Flash algorithms in this class are based on the Nested Loops approach to solve equilibrium calculations.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class NestedLoops

        Inherits FlashAlgorithm

        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Hv0, Hvid, Hlid, Hf, Hv, Hl As Double
        Dim Sv0, Svid, Slid, Sf, Sv, Sl As Double

        Private CalculatingAzeotrope As Boolean = False

        Sub New()
            MyBase.New()
            Order = 1
        End Sub

        Public Property LimitVaporFraction As Boolean = True

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Nested_Loops_VLE
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Algoritmo Flash para equilíbrio Líquido-Vapor, baseado na equação de Rachford e Rice."
                Else
                    Return "Flash Algorithm for Vapor-Liquid Equilibria based on the Rachford-Rice VLE equations."
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Nested Loops (VLE)"
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

            Dim i, n, ecount As Integer
            Dim Pb, Pd, Pmin, Pmax, Px As Double
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, Vant As Double

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            Dim Vn(n) As String, Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki_ant(n), fi(n) As Double
            Dim VPc(n), VTc(n), Vw(n) As Double

            VPc = PP.RET_VPC()
            VTc = PP.RET_VTC()
            Vw = PP.RET_VW()
            Vn = PP.RET_VNAMES()

            fi = Vz.Clone

            'Calculate Ki`s

            If Not ReuseKI Then
                For i = 0 To n
                    If VPc(i) > 0.0# Then
                        Vp(i) = VPc(i) * Exp(5.37 * (1 + Vw(i)) * (1 - VTc(i) / T))
                    Else
                        IObj?.SetCurrent()
                        Vp(i) = PP.AUX_PVAPi(i, T)
                    End If
                Next
                Ki = Vp.MultiplyConstY(1 / P)
            Else
                For i = 0 To n
                    IObj?.SetCurrent()
                    Vp(i) = PP.AUX_PVAPi(i, T)
                    Ki(i) = PrevKi(i)
                    If Double.IsNaN(Ki(i)) Then Ki(i) = 1.0E+20
                Next
            End If

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimates for K: {0}", Ki.ToMathArrayString))

            'Estimate V

            If T > MathEx.Common.Max(PP.RET_VTC, Vz) Then
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
                For i = 0 To n
                    IObj?.SetCurrent()
                    Vp(i) = PP.AUX_PVAPi(i, T)
                Next
                Px = Vp.MultiplyY(Vz).Sum
                'Px = PP.AUX_PVAPM(T)
                If Px <= P Then
                    L = 1
                    V = 0
                    Vx = Vz
                    Vy = Vx.MultiplyY(Ki)
                    GoTo out
                Else
                    L = 0
                    V = 1
                    Vy = Vz
                    Vx = Vy.DivideY(Ki)
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

            IObj?.Paragraphs.Add(String.Format("Initial estimate for V: {0}", V))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for L (1-V): {0}", L))

            If n = 0 Then
                If Vp(0) <= P Then
                    L = 1.0#
                    V = 0.0#
                Else
                    L = 0.0#
                    V = 1.0#
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

            Vy = Vz.MultiplyY(Ki).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1)).NormalizeY
            Vx = Vy.DivideY(Ki).NormalizeY

            ecount = 0
            Dim converged As Integer = 0
            Dim F, dF, e1, e2, e3 As Double

            IObj?.Paragraphs.Add(String.Format("Initial estimates for y: {0}", Vy.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimates for x: {0}", Vx.ToMathArrayString))

            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PT", "PT Flash Newton Iteration", "Pressure-Temperature Flash Algorithm Convergence Iteration Step")

                IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current values of y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                IObj2?.SetCurrent()

                Ki_ant = Ki.Clone
                Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                IObj2?.Paragraphs.Add(String.Format("K values where updated. Current values: {0}", Ki.ToMathArrayString))

                Vy_ant = Vy.Clone
                Vx_ant = Vx.Clone

                If V = 1.0# Then
                    Vy = Vz
                    Vx = Vy.DivideY(Ki).NormalizeY
                ElseIf V = 0.0# Then
                    Vx = Vz
                    Vy = Vx.MultiplyY(Ki).NormalizeY
                Else
                    Vy = Vz.MultiplyY(Ki).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1)).NormalizeY
                    Vx = Vy.DivideY(Ki).NormalizeY
                End If

                IObj2?.Paragraphs.Add(String.Format("y values (vapor phase composition) where updated. Current values: {0}", Vy.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("x values (liquid phase composition) where updated. Current values: {0}", Vx.ToMathArrayString))

                e1 = Vx.SubtractY(Vx_ant).AbsSumY
                e2 = Vy.SubtractY(Vy_ant).AbsSumY

                e3 = (V - Vant)

                IObj2?.Paragraphs.Add(String.Format("Current Vapor Fraction (<math_inline>\beta</math_inline>) error: {0}", e3))

                If Double.IsNaN(e1 + e2) Then

                    Dim ex As New Exception(Calculator.GetLocalString("PropPack_FlashError") & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                    ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                    ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                    Throw ex

                ElseIf Math.Abs(e3) < 0.0000000001 And ecount > 0 Then

                    converged = 1

                    Exit Do

                Else

                    Vant = V

                    F = Vz.MultiplyY(Ki.AddConstY(-1).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))).SumY
                    dF = Vz.NegateY.MultiplyY(Ki.AddConstY(-1).MultiplyY(Ki.AddConstY(-1)).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1)).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))).SumY

                    IObj2?.Paragraphs.Add(String.Format("Current value of the Rachford-Rice error function: {0}", F))

                    If Abs(F) < etol / 100 Then Exit Do

                    V = -F / dF + Vant

                    IObj2?.Paragraphs.Add(String.Format("Updated Vapor Fraction (<math_inline>\beta</math_inline>) value: {0}", V))

                End If

                If LimitVaporFraction Then
                    If V < 0.0# Then V = 0.0#
                    If V > 1.0# Then V = 1.0#
                End If

                L = 1 - V

                ecount += 1

                If Double.IsNaN(V) Then
                    Dim ex As New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError") & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                    ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                    ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                    Throw ex
                End If
                If ecount > maxit_e Then
                    Dim ex As New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2") & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                    ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                    ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                    Throw ex
                End If

                WriteDebugInfo("PT Flash [NL]: Iteration #" & ecount & ", VF = " & V)

                If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                IObj2?.Close()

            Loop Until converged = 1

            If V <= 0.0# Then
                V = 0.0#
                L = 1.0#
                Vx = Vz
                Vy = Ki.MultiplyY(Vx).NormalizeY
            End If
            If V >= 1.0# Then
                V = 1.0#
                L = 0.0#
                Vy = Vz
                Vx = Vy.DivideY(Ki).NormalizeY
            End If

            d2 = Date.Now

            dt = d2 - d1

out:        WriteDebugInfo("PT Flash [NL]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & F)

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & F)

            IObj?.Paragraphs.Add(String.Format("Final converged values for K: {0}", Ki.ToMathArrayString))

            IObj?.Close()

            Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", Name & " (PH Flash)", "Pressure-Enthalpy Flash Algorithm Routine")

            IObj?.Paragraphs.Add("The PH Flash calculates the equilibrium temperature and phase distribution given the mixture's pressure and overall enthalpy.")

            IObj?.SetCurrent()

            If Me.FlashSettings(Interfaces.Enums.FlashSetting.NL_FastMode) = False OrElse PP.AUX_IS_SINGLECOMP(Phase.Mixture) Then
                IObj?.Paragraphs.Add("Using the normal version of the PH Flash Algorithm.")

                IObj?.Close()

                Return Flash_PH_2(Vz, P, H, Tref, PP, ReuseKI, PrevKi)
            Else
                IObj?.Paragraphs.Add("Using the fast version of the PH Flash Algorithm.")

                IObj?.Close()

                Return Flash_PH_1(Vz, P, H, Tref, PP, ReuseKI, PrevKi)
            End If
        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", Name & " (PS Flash)", "Pressure-Entropy Flash Algorithm Routine")

            IObj?.Paragraphs.Add("The PS Flash calculates the equilibrium temperature and phase distribution given the mixture's pressure and overall entropy.")

            IObj?.SetCurrent()

            If Me.FlashSettings(Interfaces.Enums.FlashSetting.NL_FastMode) = False OrElse PP.AUX_IS_SINGLECOMP(Phase.Mixture) Then
                IObj?.Paragraphs.Add("Using the normal version of the PS Flash Algorithm.")
                IObj?.Close()
                Return Flash_PS_2(Vz, P, S, Tref, PP, ReuseKI, PrevKi)
            Else
                IObj?.Paragraphs.Add("Using the fast version of the PS Flash Algorithm.")
                IObj?.Close()
                Return Flash_PS_1(Vz, P, S, Tref, PP, ReuseKI, PrevKi)
            End If
        End Function

        Public Function Flash_PH_1(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", Name & " (PH Flash - Fast Mode)", "Pressure-Enthalpy Flash Algorithm Routine (Fast Mode)")

            IObj?.Paragraphs.Add("The PH Flash in fast mode uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall enthalpy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim i, j, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, Pf As Double

            d1 = Date.Now

            n = Vz.Length - 1

            PP = PP
            Hf = H
            Pf = P

            Dim Vn(n) As String, Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki_ant(n), fi(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax, epsilon(4), maxDT As Double

            Tmax = 10000.0#
            Tmin = 20.0#
            maxDT = 30.0#

            epsilon(0) = 1
            epsilon(1) = 0.1
            epsilon(2) = 0.01

            Dim fx, fx2, dfdx, x1, x0, dx As Double

            Dim cnt As Integer

            If Tref = 0.0# Then Tref = 298.15
            T = Tref

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} kJ/kg", H))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", T))

            For j = 0 To 2

                cnt = 0
                x1 = Tref

                Do

                    IObj?.SetCurrent()

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PH", "PH Flash Newton Iteration", "Pressure-Enthalpy Flash Algorithm (Fast Mode) Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    If cnt < 2 Then

                        If Settings.EnableParallelProcessing Then

                            Dim task1 = Task.Factory.StartNew(Sub()
                                                                  fx = Herror("PT", x1, P, Vz, PP)(0)
                                                              End Sub,
                                                                Settings.TaskCancellationTokenSource.Token,
                                                                TaskCreationOptions.None,
                                                               Settings.AppTaskScheduler)
                            Dim task2 = Task.Factory.StartNew(Sub()
                                                                  fx2 = Herror("PT", x1 + epsilon(j), P, Vz, PP)(0)
                                                              End Sub,
                                                                Settings.TaskCancellationTokenSource.Token,
                                                                TaskCreationOptions.None,
                                                               Settings.AppTaskScheduler)
                            Task.WaitAll(task1, task2)

                        Else
                            IObj2?.SetCurrent()
                            fx = Herror("PT", x1, P, Vz, PP)(0)
                            IObj2?.SetCurrent()
                            fx2 = Herror("PT", x1 + epsilon(j), P, Vz, PP)(0)
                        End If

                        IObj2?.Paragraphs.Add(String.Format("Current Enthalpy error: {0}", fx2))

                        dfdx = (fx2 - fx) / epsilon(j)

                    Else

                        fx2 = fx
                        IObj2?.SetCurrent()
                        fx = Herror("PT", x1, P, Vz, PP)(0)

                        IObj2?.Paragraphs.Add(String.Format("Current Enthalpy error: {0}", fx))

                        dfdx = (fx - fx2) / (x1 - x0)

                    End If

                    If Abs(fx) <= tolEXT Then Exit Do

                    dx = fx / dfdx

                    If Abs(dx) > maxDT Then dx = maxDT * Sign(dx)

                    x0 = x1
                    x1 = x1 - dx

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", x1))

                    cnt += 1

                    IObj2?.Close()

                Loop Until cnt > maxitEXT Or Double.IsNaN(x1) Or x1 < 0.0#

                IObj?.Paragraphs.Add(String.Format("The PH Flash algorithm converged in {0} iterations. Final Temperature value: {1} K", cnt, x1))

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > maxitEXT Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            IObj?.Close()

            If Double.IsNaN(T) Or T <= Tmin Or T >= Tmax Or cnt > maxitEXT Or Abs(fx) > tolEXT Then
                'switch to mode 2 if it doesn't converge using fast mode.
                WriteDebugInfo("PH Flash [NL]: Didn't converge in fast mode. Switching to rigorous...")
                Return Flash_PH_2(Vz, P, H, Tref, PP, ReuseKI, PrevKi)
            Else
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
                WriteDebugInfo("PH Flash [NL]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")
                IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")
                Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            End If

        End Function

        Public Function Flash_PH_2(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", Name & " (PH Flash - Normal Mode)", "Pressure-Enthalpy Flash Algorithm Routine (Normal Mode)")

            IObj?.Paragraphs.Add("The PH Flash in normal mode calculates the enthalpy at mixture bubble and dew points, in order to determine the state of the mixture. 
                                  It then converges the temperature or vapor fraction depending on the estimated state.")

            IObj?.SetCurrent()

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, Pf As Double

            Dim resultFlash As Object
            Dim Tb, Td, Hb, Hd As Double
            Dim ErrRes As Object

            d1 = Date.Now

            n = Vz.Length - 1

            Hf = H
            Pf = P

            Dim Vn(n) As String, Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki_ant(n), fi(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax As Double

            Tmax = 10000.0#
            Tmin = 20.0#

            If Tref = 0.0# Then Tref = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} kJ/kg", H))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", T))

            'calculate dew point and boiling point

            IObj?.Paragraphs.Add(String.Format("Calculating Dew and Bubble points..."))

            Dim alreadymt As Boolean = False

            If Settings.EnableParallelProcessing Then

                Dim task1 = Task.Factory.StartNew(Sub()
                                                      Dim ErrRes1 = Herror("PV", 0, P, Vz, PP)
                                                      Hb = ErrRes1(0)
                                                      Tb = ErrRes1(1)
                                                  End Sub,
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                     Settings.AppTaskScheduler)
                Dim task2 = Task.Factory.StartNew(Sub()
                                                      Dim ErrRes2 = Herror("PV", 1, P, Vz, PP)
                                                      Hd = ErrRes2(0)
                                                      Td = ErrRes2(1)
                                                  End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                Task.WaitAll(task1, task2)

            Else
                IObj?.SetCurrent()
                ErrRes = Herror("PV", 0, P, Vz, PP)
                Hb = ErrRes(0)
                Tb = ErrRes(1)
                IObj?.SetCurrent()
                ErrRes = Herror("PV", 1, P, Vz, PP)
                Hd = ErrRes(0)
                Td = ErrRes(1)
            End If

            IObj?.Paragraphs.Add(String.Format("Calculated Bubble Temperature: {0} K", Tb))

            IObj?.Paragraphs.Add(String.Format("Calculated Dew Temperature: {0} K", Td))

            IObj?.Paragraphs.Add(String.Format("Bubble Point Enthalpy Error (Spec - Calculated): {0}", Hb))

            IObj?.Paragraphs.Add(String.Format("Dew Point Enthalpy Error (Spec - Calculated): {0}", Hd))

            If Hb > 0 And Hd < 0 Then

                IObj?.Paragraphs.Add(String.Format("Enthalpy at Bubble Point is lower than spec. Requires partial evaporation."))

                'specified enthalpy requires partial evaporation 
                'calculate vapour fraction

                Dim H1, H2, V1, V2 As Double
                ecount = 0
                V = 0
                H1 = Hb
                Do

                    ecount += 1
                    V1 = V
                    If V1 < 1 Then
                        V2 = V1 + 0.01
                    Else
                        V2 = V1 - 0.01
                    End If
                    IObj?.SetCurrent()
                    H2 = Herror("PV", V2, P, Vz, PP)(0)
                    V = V1 + (V2 - V1) * (0 - H1) / (H2 - H1)
                    If V < 0 Then V = 0.0#
                    If V > 1 Then V = 1.0#
                    IObj?.Paragraphs.Add(String.Format("Updated Vapor Fraction estimate: {0}", V))
                    IObj?.SetCurrent()
                    resultFlash = Herror("PV", V, P, Vz, PP)
                    H1 = resultFlash(0)
                    IObj?.Paragraphs.Add(String.Format("Enthalpy Error (Spec - Calculated): {0}", H1))
                Loop Until Abs(H1) < itol Or ecount > maxitEXT

                T = resultFlash(1)
                L = resultFlash(3)
                Vy = resultFlash(4)
                Vx = resultFlash(5)

                For i = 0 To n
                    Ki(i) = Vy(i) / Vx(i)
                Next

            ElseIf Hd > 0 Then

                IObj?.Paragraphs.Add(String.Format("Spec Enthalpy is higher than the calculated one at Dew Point. Single Vapor Phase detected."))

                'only gas phase
                'calculate temperature

                Dim H1, H2, T1, T2 As Double
                ecount = 0
                T = Td
                H1 = Hd
                Do
                    ecount += 1
                    T1 = T
                    T2 = T1 + 1
                    IObj?.SetCurrent()
                    H2 = Hf - PP.DW_CalcEnthalpy(Vz, T2, P, State.Vapor)
                    T = T1 + (T2 - T1) * (0 - H1) / (H2 - H1)
                    IObj?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", T))
                    IObj?.SetCurrent()
                    H1 = Hf - PP.DW_CalcEnthalpy(Vz, T, P, State.Vapor)
                    IObj?.Paragraphs.Add(String.Format("Enthalpy Error (Spec - Calculated): {0}", H1))
                Loop Until Abs(H1) < itol Or ecount > maxitEXT

                L = 0
                V = 1
                Vy = Vz.Clone
                Vx = Vz.Clone
                L = 0
                For i = 0 To n
                    Ki(i) = 1
                Next

                If T <= Tmin Or T >= Tmax Or ecount > maxitEXT Then
                    Dim ex As New Exception("PH Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                    ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                    ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                    Throw ex
                End If

            Else

                IObj?.Paragraphs.Add(String.Format("Spec Enthalpy is lower than the calculated one at Bubble Point. Liquid Phase detected."))

                'specified enthalpy requires pure liquid 
                'calculate temperature

                Dim H1, H2, T1, T2 As Double
                ecount = 0
                T = Tb
                H1 = Hb
                Do
                    ecount += 1
                    T1 = T
                    T2 = T1 - 1
                    IObj?.SetCurrent()
                    H2 = Herror("PT", T2, P, Vz, PP)(0)
                    T = T1 + (T2 - T1) * (0 - H1) / (H2 - H1)
                    IObj?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", T))
                    IObj?.SetCurrent()
                    resultFlash = Herror("PT", T, P, Vz, PP)
                    H1 = resultFlash(0)
                    IObj?.Paragraphs.Add(String.Format("Enthalpy Error (Spec - Calculated): {0}", H1))
                Loop Until Abs(H1) < itol Or ecount > maxitEXT

                V = 0
                L = resultFlash(3)
                Vy = resultFlash(4)
                Vx = resultFlash(5)

                For i = 0 To n
                    Ki(i) = Vy(i) / Vx(i)
                Next

                If T <= Tmin Or T >= Tmax Or ecount > maxitEXT Then
                    Dim ex As New Exception("PH Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToMathArrayString()))
                    ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                    ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                    Throw ex
                End If

            End If

            IObj?.Paragraphs.Add(String.Format("Final converged value for T: {0} K", T))

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [NL]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Function Flash_PS_1(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", Name & " (PS Flash - Fast Mode)", "Pressure-Entropy Flash Algorithm Routine (Fast Mode)")

            IObj?.Paragraphs.Add("The PS Flash in fast mode uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall entropy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim i, j, n, ecount As Integer
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
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax, epsilon(4) As Double

            Tmax = 10000.0#
            Tmin = 20.0#

            epsilon(0) = 1
            epsilon(1) = 0.1
            epsilon(2) = 0.01

            Dim fx, fx2, dfdx, x1, x0, dx As Double

            Dim cnt As Integer

            If Tref = 0 Then Tref = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Entropy: {0} kJ/kg", S))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", Tref))

            For j = 0 To 2

                cnt = 0
                x1 = Tref

                Do

                    IObj?.SetCurrent()

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PS", "PS Flash Newton Iteration", "Pressure-Entropy Flash Algorithm (Fast Mode) Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    If cnt < 2 Then

                        If Settings.EnableParallelProcessing Then

                            Dim task1 = Task.Factory.StartNew(Sub()
                                                                  fx = Serror("PT", x1, P, Vz, PP)(0)
                                                              End Sub,
                                                                  Settings.TaskCancellationTokenSource.Token,
                                                                  TaskCreationOptions.None,
                                                                 Settings.AppTaskScheduler)
                            Dim task2 = Task.Factory.StartNew(Sub()
                                                                  fx2 = Serror("PT", x1 + epsilon(j), P, Vz, PP)(0)
                                                              End Sub,
                                                              Settings.TaskCancellationTokenSource.Token,
                                                              TaskCreationOptions.None,
                                                             Settings.AppTaskScheduler)
                            Task.WaitAll(task1, task2)

                        Else
                            IObj2?.SetCurrent()
                            fx = Serror("PT", x1, P, Vz, PP)(0)
                            IObj2?.SetCurrent()
                            fx2 = Serror("PT", x1 + epsilon(j), P, Vz, PP)(0)
                        End If

                        IObj2?.Paragraphs.Add(String.Format("Current Entropy error: {0}", fx2))

                        dfdx = (fx2 - fx) / epsilon(j)

                    Else

                        fx2 = fx
                        IObj2?.SetCurrent()
                        fx = Serror("PT", x1, P, Vz, PP)(0)

                        IObj2?.Paragraphs.Add(String.Format("Current Entropy error: {0}", fx))

                        dfdx = (fx - fx2) / (x1 - x0)

                    End If

                    If Abs(fx) < tolEXT Then Exit Do

                    dx = fx / dfdx

                    x0 = x1
                    x1 = x1 - dx

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", x1))

                    cnt += 1

                    IObj2?.Close()

                Loop Until cnt > maxitEXT Or Double.IsNaN(x1)

                IObj?.Paragraphs.Add(String.Format("The PS Flash algorithm converged in {0} iterations. Final Temperature value: {1} K", cnt, x1))

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > maxitEXT Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            IObj?.Close()

            If Double.IsNaN(T) Or T <= Tmin Or T >= Tmax Then
                Dim ex As New Exception("PS Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                Throw ex
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

            WriteDebugInfo("PS Flash [NL]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function


        Public Function Flash_PS_2(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", Name & " (PS Flash - Normal Mode)", "Pressure-Entropy Flash Algorithm Routine (Normal Mode)")

            IObj?.Paragraphs.Add("The PS Flash in normal mode calculates the entropy at mixture bubble and dew points, in order to determine the state of the mixture. 
                                  It then converges the temperature or vapor fraction depending on the estimated state.")

            IObj?.SetCurrent()

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, Pf As Double

            Dim resultFlash As Object
            Dim Tb, Td, Sb, Sd As Double
            Dim ErrRes As Object

            d1 = Date.Now

            n = Vz.Length - 1

            Sf = S
            Pf = P

            ReDim Vn(n), Vy(n), Vp(n), Ki(n), fi(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax As Double

            Tmax = 10000.0#
            Tmin = 20.0#

            If Tref = 0.0# Then Tref = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Entropy: {0} kJ/kg", S))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", T))

            'calculate dew point and boiling point

            IObj?.Paragraphs.Add(String.Format("Calculating Dew and Bubble points..."))

            Dim alreadymt As Boolean = False

            If Settings.EnableParallelProcessing Then

                Dim task1 = Task.Factory.StartNew(Sub()
                                                      Dim ErrRes1 = Serror("PV", 0, P, Vz, PP)
                                                      Sb = ErrRes1(0)
                                                      Tb = ErrRes1(1)
                                                  End Sub,
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                     Settings.AppTaskScheduler)
                Dim task2 = Task.Factory.StartNew(Sub()
                                                      Dim ErrRes2 = Serror("PV", 1, P, Vz, PP)
                                                      Sd = ErrRes2(0)
                                                      Td = ErrRes2(1)
                                                  End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                Task.WaitAll(task1, task2)

            Else
                IObj?.SetCurrent()
                ErrRes = Serror("PV", 0, P, Vz, PP)
                Sb = ErrRes(0)
                Tb = ErrRes(1)
                IObj?.SetCurrent()
                ErrRes = Serror("PV", 1, P, Vz, PP)
                Sd = ErrRes(0)
                Td = ErrRes(1)
            End If

            Dim S1, S2, T1, T2, V1, V2 As Double

            IObj?.Paragraphs.Add(String.Format("Calculated Bubble Temperature: {0} K", Tb))

            IObj?.Paragraphs.Add(String.Format("Calculated Dew Temperature: {0} K", Td))

            IObj?.Paragraphs.Add(String.Format("Bubble Point Entropy Error (Spec - Calculated): {0}", Sb))

            IObj?.Paragraphs.Add(String.Format("Dew Point Entropy Error (Spec - Calculated): {0}", Sd))

            If Sb > 0 And Sd < 0 Then

                IObj?.Paragraphs.Add(String.Format("Entropy at Bubble Point is lower than spec. Requires partial evaporation."))

                'specified entropy requires partial evaporation 
                'calculate vapour fraction

                ecount = 0
                V = 0
                S1 = Sb
                Do
                    ecount += 1
                    V1 = V
                    If V1 < 1 Then
                        V2 = V1 + 0.01
                    Else
                        V2 = V1 - 0.01
                    End If

                    IObj?.SetCurrent()
                    S2 = Serror("PV", V2, P, Vz, PP)(0)
                    V = V1 + (V2 - V1) * (0 - S1) / (S2 - S1)
                    If V < 0 Then V = 0
                    If V > 1 Then V = 1
                    IObj?.Paragraphs.Add(String.Format("Updated Vapor Fraction estimate: {0}", V))
                    IObj?.SetCurrent()
                    resultFlash = Serror("PV", V, P, Vz, PP)
                    S1 = resultFlash(0)
                    IObj?.Paragraphs.Add(String.Format("Entropy Error (Spec - Calculated): {0}", S1))
                Loop Until Abs(S1) < itol Or ecount > maxitEXT

                T = resultFlash(1)
                L = resultFlash(3)
                Vy = resultFlash(4)
                Vx = resultFlash(5)
                For i = 0 To n
                    Ki(i) = Vy(i) / Vx(i)
                Next

            ElseIf Sd > 0 Then

                IObj?.Paragraphs.Add(String.Format("Spec Entropy is higher than the calculated one at Dew Point. Single Vapor Phase detected."))

                'only gas phase
                'calculate temperature

                ecount = 0
                T = Td
                S1 = Sd
                Do
                    ecount += 1
                    T1 = T
                    T2 = T1 + 1
                    IObj?.SetCurrent()
                    S2 = Sf - PP.DW_CalcEntropy(Vz, T2, P, State.Vapor)
                    T = T1 + (T2 - T1) * (0 - S1) / (S2 - S1)
                    IObj?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", T))
                    IObj?.SetCurrent()
                    S1 = Sf - PP.DW_CalcEntropy(Vz, T, P, State.Vapor)
                    IObj?.Paragraphs.Add(String.Format("Entropy Error (Spec - Calculated): {0}", S1))
                Loop Until Abs(S1) < itol Or ecount > maxitEXT

                L = 0
                V = 1
                Vy = Vz.Clone
                Vx = Vz.Clone
                L = 0
                For i = 0 To n
                    Ki(i) = 1
                Next

                If T <= Tmin Or T >= Tmax Then
                    Dim ex As New Exception("PS Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                    ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                    ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                    Throw ex
                End If

            Else

                IObj?.Paragraphs.Add(String.Format("Spec Entropy is lower than the calculated one at Bubble Point. Liquid Phase detected."))

                'specified enthalpy requires pure liquid 
                'calculate temperature

                ecount = 0
                T = Tb
                S1 = Sb
                Do
                    ecount += 1
                    T1 = T
                    T2 = T1 - 1
                    IObj?.SetCurrent()
                    S2 = Serror("PT", T2, P, Vz, PP)(0)
                    T = T1 + (T2 - T1) * (0 - S1) / (S2 - S1)
                    IObj?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", T))
                    IObj?.SetCurrent()
                    resultFlash = Serror("PT", T, P, Vz, PP)
                    S1 = resultFlash(0)
                    IObj?.Paragraphs.Add(String.Format("Entropy Error (Spec - Calculated): {0}", S1))
                Loop Until Abs(S1) < itol Or ecount > maxitEXT

                V = 0
                L = resultFlash(3)
                Vy = resultFlash(4)
                Vx = resultFlash(5)

                For i = 0 To n
                    Ki(i) = Vy(i) / Vx(i)
                Next

                If T <= Tmin Or T >= Tmax Then
                    Dim ex As New Exception("PS Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                    ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                    ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                    Throw ex
                End If

            End If

            IObj?.Paragraphs.Add(String.Format("Final converged value for T: {0} K", T))

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [NL]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

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

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim Pmin, Pmax, soma_x, soma_y As Double
            Dim L, Lf, Vf, P, Pf, deltaP As Double

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
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

            If Pref = 0.0# Then

                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(i, T)
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
                    Vp(i) = PP.AUX_PVAPi(i, T)
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                If Not PP.AUX_CheckTrivial(PrevKi) Then
                    For i = 0 To n
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        Ki(i) = PrevKi(i)
                    Next
                Else
                    i = 0
                    Do
                        IObj?.SetCurrent
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        Ki(i) = Vp(i) / P
                        i += 1
                    Loop Until i = n + 1
                End If
            End If

            IObj?.Paragraphs.Add(String.Format("Initial estimates for P: {0} K", P))
            IObj?.Paragraphs.Add(String.Format("Initial estimates for K: {0}", Ki.ToMathArrayString))

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

            IObj?.Paragraphs.Add(String.Format("Initial estimates for x: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimates for y: {0}", Vy.ToMathArrayString))

            If PP.AUX_IS_SINGLECOMP(Vz) Then
                WriteDebugInfo("TV Flash [NL]: Converged in 1 iteration.")
                P = 0
                For i = 0 To n
                    IObj?.SetCurrent
                    P += Vz(i) * PP.AUX_PVAPi(i, T)
                Next
                IObj?.Close()
                Return New Object() {L, V, Vx, Vy, P, 0, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            End If

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Pant, fval As Double
            Dim chk As Boolean = False

            If V = 1.0# Or V = 0.0# Then

                If V = 1.0 Then
                    IObj?.Paragraphs.Add("This is a dew point calculation (V = 1).")
                Else
                    IObj?.Paragraphs.Add("This is a bubble point calculation (V = 0).")
                End If

                ecount = 0
                Do

                    IObj?.SetCurrent

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_TV", "TV Flash Newton Iteration", "Temperature-Vapor Fraction Flash Algorithm Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current values of P, y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                    IObj2?.SetCurrent()

                    IObj2?.Paragraphs.Add(String.Format("Tentative pressure value: {0} K", P))

                    marcador3 = 0

                    Dim cont_int = 0
                    Do

                        IObj2?.SetCurrent

                        Dim IObj3 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                        Inspector.Host.CheckAndAdd(IObj3, "", "Flash_TV", "TV Flash Inner Iteration", "Temperature-Vapor Fraction Flash Algorithm Convergence Inner Iteration Step")

                        IObj3?.Paragraphs.Add(String.Format("This is the inner convergence loop iteration #{0}. DWSIM will use the current value of P to converge x and y.", ecount))

                        IObj3?.SetCurrent()

                        IObj3?.Paragraphs.Add(String.Format("Tentative value for K: {0}", Ki.ToMathArrayString))

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

                        IObj3?.Paragraphs.Add(String.Format("Updated x: {0}", Vx.ToMathArrayString))
                        IObj3?.Paragraphs.Add(String.Format("Updated y: {0}", Vy.ToMathArrayString))

                        IObj3?.Close()

                    Loop Until marcador2 = 1 Or Double.IsNaN(stmp4) Or cont_int > maxit_i

                    IObj2?.Paragraphs.Add(String.Format("Updated x: {0}", Vx.ToMathArrayString))
                    IObj2?.Paragraphs.Add(String.Format("Updated y: {0}", Vy.ToMathArrayString))

                    Dim K1(n), K2(n), dKdP(n) As Double

                    IObj?.SetCurrent
                    K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                    IObj?.SetCurrent
                    K2 = PP.DW_CalcKvalue(Vx, Vy, T, P * 1.001)

                    For i = 0 To n
                        dKdP(i) = (K2(i) - K1(i)) / (0.001 * P)
                    Next

                    IObj2?.Paragraphs.Add(String.Format("K: {0}", Ki.ToMathArrayString))

                    IObj2?.Paragraphs.Add(String.Format("dK/dP: {0}", dKdP.ToMathArrayString))

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

                        deltaP = -fval / dFdP

                        If Abs(deltaP) < etol / 1000 And ecount > 5 Then Exit Do

                        If Abs(deltaP) > 0.1 * P And ecount < 5 Then
                            P = P + Sign(deltaP) * 0.1 * P
                        Else
                            P = P + deltaP
                        End If

                    End If

                    IObj2?.Paragraphs.Add(String.Format("Pressure error: {0} K", deltaP))

                    IObj2?.Paragraphs.Add(String.Format("Updated Pressure: {0} K", P))

                    WriteDebugInfo("TV Flash [NL]: Iteration #" & ecount & ", P = " & P & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                    IObj2?.Close()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(P) = True Or ecount > maxit_e

            Else

                ecount = 0

                IObj?.SetCurrent

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_TV", "TV Flash Newton Iteration", "Temperature-Vapor Fraction Flash Algorithm Convergence Iteration Step")

                IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current values of P, y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("Tentative temperature value: {0} K", T))

                Do

                    IObj2?.SetCurrent

                    Dim IObj3 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj3, "", "Flash_TV", "TV Flash Inner Iteration", "Temperature-Vapor Fraction Flash Algorithm Convergence Inner Iteration Step")

                    IObj3?.Paragraphs.Add(String.Format("This is the inner convergence loop iteration #{0}. DWSIM will use the current value of P to converge x and y.", ecount))

                    IObj3?.SetCurrent()

                    IObj3?.Paragraphs.Add(String.Format("Tentative value for K: {0}", Ki.ToMathArrayString))

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

                    IObj2?.Paragraphs.Add(String.Format("Updated x: {0}", Vx.ToMathArrayString))
                    IObj2?.Paragraphs.Add(String.Format("Updated y: {0}", Vy.ToMathArrayString))

                    If V <= 0.5 Then

                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Ki(i) * Vx(i)
                            i = i + 1
                        Loop Until i = n + 1

                        Dim K1(n), K2(n), dKdP(n) As Double

                        IObj2?.SetCurrent

                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        IObj2?.SetCurrent

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

                        IObj2?.Paragraphs.Add(String.Format("dK/dP: {0}", dKdP.ToMathArrayString))

                    Else

                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Vy(i) / Ki(i)
                            i = i + 1
                        Loop Until i = n + 1

                        Dim K1(n), K2(n), dKdP(n) As Double

                        IObj2?.SetCurrent
                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                        IObj2?.SetCurrent
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

                        IObj2?.Paragraphs.Add(String.Format("dK/dP: {0}", dKdP.ToMathArrayString))

                    End If

                    ecount += 1

                    fval = stmp4 - 1

                    If (P - fval / dFdP) < 0 Then

                        P = (P + Pant) / 2

                    Else

                        Pant = P

                        deltaP = -fval / dFdP

                        If Abs(deltaP) < etol / 1000 And ecount > 5 Then Exit Do

                        If Abs(deltaP) > 0.1 * P And ecount < 5 Then
                            P = P + Sign(deltaP) * 0.1 * P
                        Else
                            P = P + deltaP
                        End If

                    End If

                    IObj2?.Paragraphs.Add(String.Format("Pressure error: {0} K", deltaP))

                    IObj2?.Paragraphs.Add(String.Format("Updated Pressure: {0} K", P))

                    WriteDebugInfo("TV Flash [NL]: Iteration #" & ecount & ", P = " & P & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                    IObj2?.Close()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(P) = True Or ecount > maxit_e

            End If

            d2 = Date.Now

            dt = d2 - d1

            If ecount > maxit_e Then
                Dim ex As New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2") & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                Throw ex
            End If

            If PP.AUX_CheckTrivial(Ki) Then Throw New Exception("TV Flash [NL]: Invalid result: converged to the trivial solution (P = " & P & " ).")

            WriteDebugInfo("TV Flash [NL]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add(String.Format("Final converged value for P: {0}", P))

            IObj?.Close()

            Return New Object() {L, V, Vx, Vy, P, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

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

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, Lf, Vf, T, Tf, deltaT, deltaT_ant, epsilon, df, maxdT As Double
            Dim e1 As Double

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            epsilon = Me.FlashSettings(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon).ToDoubleFromInvariant
            df = Me.FlashSettings(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor).ToDoubleFromInvariant
            maxdT = Me.FlashSettings(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange).ToDoubleFromInvariant

            n = Vz.Length - 1

            PP = PP
            Vf = V
            L = 1 - V
            Lf = 1 - Vf
            Tf = T

            Dim Vn(n) As String, Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n) As Double
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
                    'Tref += 0.8 * Vz(i) * VTc(i)
                    Tref += Vz(i) * PP.AUX_TSATi(P, i)
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
                    IObj?.SetCurrent
                    Vp(i) = PP.AUX_PVAPi(i, T)
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                If Not PP.AUX_CheckTrivial(PrevKi) And Not Double.IsNaN(PrevKi(0)) Then
                    For i = 0 To n
                        IObj?.SetCurrent
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        Ki(i) = PrevKi(i)
                    Next
                Else
                    i = 0
                    Do
                        IObj?.SetCurrent
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        Ki(i) = Vp(i) / P
                        i += 1
                    Loop Until i = n + 1
                End If
            End If

            IObj?.Paragraphs.Add(String.Format("Initial estimates for T: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Initial estimates for K: {0}", Ki.ToMathArrayString))

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                    If Double.IsInfinity(Vy(i)) Then Vy(i) = 0.0#
                    Vx(i) = Vy(i) / Ki(i)
                Else
                    Vy(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            Vx = Vx.NormalizeY()
            Vy = Vy.NormalizeY()

            IObj?.Paragraphs.Add(String.Format("Initial estimates for x: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimates for y: {0}", Vy.ToMathArrayString))

            If PP.AUX_IS_SINGLECOMP(Vz) Then
                WriteDebugInfo("PV Flash [NL]: Converged in 1 iteration.")
                T = 0
                For i = 0 To n
                    IObj?.SetCurrent
                    T += Vz(i) * PP.AUX_TSATi(P, i)
                Next
                IObj?.Close()
                If Vz.Count = 1 Then
                    Vx = New Double() {1.0}
                    Vy = New Double() {1.0}
                    Ki = New Double() {1.0}
                End If
                Return New Object() {L, V, Vx, Vy, T, 0, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            End If

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Tant, fval, fval_ant As Double
            Dim chk As Boolean = False

            If V = 1.0# Or V = 0.0# Then

                If V = 1.0 Then
                    IObj?.Paragraphs.Add("This is a dew point calculation (V = 1).")
                Else
                    IObj?.Paragraphs.Add("This is a bubble point calculation (V = 0).")
                End If

                ecount = 0
                Do

                    IObj?.SetCurrent

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PV", "PV Flash Newton Iteration", "Pressure-Vapor Fraction Flash Algorithm Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current values of T, y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                    IObj2?.SetCurrent()

                    IObj2?.Paragraphs.Add(String.Format("Tentative temperature value: {0} K", T))

                    marcador3 = 0

                    Dim cont_int = 0
                    Do

                        IObj2?.SetCurrent

                        Dim IObj3 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                        Inspector.Host.CheckAndAdd(IObj3, "", "Flash_PV", "PV Flash Inner Iteration", "Pressure-Vapor Fraction Flash Algorithm Convergence Inner Iteration Step")

                        IObj3?.Paragraphs.Add(String.Format("This is the inner convergence loop iteration #{0}. DWSIM will use the current value of T to converge x and y.", ecount))

                        IObj3?.SetCurrent()

                        IObj3?.Paragraphs.Add(String.Format("Tentative value for K: {0}", Ki.ToMathArrayString))

                        Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        marcador = 0
                        If stmp4_ant <> 0 Then
                            marcador = 1
                        End If
                        stmp4_ant = stmp4

                        If V = 0 Then
                            stmp4 = Ki.MultiplyY(Vx).SumY
                        Else
                            stmp4 = Vy.DivideY(Ki).SumY
                        End If

                        If V = 0 Then
                            Vy_ant = Vy.Clone
                            Vy = Ki.MultiplyY(Vx).MultiplyConstY(1 / stmp4)
                        Else
                            Vx_ant = Vx.Clone
                            Vx = Vy.DivideY(Ki).MultiplyConstY(1 / stmp4)
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

                        IObj3?.Paragraphs.Add(String.Format("Updated x: {0}", Vx.ToMathArrayString))
                        IObj3?.Paragraphs.Add(String.Format("Updated y: {0}", Vy.ToMathArrayString))

                        IObj3?.Close()

                    Loop Until marcador2 = 1 Or Double.IsNaN(stmp4) Or cont_int > maxit_i

                    IObj2?.Paragraphs.Add(String.Format("Updated x: {0}", Vx.ToMathArrayString))
                    IObj2?.Paragraphs.Add(String.Format("Updated y: {0}", Vy.ToMathArrayString))

                    Dim K1(n), K2(n), dKdT(n) As Double

                    IObj?.SetCurrent
                    K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                    IObj?.SetCurrent
                    K2 = PP.DW_CalcKvalue(Vx, Vy, T + epsilon, P)

                    dKdT = K2.SubtractY(K1).MultiplyConstY(1 / epsilon)

                    IObj2?.Paragraphs.Add(String.Format("K: {0}", Ki.ToMathArrayString))

                    IObj2?.Paragraphs.Add(String.Format("dK/dT: {0}", dKdT.ToMathArrayString))

                    fval_ant = fval
                    fval = stmp4 - 1

                    ecount += 1

                    i = 0
                    dFdT = 0
                    Do
                        If V = 0 Then
                            dFdT = Vx.MultiplyY(dKdT).SumY
                        Else
                            dFdT = -Vy.DivideY(Ki).DivideY(Ki).MultiplyY(dKdT).SumY
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    Tant = T
                    deltaT_ant = deltaT
                    deltaT = -df * fval / dFdT

                    IObj2?.Paragraphs.Add(String.Format("Temperature error: {0} K", deltaT))

                    If Abs(deltaT) < etol / 1000 And ecount > 5 Then Exit Do

                    If Sign(fval) <> Sign(fval_ant) And ecount > 20 And Vx.Length = 2 And Not CalculatingAzeotrope Then
                        'azeotrope
                        T = Flash_PV_Azeotrope_Temperature(Vz, P, V, Tref, PP, ReuseKI, PrevKi)
                        If V = 0 Then
                            Vy = Vx.Clone
                        Else
                            Vx = Vy.Clone
                        End If
                        Exit Do
                    Else
                        If Abs(deltaT) > maxdT Then
                            T = T + Sign(deltaT) * maxdT
                        Else
                            T = T + deltaT
                        End If
                    End If

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature: {0} K", T))

                    WriteDebugInfo("PV Flash [NL]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                    IObj2?.Close()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(T) = True Or ecount > maxit_e

            Else

                ecount = 0

                IObj?.SetCurrent

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PV", "PV Flash Newton Iteration", "Pressure-Vapor Fraction Flash Algorithm Convergence Iteration Step")

                IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current values of T, y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("Tentative temperature value: {0} K", T))

                Do

                    IObj2?.SetCurrent

                    Dim IObj3 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj3, "", "Flash_PV", "PV Flash Inner Iteration", "Pressure-Vapor Fraction Flash Algorithm Convergence Inner Iteration Step")

                    IObj3?.Paragraphs.Add(String.Format("This is the inner convergence loop iteration #{0}. DWSIM will use the current value of T to converge x and y.", ecount))

                    IObj3?.SetCurrent()

                    IObj3?.Paragraphs.Add(String.Format("Tentative value for K: {0}", Ki.ToMathArrayString))

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

                    Vx = Vx.NormalizeY()
                    Vy = Vy.NormalizeY()

                    IObj2?.Paragraphs.Add(String.Format("Updated x: {0}", Vx.ToMathArrayString))
                    IObj2?.Paragraphs.Add(String.Format("Updated y: {0}", Vy.ToMathArrayString))

                    If V <= 0.5 Then

                        stmp4 = Ki.MultiplyY(Vx).SumY

                        Dim K1(n), K2(n), dKdT(n) As Double

                        IObj2?.SetCurrent

                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        IObj2?.SetCurrent

                        K2 = PP.DW_CalcKvalue(Vx, Vy, T + epsilon, P)

                        dKdT = K2.SubtractY(K1).MultiplyConstY(1 / epsilon)

                        dFdT = Vx.MultiplyY(dKdT).SumY

                        IObj2?.Paragraphs.Add(String.Format("dK/dT: {0}", dKdT.ToMathArrayString))

                    Else

                        stmp4 = Vy.DivideY(Ki).SumY

                        Dim K1(n), K2(n), dKdT(n) As Double

                        IObj2?.SetCurrent

                        K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        IObj2?.SetCurrent

                        K2 = PP.DW_CalcKvalue(Vx, Vy, T + epsilon, P)

                        dKdT = K2.SubtractY(K1).MultiplyConstY(1 / epsilon)

                        dFdT = -Vy.DivideY(Ki).DivideY(Ki).MultiplyY(dKdT).SumY

                        IObj2?.Paragraphs.Add(String.Format("dK/dT: {0}", dKdT.ToMathArrayString))

                    End If

                    ecount += 1

                    fval = stmp4 - 1

                    Tant = T

                    deltaT = -df * fval / dFdT

                    IObj2?.Paragraphs.Add(String.Format("Temperature error: {0} K", deltaT))

                    If Abs(deltaT) < etol / 1000 And ecount > 5 Then Exit Do

                    If Abs(deltaT) > maxdT Then
                        T = T + Sign(deltaT) * maxdT
                    Else
                        T = T + deltaT
                    End If

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature: {0} K", T))

                    e1 = Vx.SubtractY(Vx_ant).AbsSumY + Vy.SubtractY(Vy_ant).AbsSumY + Math.Abs(T - Tant)

                    WriteDebugInfo("PV Flash [NL]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                    IObj2?.Close()

                Loop Until (Math.Abs(fval) < etol And e1 < etol) Or Double.IsNaN(T) = True Or ecount > maxit_e

            End If

            d2 = Date.Now

            dt = d2 - d1

            If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2") & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))

            If PP.AUX_CheckTrivial(Ki) Then
                Dim ex As New Exception("PV Flash [NL]: Invalid result: converged to the trivial solution (T = " & T & " ).")
                ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                Throw ex
            End If

            WriteDebugInfo("PV Flash [NL]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add(String.Format("Final converged value for T: {0}", T))

            IObj?.Close()

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Function Flash_PV_Azeotrope_Temperature(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Double

            Dim T, dx, validdx As New List(Of Double)
            Dim T0 As Double = Tref
            Dim xaz As Double = Vz(0)

            For i = 0 To 100 Step 10
                dx.Add(i / 100)
            Next

            CalculatingAzeotrope = True

            For Each item In dx
                Try
                    T.Add(Flash_PV(New Double() {item, 1 - item}, P, V, T0, PP, True, PrevKi)(4))
                    T0 = T.Last
                    validdx.Add(item)
                Catch ex As Exception
                End Try
            Next

            CalculatingAzeotrope = False

            Dim Taz = DWSIM.MathOps.MathEx.Interpolation.polinterpolation.nevilleinterpolation(validdx.ToArray, T.ToArray, T.Count - 1, xaz)

            Return Taz

        End Function


        Function OBJ_FUNC_PH_FLASH(ByVal Type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", "PH Flash Objective Function (Error)", "Pressure-Enthalpy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified enthalpies.")

            IObj?.SetCurrent()

            Dim n As Integer = Vz.Length - 1
            Dim L, V, Vx(), Vy(), _Hl, _Hv, T As Double

            If Type = "PT" Then
                Dim tmp = Me.Flash_PT(Vz, P, X, PP)
                L = tmp(0)
                V = tmp(1)
                Vx = tmp(2)
                Vy = tmp(3)
                T = X
            Else
                Dim tmp = Me.Flash_PV(Vz, P, X, 0.0#, PP)
                L = tmp(0)
                V = tmp(1)
                Vx = tmp(2)
                Vy = tmp(3)
                T = tmp(4)
            End If

            _Hv = 0.0#
            _Hl = 0.0#

            Dim mmg, mml As Double

            mmg = PP.AUX_MMM(Vy)
            mml = PP.AUX_MMM(Vx)

            If Settings.EnableParallelProcessing Then

                Dim t1 = Task.Factory.StartNew(Sub() If V > 0 Then _Hv = PP.DW_CalcEnthalpy(Vy, T, P, State.Vapor))

                Dim t2 = Task.Factory.StartNew(Sub() If L > 0 Then _Hl = PP.DW_CalcEnthalpy(Vx, T, P, State.Liquid))

                Task.WaitAll(t1, t2)

            Else

                IObj?.SetCurrent()

                If V > 0 Then _Hv = PP.DW_CalcEnthalpy(Vy, T, P, State.Vapor)

                IObj?.SetCurrent()
                If L > 0 Then _Hl = PP.DW_CalcEnthalpy(Vx, T, P, State.Liquid)

            End If

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L)) * _Hv - (mml * L / (mmg * V + mml * L)) * _Hl
            OBJ_FUNC_PH_FLASH = {herr, T, V, L, Vy, Vx}

            IObj?.Paragraphs.Add(String.Format("Specified Enthalpy: {0} kJ/kg", Hf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/kg", herr))

            IObj?.Close()

            WriteDebugInfo("PH Flash [NL]: Current T = " & T & ", Current H Error = " & herr)

            If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal Type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", "PS Flash Objective Function (Error)", "Pressure-Entropy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified entropies.")

            IObj?.SetCurrent()

            Dim n = Vz.Length - 1
            Dim L, V, Vx(), Vy(), _Sl, _Sv, T As Double

            If Type = "PT" Then
                Dim tmp = Me.Flash_PT(Vz, P, X, PP)
                L = tmp(0)
                V = tmp(1)
                Vx = tmp(2)
                Vy = tmp(3)
                T = X
            Else
                Dim tmp = Me.Flash_PV(Vz, P, X, 0.0#, PP)
                L = tmp(0)
                V = tmp(1)
                Vx = tmp(2)
                Vy = tmp(3)
                T = tmp(4)
            End If

            _Sv = 0.0#
            _Sl = 0.0#
            Dim mmg, mml As Double

            mmg = PP.AUX_MMM(Vy)
            mml = PP.AUX_MMM(Vx)

            If Settings.EnableParallelProcessing Then

                Dim t1 = Task.Factory.StartNew(Sub() If V > 0 Then _Sv = PP.DW_CalcEntropy(Vy, T, P, State.Vapor))

                Dim t2 = Task.Factory.StartNew(Sub() If L > 0 Then _Sl = PP.DW_CalcEntropy(Vx, T, P, State.Liquid))

                Task.WaitAll(t1, t2)

            Else

                IObj?.SetCurrent()
                If V > 0 Then _Sv = PP.DW_CalcEntropy(Vy, T, P, State.Vapor)
                IObj?.SetCurrent()
                If L > 0 Then _Sl = PP.DW_CalcEntropy(Vx, T, P, State.Liquid)

            End If

            Dim serr As Double = Sf - (mmg * V / (mmg * V + mml * L)) * _Sv - (mml * L / (mmg * V + mml * L)) * _Sl
            OBJ_FUNC_PS_FLASH = {serr, T, V, L, Vy, Vx}

            IObj?.Paragraphs.Add(String.Format("Specified Entropy: {0} kJ/[kg.K]", Sf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", serr))

            IObj?.Close()

            WriteDebugInfo("PS Flash [NL]: Current T = " & T & ", Current S Error = " & serr)

            If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

        End Function

        Function Herror(ByVal type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object
            Return OBJ_FUNC_PH_FLASH(type, X, P, Vz, PP)
        End Function

        Function Serror(ByVal type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object
            Return OBJ_FUNC_PS_FLASH(type, X, P, Vz, PP)
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property
    End Class

End Namespace
