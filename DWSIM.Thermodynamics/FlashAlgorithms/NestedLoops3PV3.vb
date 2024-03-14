'    DWSIM Three-Phase Nested Loops Flash Algorithms
'    Copyright 2014 Daniel Wagner O. de Medeiros
'    Copyright 2015, 2021 Gregor Reichert
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
Imports DotNumerics.Optimization
Imports DWSIM.SharedClasses

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

        Private _nl As New NestedLoops

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

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n)

            Dim result As Object = Nothing

            If prevres IsNot Nothing AndAlso prevres.L2 = 0.0 Then

                V = prevres.V
                L = prevres.L1
                Vy = prevres.Vy
                Vx = prevres.Vx1

                If L > 0.0 Then

                    Dim lps = GetPhaseSplitEstimates(T, P, L, Vx, PP)

                    L1 = lps(0)
                    Vx1 = lps(1)
                    L2 = lps(2)
                    Vx2 = lps(3)

                    If L2 > 0.0 Then

                        result = Flash_PT_3P(Vz, V, L1, L2, Vy, Vx1, Vx2, P, T, PP)

                    Else

                        result = _nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)

                    End If

                Else

                    result = _nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)

                End If

            Else

                If prevres IsNot Nothing Then

                    'jump to 3pflash

                    result = Flash_PT_3P(Vz, prevres.V, prevres.L1, prevres.L2, prevres.Vy, prevres.Vx1, prevres.Vx2, P, T, PP)

                Else

                    result = _nl.Flash_PT(Vz, P, T, PP, False, Nothing)

                    L = result(0)
                    V = result(1)
                    Vx = result(2)
                    Vy = result(3)

                    If L > 0.0 Then

                        Dim lps = GetPhaseSplitEstimates(T, P, L, Vx, PP)

                        L1 = lps(0)
                        Vx1 = lps(1)
                        L2 = lps(2)
                        Vx2 = lps(3)

                        If L2 > 0.0 Then

                            result = Flash_PT_3P(Vz, V, L1, L2, Vy, Vx1, Vx2, P, T, PP)

                        Else

                            result = _nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)

                        End If

                    Else

                        result = _nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)

                    End If

                End If

            End If

            Return result

        End Function

        Public Function Flash_PT_New(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT", Name & " (PT Flash)", "Pressure-Temperature Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            Dim Pvap As Double
            Dim PV() As Double = PP.RET_VPVAP(T)

            Dim gamma1(), gamma2() As Double
            Dim result As Object = Nothing

            Dim d1, d2, d3, d4, d5, d6, d7, d8 As DateTime
            Dim dT1, dT2, dT3, dT4 As TimeSpan

            n = Vz.Length - 1

            proppack = PP

            '============================================================
            '= estimate liquid compositions assuming liquid phases only =
            '============================================================

            IObj?.Paragraphs.Add(String.Format("<hr><b>1. Run LLE-Flash</b>"))
            d1 = Now
            Dim slle As New SimpleLLE()
            Dim resultL As Object = slle.Flash_PT(Vz, P, T, PP)
            L1 = resultL(0) 'phase fraction liquid/liquid
            L2 = resultL(5)
            Vx1 = resultL(2)
            Vx2 = resultL(6)
            gamma1 = resultL(9)
            gamma2 = resultL(10)
            d2 = Now
            dT1 = d2 - d1

            IObj?.Paragraphs.Add(String.Format("Phase fraction L1: {0}", L1))
            IObj?.Paragraphs.Add(String.Format("Phase fraction L2: {0}", L2))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Molar fractions L1: {0}", Vx1.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Molar fractions L2: {0}", Vx2.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Activity coefficients L1: {0}", gamma1.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Activity coefficients L2: {0}", gamma2.ToMathArrayString))

            '============================================================
            '= calculate total vapor pressure of phase 1                =
            '= In equilibrium the vapor pressure of both phases is      =
            '= identical. Therefore only one phase is sufficient for    =
            '= calculation. If no second liquid phase is existing,      =
            '= phase 1 will still be available.                         =
            '============================================================
            Pvap = Vx1.MultiplyY(gamma1).MultiplyY(PV).SumY
            IObj?.SetCurrent
            IObj?.Paragraphs.Add(String.Format("<hr><b>2. Calculate boiling pressure</b><br><br>Boiling Pressure: {0} Pa", Pvap))

            '============================================================
            '= If we are below boiling pressure then we are done.       =
            '============================================================

            If P > Pvap Then
                'we are below boiling point
                IObj?.SetCurrent
                IObj?.Paragraphs.Add(String.Format("Specified Pressure ({0} Pa) is above Boiling Pressure ({1} Pa).", P, Pvap))
                IObj?.Paragraphs.Add(String.Format("No vapor phase exists. Final solution is found."))
                result = {L1, 0, Vx1, PP.RET_NullVector, T, L2, Vx2, 0, PP.RET_NullVector}
            Else
                'we are abov boiling point
                'first split liquids and vapor
                IObj?.SetCurrent
                IObj?.Paragraphs.Add(String.Format("Specified Pressure {0} Pa is below Boiling Pressure: {1} Pa <br><br>This mixture is boiling! We need to run some kind of VLE falsh calculation.", P, Pvap))


                IObj?.Paragraphs.Add(String.Format("<hr><b>3. Split liquid and vapor phases by VLE PT-flash.</b>"))
                d3 = Now
                result = _nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                L = result(0)
                V = result(1)
                Vx = result(2)
                Vy = result(3)

                d4 = Now
                dT2 = d4 - d3
                IObj?.SetCurrent
                IObj?.Paragraphs.Add("<b>VLE flash result:</b>")
                IObj?.Paragraphs.Add(String.Format("Liquid fraction {0}", L))
                IObj?.Paragraphs.Add(String.Format("Vapor fraction {0}", V))
                IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
                IObj?.Paragraphs.Add(String.Format("Liquid composition {0}", Vx.ToMathArrayString))
                IObj?.Paragraphs.Add(String.Format("Vapor composition {0}", Vy.ToMathArrayString))


                'Check number of components and if a liquid phase exists
                'Due to general phase rule, with more than 2 components a 3 phase PT flash may be necessary
                If n > 1 And L > 0 Then
                    IObj?.SetCurrent
                    IObj?.Paragraphs.Add("<hr><b>4. Check Liquid Split</b>")
                    IObj?.Paragraphs.Add("There are more than 2 components in mixture and a liquid phase is existing.<br>
                                          Due to general phase rule this liquid might split up. We need to check that.")

                    'Check possible LLE phase split from remaining liquid
                    d5 = Now

                    Dim lps = GetPhaseSplitEstimates(T, P, L, Vx, PP)
                    L1 = lps(0)
                    Vx1 = lps(1)
                    L2 = lps(2)
                    Vx2 = lps(3)

                    d6 = Now
                    dT3 = d6 - d5

                    'If a second liquid phase is predicted, run a 3 phase PT flash
                    If L2 > 0 Then
                        IObj?.SetCurrent
                        IObj?.Paragraphs.Add("There are two liquid phases predicted. We will need a 3-Phase-Flash!")
                        IObj?.Paragraphs.Add("<hr><b>5. Run 3-Phase-PT-Flash</b>")
                        d7 = Now
                        result = Flash_PT_3P(Vz, V, L1, L2, Vy, Vx1, Vx2, P, T, PP)
                        d8 = Now
                        dT4 = d8 - d7
                    Else
                        IObj?.SetCurrent
                        IObj?.Paragraphs.Add("<b>Result:>/b> Only a single liquid phase was predicted. Previous VLE-Flash result can be taken as final result.")
                    End If

                End If

            End If
            L1 = result(0)
            L2 = result(5)
            V = result(1)
            Vx1 = result(2)
            Vx1 = result(6)
            Vy = result(3)
            IObj?.SetCurrent
            IObj?.Paragraphs.Add(String.Format("<hr><h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid 1 fraction {0}<br>
                                                Liquid 2 fraction {1}<br>
                                                Vapor fraction {2}<br>
                                                Liquid 1 composition {3}<br>
                                                Liquid 2 composition {4}<br>
                                                Vapor composition {5}", L1, L2, V, Vx1.ToMathArrayString, Vx2.ToMathArrayString, Vy.ToMathArrayString))

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

                Dim task1 As Task = TaskHelper.Run(Sub()
                                                       Ki1 = PP.DW_CalcKvalue(Vx1EST, VyEST, T, P)
                                                   End Sub)
                Dim task2 As Task = TaskHelper.Run(Sub()
                                                       Ki2 = PP.DW_CalcKvalue(Vx2EST, VyEST, T, P)
                                                   End Sub)
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

            Dim F1 = 0.0#, F2 = 0.0#
            Dim dF1dL1 = 0.0#, dF1dL2 = 0.0#, dF2dL1 = 0.0#, dF2dL2 = 0.0#
            Dim dL1, dL2 As Double

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
                    Dim task1 As Task = TaskHelper.Run(Sub() CFL1 = proppack.DW_CalcFugCoeff(Vx1, T, P, State.Liquid))
                    Dim task2 As Task = TaskHelper.Run(Sub() CFL2 = proppack.DW_CalcFugCoeff(Vx2, T, P, State.Liquid))
                    Dim task3 As Task = TaskHelper.Run(Sub() CFV = proppack.DW_CalcFugCoeff(Vy, T, P, State.Vapor))
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
                    If Double.IsNaN(CFV(i)) Then CFV(i) = 1.0
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
                    b1(i) = 1 - 1 / Ki1(i)
                    b2(i) = 1 - 1 / Ki2(i)
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

                    i = 0
                    F1 = 0.0
                    F2 = 0.0
                    dF1dL1 = 0.0
                    dF1dL2 = 0.0
                    dF2dL1 = 0.0
                    dF2dL2 = 0.0
                    Do
                        F1 += b1(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        F2 += b2(i) * Vz(i) / (1 - b1(i) * L1 - b2(i) * L2)
                        dF1dL1 += b1(i) * Vz(i) * -b1(i) / Math.Pow(1 - b1(i) * L1 - b2(i) * L2, 2)
                        dF1dL2 += b1(i) * Vz(i) * -b2(i) / Math.Pow(1 - b1(i) * L1 - b2(i) * L2, 2)
                        dF2dL1 += b2(i) * Vz(i) * -b1(i) / Math.Pow(1 - b1(i) * L1 - b2(i) * L2, 2)
                        dF2dL2 += b2(i) * Vz(i) * -b2(i) / Math.Pow(1 - b1(i) * L1 - b2(i) * L2, 2)
                        i = i + 1
                    Loop Until i = n + 1

                    IObj2?.Paragraphs.Add(String.Format("Equilibrium Equation 1 error: {0}", F1))
                    IObj2?.Paragraphs.Add(String.Format("Equilibrium Equation 2 error: {0}", F2))

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

                    Dim df1 = Abs(dL1 / L1)
                    Dim df2 = Abs(dL2 / L2)

                    Dim df = Math.Max(Math.Min(df1, df2), 0.3)

                    If L1 > 0.0 Then L1 += -dL1 * df
                    If L2 > 0.0 Then L2 += -dL2 * df

                    If L1 < 0.0 Then L1 = 0.0
                    If L2 < 0.0 Then L2 = 0.0

                    V = 1 - L1 - L2

                    IObj2?.Paragraphs.Add(String.Format("Updated Estimate for Vapor Phase Molar Fraction (V): {0}", V))
                    IObj2?.Paragraphs.Add(String.Format("Updated Estimate for Liquid Phase 1 Molar Fraction (L1): {0}", L1))
                    IObj2?.Paragraphs.Add(String.Format("Updated Estimate for Liquid Phase 2 Molar Fraction (L2): {0}", L2))

                    If (L1 = 0.0 And L2 > 0.0) Or (L1 > 0.0 And L2 = 0.0) Then

                        'do VLE flash
                        Dim vle = _nl.Flash_PT(Vz, P, T, PP, False, Nothing)
                        L1 = vle(0)
                        V = vle(1)
                        L2 = 0.0
                        Vx1 = vle(2)
                        Vy = vle(3)

                        prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}

                        Return New Object() {L1, V, Vx1, Vy, ecount, L2, Vx2, 0.0#, PP.RET_NullVector, Ki1, vle(9)}

                    ElseIf V <= 0.0# Or Abs(L1) > 1.0# Or Abs(L2) > 1.0# Then

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

            'order liquid phases by gibbs energy

            Dim gl1 = PP.DW_CalcGibbsEnergy(Vx1, T, P, "L")
            Dim gl2 = PP.DW_CalcGibbsEnergy(Vx2, T, P, "L")

            IObj?.Paragraphs.Add("The three-phase algorithm converged in " & ecount & " iterations.")

            IObj?.Paragraphs.Add(String.Format("Converged Value for Vapor Phase Molar Fraction (V): {0}", V))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 1 Molar Fraction (L1): {0}", L1))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 2 Molar Fraction (L2): {0}", L2))

            IObj?.Paragraphs.Add(String.Format("Converged Value for Vapor Phase Molar Composition: {0}", Vy.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 1 Molar Composition: {0}", Vx1.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 2 Molar Composition: {0}", Vx2.ToMathArrayString))

            IObj?.Close()

            If gl1 <= gl2 Then
                prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}
                Return New Object() {L1, V, Vx1, Vy, ecount, L2, Vx2, 0.0#, PP.RET_NullVector, Ki1, Ki2}
            Else
                If L2 < 0.00000001 Then
                    prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}
                    Return New Object() {L1, V, Vx1, Vy, ecount, L2, Vx2, 0.0#, PP.RET_NullVector, Ki1, Ki2}
                Else
                    prevres = New PreviousResults With {.L1 = L2, .L2 = L1, .V = V, .Vy = Vy, .Vx1 = Vx2, .Vx2 = Vx1}
                    Return New Object() {L2, V, Vx2, Vy, ecount, L1, Vx1, 0.0#, PP.RET_NullVector, Ki2, Ki1}
                End If
            End If

        End Function

        Private Function MinimizeGibbs(ByVal dfmin As Double, pp As PropertyPackage, T As Double, P As Double, _L1 As Double, _L2 As Double, dL1 As Double, dl2 As Double, _Vy As Double(), _Vx1 As Double(), _Vx2 As Double()) As Double

            Dim fcv(n), fcl(n), fcl2(n), Gm, Gv, Gl1, Gl2, _V As Double

            Dim brent As New BrentOpt.BrentMinimize

            brent.DefineFuncDelegate(Function(x)
                                         Dim l1x, l2x As Double
                                         If Settings.EnableParallelProcessing Then
                                             Dim task1 As Task = TaskHelper.Run(Sub() fcv = proppack.DW_CalcFugCoeff(_Vy, T, P, State.Vapor))
                                             Dim task2 As Task = TaskHelper.Run(Sub() fcl = proppack.DW_CalcFugCoeff(_Vx1, T, P, State.Liquid))
                                             Dim task3 As Task = TaskHelper.Run(Sub() fcl2 = proppack.DW_CalcFugCoeff(_Vx2, T, P, State.Liquid))
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
                Dim task1 As Task = TaskHelper.Run(Sub() fcv = proppack.DW_CalcFugCoeff(_Vy, T, P, State.Vapor))
                Dim task2 As Task = TaskHelper.Run(Sub() fcl = proppack.DW_CalcFugCoeff(_Vx1, T, P, State.Liquid))
                Dim task3 As Task = TaskHelper.Run(Sub() fcl2 = proppack.DW_CalcFugCoeff(_Vx2, T, P, State.Liquid))
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

            Dim errflag As Boolean = True
            Try
                Dim nl As New NestedLoops
                nl.PTFlashFunction = AddressOf Flash_PT
                nl.DisableParallelCalcs = True
                Return nl.Flash_PH_1(Vz, P, H, Tref, PP, False, Nothing)
                errflag = False
            Catch ex As Exception
            End Try
            If errflag Then
                Return Flash_PH_1(Vz, P, H, Tref, PP, ReuseKI, PrevKi)
            End If

        End Function


        Public Function Flash_PH_1(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            prevres = Nothing

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", Name & " (PH Flash)", "Pressure-Enthalpy Flash Algorithm Routine")

            IObj?.Paragraphs.Add("The PH Flash uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall enthalpy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim d1, d2 As Date, dt As TimeSpan
            Dim n, ecount As Integer
            Dim Type As String

            d1 = Date.Now

            n = Vz.Length - 1

            proppack = PP
            Hf = H
            Pf = P

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n)
            Dim Tb, Td, Hb, Hd As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax, epsilon As Double
            Dim ErrRes As Object

            Tmax = 10000.0#
            Tmin = 20.0#

            Dim fx0, fx1, fx2, dfdx, X0, X1, X2, dx As Double

            T = Tref
            If Tref = 0 Then T = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} kJ/kg", H))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", T))


            '==================================================================================
            '= First check check operating range                                              =
            '= In three phase range, temperature will be constant!                            =
            '= With H in range of partial evaporation               -> iterate vapor fraction =
            '= With H either below boiling point or above dew point -> iterate temperature    =
            '==================================================================================

            If Settings.EnableParallelProcessing Then

                Dim task1 = TaskHelper.Run(Sub()
                                               Dim ErrRes1 = Herror("PV", 0, P, Vz) 'boiling point
                                               Hb = ErrRes1(0)
                                               Tb = ErrRes1(1)
                                           End Sub,
                                                      Settings.TaskCancellationTokenSource.Token)
                Dim task2 = TaskHelper.Run(Sub()
                                               Dim ErrRes2 = Herror("PV", 1, P, Vz) 'dew point
                                               Hd = ErrRes2(0)
                                               Td = ErrRes2(1)
                                           End Sub,
                                                  Settings.TaskCancellationTokenSource.Token)
                Task.WaitAll(task1, task2)

            Else
                IObj?.SetCurrent()
                ErrRes = Herror("PV", 0, P, Vz) 'boiling point
                Hb = ErrRes(0)
                Tb = ErrRes(1)
                IObj?.SetCurrent()
                ErrRes = Herror("PV", 1, P, Vz) 'dew point
                Hd = ErrRes(0)
                Td = ErrRes(1)
            End If

            IObj?.Paragraphs.Add(String.Format("Calculated Bubble Temperature: {0} K", Tb))
            IObj?.Paragraphs.Add(String.Format("Calculated Dew Temperature: {0} K", Td))
            IObj?.Paragraphs.Add(String.Format("Bubble Point Enthalpy Error (Spec - Calculated): {0}", Hb))
            IObj?.Paragraphs.Add(String.Format("Dew Point Enthalpy Error (Spec - Calculated): {0}", Hd))

            If Hb > 0 And Hd < 0 Then
                IObj?.Paragraphs.Add(String.Format("Specified enthalpy between Bubble Point and Dew Point. Calculation requires iteration of vapor fraction."))
                Type = "PV"
                X0 = Hb / (Hb - Hd)
                epsilon = 0.001 'vapor fraction step
            Else
                IObj?.Paragraphs.Add(String.Format("Specified enthalpy between Bubble Point and Dew Point. Calculation requires iteration of vapor fraction."))
                Type = "PT"
                X0 = Tref
                If Hb < 0 Then Tmax = Tb 'Limit temperature below boiling point
                If Hd > 0 Then Tmin = Td 'Limit temperature above dew point
                If X0 < Tmin Then X0 = Tmin
                If X0 > Tmax Then X0 = Tmax
                epsilon = 0.01 'temperature step
            End If

            '==================================================================================
            '= Search enthalpy in operating range by iterating on either T or V               =
            '==================================================================================

            ecount = 0
            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PH", "PH Flash Newton Iteration", "Pressure-Enthalpy Flash Algorithm Convergence Iteration Step")
                IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}.", ecount))

                IObj2?.SetCurrent()

                prevres = Nothing
                ErrRes = Herror(Type, X0, P, Vz)
                fx0 = ErrRes(0)
                T = ErrRes(1)
                V = ErrRes(2)
                L1 = ErrRes(3)
                L2 = ErrRes(6)
                Vy = ErrRes(4)
                Vx1 = ErrRes(5)
                Vx2 = ErrRes(7)

                IObj2?.SetCurrent()

                'if spec is reached -> exit loop
                If Abs(fx0) < tolEXT Then Exit Do

                'Limit X1 to valid range
                X1 = X0 - epsilon
                X2 = X0 + epsilon
                If Type = "PT" Then
                    If (X2 > Tmax) Then
                        X1 = X0 - 2 * epsilon
                        X2 = X0
                    ElseIf (X2 < Tmin) Then
                        X1 = X0
                        X2 = X0 + 2 * epsilon
                    End If
                Else
                    If (X2 > 1) Then
                        X1 = X0 - 2 * epsilon
                        X2 = X0
                    ElseIf (X2 < 0) Then
                        X1 = X0
                        X2 = X0 + 2 * epsilon
                    End If
                End If

                prevres = Nothing
                ErrRes = Herror(Type, X1, P, Vz)
                fx1 = ErrRes(0)
                ErrRes = Herror(Type, X2, P, Vz)
                fx2 = ErrRes(0)

                IObj2?.Paragraphs.Add(String.Format("Current Enthalpy error: {0}", fx0))

                dfdx = (fx2 - fx1) / (2 * epsilon)
                dx = fx0 / dfdx
                X0 -= dx

                'Limit X to valid range
                If Type = "PT" Then
                    If X0 < Tmin Then X0 = Tmin
                    If X0 > Tmax Then X0 = Tmax
                Else
                    If X0 < 0 Then X0 = 0
                    If X0 > 1 Then X0 = 1
                End If

                If Type = "PT" Then
                    IObj2?.Paragraphs.Add(String.Format("Updated temperature estimate: {0} K", X0))
                Else
                    IObj2?.Paragraphs.Add(String.Format("Updated vapor fraction estimate: {0}", X0))
                End If

                ecount += 1

                IObj2?.Close()

            Loop Until ecount > maxitEXT Or Double.IsNaN(X0)

            IObj?.Paragraphs.Add(String.Format("The PH Flash algorithm converged in {0} iterations. Final Temperature value: {1} K  Final Vapor Fraction {2}", ecount, T, V))

            If ecount > maxitEXT Then
                Dim ex As New Exception("PH Flash [NL]: Invalid result: Temperature did not converge." & String.Format(" (T = {0} K, P = {1} Pa, MoleFracs = {2})", T.ToString("N2"), P.ToString("N2"), Vz.ToArrayString()))
                ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                Throw ex
            End If

            Ki = Vy.DivideY(Vx1)

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

            IObj?.Paragraphs.Add("The PS Flash uses two nested loops (hence the name) to calculate temperature and phase distribution. 
                                    The external one converges the temperature, while the internal one finds the phase distribution for the current temperature estimate in the external loop.
                                    The algorithm converges when the calculated overall enthalpy for the tentative phase distribution and temperature matches the specified one.")

            IObj?.SetCurrent()

            Dim d1, d2 As Date, dt As TimeSpan
            Dim n, ecount As Integer
            Dim Type As String

            d1 = Date.Now

            n = Vz.Length - 1

            proppack = PP

            ReDim Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n)
            Dim Tb, Td, Sb, Sd As Double

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tmin, Tmax, epsilon As Double
            Dim ErrRes As Object

            Tmax = 10000.0#
            Tmin = 20.0#

            Dim fx0, fx1, dfdx, X0, X1, dx As Double

            T = Tref
            If Tref = 0 Then T = 298.15

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Entropy: {0} kJ/kg", S))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", T))


            '==================================================================================
            '= First check check operating range                                              =
            '= In three phase range, temperature will be constant!                            =
            '= With S in range of partial evaporation               -> iterate vapor fraction =
            '= With S either below boiling point or above dew point -> iterate temperature    =
            '==================================================================================

            If Settings.EnableParallelProcessing Then

                Dim task1 = TaskHelper.Run(Sub()
                                               Dim ErrRes1 = Serror("PV", 0, S, P, T, Vz) 'boiling point
                                               Sb = ErrRes1(0)
                                               Tb = ErrRes1(1)
                                           End Sub, Settings.TaskCancellationTokenSource.Token)
                Dim task2 = TaskHelper.Run(Sub()
                                               Dim ErrRes2 = Serror("PV", 1, S, P, T, Vz) 'dew point
                                               Sd = ErrRes2(0)
                                               Td = ErrRes2(1)
                                           End Sub, Settings.TaskCancellationTokenSource.Token)
                Task.WaitAll(task1, task2)

            Else
                IObj?.SetCurrent()
                ErrRes = Serror("PV", 0, S, P, T, Vz) 'boiling point
                Sb = ErrRes(0)
                Tb = ErrRes(1)
                IObj?.SetCurrent()
                ErrRes = Serror("PV", 1, S, P, T, Vz) 'dew point
                Sd = ErrRes(0)
                Td = ErrRes(1)
            End If

            IObj?.Paragraphs.Add(String.Format("Calculated Bubble Temperature: {0} K", Tb))
            IObj?.Paragraphs.Add(String.Format("Calculated Dew Temperature: {0} K", Td))
            IObj?.Paragraphs.Add(String.Format("Bubble Point Entropy Error (Spec - Calculated): {0}", Sb))
            IObj?.Paragraphs.Add(String.Format("Dew Point Entropy Error (Spec - Calculated): {0}", Sd))

            If Sb > 0 And Sd < 0 Then
                IObj?.Paragraphs.Add(String.Format("Specified entropy between Bubble Point and Dew Point. Calculation requires iteration of vapor fraction."))
                Type = "PV"
                X0 = Sb / (Sb - Sd)
                epsilon = 0.01 'vapor fraction step
            Else
                IObj?.Paragraphs.Add(String.Format("Specified entropy below Bubble Point or above Dew Point. Calculation requires iteration of temperature."))
                Type = "PT"
                X0 = Tref
                If Sb < 0 Then Tmax = Tb 'Limit temperature below boiling point
                If Sd > 0 Then Tmin = Td 'Limit temperature above dew point
                If X0 < Tmin Then X0 = Tmin
                If X0 > Tmax Then X0 = Tmax
                epsilon = 0.1 'temperature step
            End If

            '==================================================================================
            '= Search entropy in operating range by iterating on either T or V                =
            '==================================================================================

            ecount = 0
            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PS", "PS Flash Newton Iteration", "Pressure-Entropy Flash Algorithm Convergence Iteration Step")
                IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}.", ecount))

                IObj2?.SetCurrent()

                prevres = Nothing
                ErrRes = Serror(Type, X0, S, P, T, Vz)
                fx0 = ErrRes(0)
                T = ErrRes(1)
                V = ErrRes(2)
                L1 = ErrRes(3)
                L2 = ErrRes(6)
                Vy = ErrRes(4)
                Vx1 = ErrRes(5)
                Vx2 = ErrRes(7)

                IObj2?.SetCurrent()

                'if spec is reached -> exit loop
                If Abs(fx0) < tolEXT Then Exit Do

                'Limit X1 to valid range
                X1 = X0 + epsilon
                If Type = "PT" Then
                    If (X1 > Tmax) Or (X1 < Tmin) Then
                        epsilon = -epsilon
                        X1 = X0 + epsilon
                    End If
                Else
                    If (X1 > 1) Or (X1 < 0) Then
                        epsilon = -epsilon
                        X1 = X0 + epsilon
                    End If
                End If

                prevres = Nothing
                ErrRes = Serror(Type, X1, S, P, T, Vz)
                fx1 = ErrRes(0)

                IObj2?.Paragraphs.Add(String.Format("Current Entropy error: {0}", fx0))


                dfdx = (fx1 - fx0) / epsilon
                dx = fx0 / dfdx
                X0 -= dx

                'Limit X to valid range
                If Type = "PT" Then
                    If X0 < Tmin Then X0 = Tmin
                    If X0 > Tmax Then X0 = Tmax
                Else
                    If X0 < 0 Then X0 = 0
                    If X0 > 1 Then X0 = 1
                End If

                If Type = "PT" Then
                    IObj2?.Paragraphs.Add(String.Format("Updated temperature estimate: {0} K", X0))
                Else
                    IObj2?.Paragraphs.Add(String.Format("Updated vapor fraction estimate: {0}", X0))
                End If

                ecount += 1

                IObj2?.Close()

            Loop Until ecount > maxitEXT Or Double.IsNaN(X0)

            IObj?.Paragraphs.Add(String.Format("The PS Flash algorithm converged in {0} iterations. Final Temperature value: {1} K. Final vapor fraction: {2}", ecount, T, V))

            Ki = Vy.DivideY(Vx1)

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [NL3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Function OBJ_FUNC_PH_FLASH(Type As String, X As Double, P As Double, Vz() As Double) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", "PH Flash Objective Function (Error)", "Pressure-Enthalpy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified enthalpies.")

            IObj?.SetCurrent()

            Dim L1, L2, V, Vx1(), Vx2(), Vy() As Double

            Dim tmp As Object

            If Type = "PT" Then
                tmp = Me.Flash_PT(Vz, P, X, proppack)
                L2 = tmp(5)
                Vx2 = tmp(6)
                T = X
            Else
                tmp = Me.Flash_PV(Vz, P, X, 0.0, proppack)
                L2 = tmp(7)
                Vx2 = tmp(8)
                T = tmp(4)
            End If

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)

            Dim _Hv, _Hl1, _Hl2 As Double

            _Hv = 0.0#
            _Hl1 = 0.0#
            _Hl2 = 0.0#

            If V > 0 Then _Hv = proppack.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L1 > 0 Then _Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, P, State.Liquid)
            If L2 > 0 Then _Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)

            Dim mmg, mml, mml2 As Double
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx1)
            mml2 = proppack.AUX_MMM(Vx2)

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2)) * _Hv - (mml * L1 / (mmg * V + mml * L1 + mml2 * L2)) * _Hl1 - (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2)) * _Hl2

            OBJ_FUNC_PH_FLASH = {herr, T, V, L1, Vy, Vx1, L2, Vx2}


            IObj?.Paragraphs.Add(String.Format("Specified Enthalpy: {0} kJ/kg", Hf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/kg", herr))

            IObj?.Close()
            If Type = "PT" Then
                WriteDebugInfo("PH Flash [NL3P]: Current T = " & X & ", Current H Error = " & herr)
            Else
                WriteDebugInfo("PH Flash [NL3P]: Current V = " & X & ", Current H Error = " & herr)
            End If


        End Function

        Function OBJ_FUNC_PS_FLASH(Type As String, X As Double, S As Double, P As Double, T As Double, Vz As Object) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", "PS Flash Objective Function (Error)", "Pressure-Entropy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified entropies.")

            IObj?.SetCurrent()

            Dim L1, L2, V, Vx1(), Vx2(), Vy() As Double

            Dim tmp As Object

            If Type = "PT" Then
                tmp = Me.Flash_PT(Vz, P, X, proppack)
                L2 = tmp(5)
                Vx2 = tmp(6)
                T = X
            Else
                tmp = Me.Flash_PV(Vz, P, X, T, proppack)
                L2 = tmp(7)
                Vx2 = tmp(8)
                T = tmp(4)
            End If

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)

            Dim _Sv, _Sl1, _Sl2 As Double

            _Sv = 0.0#
            _Sl1 = 0.0#
            _Sl2 = 0.0#

            If V > 0 Then _Sv = proppack.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L1 > 0 Then _Sl1 = proppack.DW_CalcEntropy(Vx1, T, P, State.Liquid)
            If L2 > 0 Then _Sl2 = proppack.DW_CalcEntropy(Vx2, T, P, State.Liquid)

            Dim mmg, mml, mml2 As Double
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx1)
            mml2 = proppack.AUX_MMM(Vx2)

            Dim serr As Double = S - (mmg * V / (mmg * V + mml * L1 + mml2 * L2)) * _Sv - (mml * L1 / (mmg * V + mml * L1 + mml2 * L2)) * _Sl1 - (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2)) * _Sl2
            OBJ_FUNC_PS_FLASH = {serr, T, V, L1, Vy, Vx1, L2, Vx2}

            IObj?.Paragraphs.Add(String.Format("Specified Entropy: {0} kJ/[kg.K]", S))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", serr))

            IObj?.Close()


            If Type = "PT" Then
                WriteDebugInfo("PS Flash [NL-3PV3]: Current T = " & X & ", Current S Error = " & serr)
            Else
                WriteDebugInfo("PS Flash [NL3P]: Current V = " & X & ", Current S Error = " & serr)
            End If

        End Function

        Function Herror(Type As String, X As Double, P As Double, Vz() As Double) As Object
            Return OBJ_FUNC_PH_FLASH(Type, X, P, Vz)
        End Function

        Function Serror(Type As String, X As Double, S As Double, P As Double, T As Double, Vz() As Double) As Object
            Return OBJ_FUNC_PS_FLASH(Type, X, S, P, T, Vz)
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

            n = Vz.Length - 1

            d1 = Date.Now

            Dim _nl As New NestedLoops

            Dim result As Object = _nl.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)

            P = result(4)

            If result(0) > 0 Then

                IObj?.SetCurrent
                Dim lps As Object = GetPhaseSplitEstimates(T, P, result(0), result(2), PP)

                If lps(2) > 0 Then

                    If Not prevres Is Nothing Then

                        result = Flash_PV_3P(Vz, prevres.V, prevres.L1, prevres.L2, prevres.Vy, prevres.Vx1, prevres.Vx2, P, V, T, PP)

                    Else

                        L1 = lps(0)
                        L2 = lps(2)
                        Vx1 = lps(1)
                        Vx2 = lps(3)
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

            n = Vz.Length - 1

            d1 = Date.Now

            Dim _nl As New NestedLoops

            IObj?.SetCurrent
            Dim result As Object = _nl.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)

            If V < 1.0 Then

                T = result(4)

                IObj?.SetCurrent
                Dim lps As Object = GetPhaseSplitEstimates(T, P, result(0), result(2), PP)

                If lps(2) / (lps(0) + lps(2)) > 0.00001 Then

                    If Not prevres Is Nothing Then

                        result = Flash_PV_3P(Vz, prevres.V, prevres.L1, prevres.L2, prevres.Vy, prevres.Vx1, prevres.Vx2, P, V, T, PP)

                    Else

                        L1 = lps(0) / (lps(0) + lps(2))
                        L2 = lps(2) / (lps(0) + lps(2))
                        Vx1 = lps(1)
                        Vx2 = lps(3)
                        IObj?.SetCurrent
                        result = Flash_PV_3P(Vz, V, L1 * (1 - V), L2 * (1 - V), result(3), Vx1, Vx2, P, V, T, PP)

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

            VL = VL.NormalizeY()

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
                Dim Pn, P1, P2, lnP, lnP1, lnP2, T1, T2, dTP As Double

                lnP = Log(P)
                cnt = 0
                For i = 0 To n
                    P1 = P1 + Vx1(i) * gamma1(i) * PP.AUX_PVAPi(i, T)
                Next
                lnP1 = Log(P1)
                Dim icount As Integer = 0
                Do
                    cnt += 1
                    T1 = T
                    T2 = T1 + 1
                    P2 = 0
                    For i = 0 To n
                        P2 = P2 + Vx1(i) * gamma1(i) * PP.AUX_PVAPi(i, T2)
                    Next
                    lnP2 = Log(P2)
                    dTP = (T2 - T1) * (lnP - lnP1) / (lnP2 - lnP1)

                    'limit temperature change to avoid problems near aceotropic point
                    If Abs(dTP) > 40 Then dTP = 40 * Sign(dTP)
                    T = T1 + dTP
                    If T < 10 Then T = 10
                    If T > 700 Then T = 700

                    Pn = 0
                    For i = 0 To n
                        Pn = Pn + Vx1(i) * gamma1(i) * PP.AUX_PVAPi(i, T)
                    Next
                    lnP1 = Log(Pn)
                    F = P - Pn

                    icount += 1

                    If icount > maxit_i Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))

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

            'order liquid phases by gibbs energy

            Dim gl1 = PP.DW_CalcGibbsEnergy(Vx1, T, P, "L")
            Dim gl2 = PP.DW_CalcGibbsEnergy(Vx2, T, P, "L")

            If gl1 < gl2 Then
                prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}
                Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}
            Else
                prevres = New PreviousResults With {.L1 = L2, .L2 = L1, .V = V, .Vy = Vy, .Vx1 = Vx2, .Vx2 = Vx1}
                Return New Object() {L2, V, Vx2, Vy, T, ecount, Ki1, L1, Vx1, 0.0#, PP.RET_NullVector}
            End If

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

            'order liquid phases by gibbs energy

            Dim gl1 = PP.DW_CalcGibbsEnergy(Vx1, T, P, "L")
            Dim gl2 = PP.DW_CalcGibbsEnergy(Vx2, T, P, "L")

            If gl1 < gl2 Then
                prevres = New PreviousResults With {.L1 = L1, .L2 = L2, .V = V, .Vy = Vy, .Vx1 = Vx1, .Vx2 = Vx2}
                Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}
            Else
                prevres = New PreviousResults With {.L1 = L2, .L2 = L1, .V = V, .Vy = Vy, .Vx1 = Vx2, .Vx2 = Vx1}
                Return New Object() {L2, V, Vx2, Vy, T, ecount, Ki1, L1, Vx1, 0.0#, PP.RET_NullVector}
            End If

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property
    End Class

End Namespace
