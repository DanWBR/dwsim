'    Multiphase Gibbs Minimization Flash Algorithms
'    Copyright 2012-2020 Daniel Wagner O. de Medeiros
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

Imports Cureos.Numerics
Imports DotNumerics.Optimization
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class GibbsMinimizationMulti

        Inherits FlashAlgorithm

        Private _IObj As InspectorItem

        Private _nl As New NestedLoops

        Private _lastsolution As Object

        Public L1sat As Double = 0.0#
        Dim n, ecount As Integer
        Dim etol As Double = 0.01
        Dim itol As Double = 0.01
        Dim maxit_i As Integer = 1000
        Dim maxit_e As Integer = 1000
        Dim Vn(n) As String
        Dim Vx1(n), Vx2(n), Vy(n), Vv(n), Vl(n), Vs(n), fi(n), Vp(n), Ki(n) As Double
        Dim Vy_ant(n), Ki_ant(n) As Double
        Dim F, L, L1, L2, V, Sx, Tf, Pf, Hf, Sf As Double
        Dim DHv, DHl, DHl1, DHl2, DHs, Hv0, Hvid, Hlid1, Hlid2, Hsid, Hm, Hv, Hl1, Hl2, Hs As Double
        Dim DSv, DSl, DSl1, DSl2, DSs, Sv0, Svid, Slid1, Slid2, Ssid, Sm, Sv, Sl1, Sl2, Ss As Double
        Dim DGv, DGl, DGl1, DGl2, DGs, Gv0, Gvid, Glid1, Glid2, Gsid, Gm, Gv, Gl1, Gl2, Gs As Double
        Dim MMv, MMl1, MMl2, MMs As Double
        Dim Pb, Pd, Pmin, Pmax, Px, soma_x1, soma_x2, soma_y, soma_x, soma_s, Tmin, Tmax As Double
        Dim proppack As PropertyPackages.PropertyPackage
        Dim objval, objval0 As Double

        Dim Vx1_ant(n), Vx2_ant(n), Vs_ant(n), Ki2(n), Ki2_ant(n) As Double
        Dim Vant, T, Tant, P As Double
        Dim Ki1(n) As Double

        Dim prevsol As Double()

        Dim UsePreviousSolution As Boolean = False

        Sub New()
            MyBase.New()
            Order = 5
        End Sub

        Public Property Solver As OptimizationMethod = OptimizationMethod.IPOPT

        Public Enum ObjFuncType As Integer
            MinGibbs = 0
            BubblePointT = 1
            BubblePointP = 2
            DewPointT = 3
            DewPointP = 4
        End Enum

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Gibbs_Minimization_Multiphase
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Encontra as fases em equilíbrio e suas composições através da minimização da Energia Livre de Gibbs."
                Else
                    Return "Finds the equilibrium phases and their compositions through a Free Gibbs energy minimization procedure."
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Gibbs Minimization (Multiphase)"
            End Get
        End Property

        Public Function CheckSolution() As Boolean

            If V < 0.0# Then Return False
            If L < 0.0# Then Return False
            If L1 < 0.0# Then Return False
            If L2 < 0.0# Then Return False

            For i As Integer = 0 To n
                If (fi(i) * F - Vy(i) * V - Vx1(i) * L1 - Vx2(i) * L2 - Vs(i) * Sx) / F > 0.01 Then Return False
            Next

            Return True

        End Function

        Public Overrides Function Flash_PT(ByVal Vz() As Double, ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            _nl.FlashSettings = FlashSettings

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT", Name & " (PT Flash)", "Pressure-Temperature Flash Algorithm Routine", True)

            _IObj = IObj

            IObj?.Paragraphs.Add("In terms of component fugacities the objective function for Gibbs energy minimization can be formulated")

            IObj?.Paragraphs.Add("<m>Q = \sum\limits_{k=1}^{F}{\sum\limits_{i=1}^{C}{n_{ik}\ln f_{ik} }}</m>")

            IObj?.Paragraphs.Add("A straightforward approach is to use the molar amounts in phases 1 to F — 1 as the independent variables, eliminating the nt•F by means of the overall material balance, i.e.,")

            IObj?.Paragraphs.Add("<m>n_{iF}=z_i-\sum\limits_{k=1}^{F-1}{n_{ik}} </m>")

            IObj?.Paragraphs.Add("The gradient and the Hessian are then given by")

            IObj?.Paragraphs.Add("<m>\frac{\partial Q}{\partial n_{ij}}=\ln f_{ij}-\ln f_{iF} </m>")

            IObj?.Paragraphs.Add("and")

            IObj?.Paragraphs.Add("<m>\frac{\partial^2Q}{\partial n_{ij}\partial n_{lm}}=\frac{\partial \ln f_{ij}}{\partial n_{ij}}\delta_jm + \frac{\partial \ln f_{iF}}{\partial n_{lF}}</m>")

            IObj?.Paragraphs.Add("This choice of independent variables may, however, create problems when specific components in phase F are present in very small amounts since the calculation of the <mi>n_{jF}</mi> will be sensitive to roundoff errors. In addition, the ideal solution contribution to the Hessian matrix introduces large off-diagonal elements (<mi>1/n_{iF}</mi>) making the set of linear equations, that must be solved for the correction vector, ill-conditioned. For this reason, Michelsen suggested to use individual 'dependent' variables for each component, selecting")

            IObj?.Paragraphs.Add("<m>n_{im} = z_i - \sum\limits_{k\neq M}^{F}{n_{ik}}</m>")

            IObj?.Paragraphs.Add("where M specifies the phase in which component i is present in the largest amount. Michelson also chose to use scaled independent variables, given by")

            IObj?.Paragraphs.Add("<m>\theta _{ij} = \frac{n_{ij}}{z_i}</m>")

            IObj?.Paragraphs.Add("and suggested to use the Murray factorization of the Hessian to ensure a positive definite approximation to this matrix.")

            IObj?.Paragraphs.Add("The approach of Michelsen appears to work adequately in practice, but it  is by no means guaranteed to be the most efficient approach. An alternative possibility worthwhile considering is the restricted step method, which works very well for the two-phase problem, but no natural diagonal correction term to the Hessian in the cases where modifications are needed seems evident. New and more efficient algorithms may well appear, but drastic gains are not likely to be found.")

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("<h2>Solver</h2>"))

            IObj?.Paragraphs.Add(String.Format("Gibbs Energy will be minimized using the {0} solver.", [Enum].GetName(Solver.GetType, Solver)))

            Me.Solver = [Enum].Parse(Me.Solver.GetType, Me.FlashSettings(FlashSetting.GM_OptimizationMethod))

            If Me.Solver = OptimizationMethod.IPOPT Then Calculator.CheckParallelPInvoke()

            Dim i, j As Integer

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n), Vs(n)

            Dim result As Object = Nothing

            Vn = PP.RET_VNAMES()

            Tf = T
            Pf = P

            fi = Vz.Clone

            Dim Gz0, Gz, Gzv, Gzl, Gzs, fczv(n), fczl(n), fczs(n) As Double

            fczv = PP.DW_CalcFugCoeff(Vz, T, P, State.Vapor)
            fczl = PP.DW_CalcFugCoeff(Vz, T, P, State.Liquid)
            fczs = PP.DW_CalcSolidFugCoeff(T, P)

            Gzv = Vz.MultiplyY(fczv.MultiplyY(Vz).LogY).SumY
            Gzl = Vz.MultiplyY(fczl.MultiplyY(Vz).LogY).SumY
            Gzs = Vz.MultiplyY(fczs.MultiplyY(Vz).LogY).SumY

            Gz0 = {Gzv, Gzl, Gzs}.Min * 1000

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

            If T > MathEx.Common.Max(proppack.RET_VTC, Vz) Then
                'all vapor
                Vy = Vz
                V = 1
                L = 0
                result = New Object() {L, V, Vx1, Vy, ecount, 0.0#, PP.RET_NullVector, Sx, Vs}
                GoTo out
            End If

            If T <= proppack.RET_VTF.MultiplyY(Vz).MinY() Then
                'all solid
                Vs = Vz
                V = 0
                L = 0
                Sx = 1.0
                result = New Object() {L, V, Vx1, Vy, ecount, 0.0#, PP.RET_NullVector, Sx, Vs}
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
                    Vx1 = Vz
                    result = New Object() {L, V, Vx1, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
                    GoTo out
                Else
                    L = 0
                    V = 1
                    Vy = Vz
                    result = New Object() {L, V, Vx1, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
                    GoTo out
                End If
            End If

            Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found

            Dim initval(3 * n + 2) As Double
            Dim lconstr(3 * n + 2) As Double
            Dim uconstr(3 * n + 2) As Double
            Dim finalval(3 * n + 2) As Double
            Dim glow(n), gup(n), g(n) As Double

            IObj?.SetCurrent

            ecount = 0

            objval = 0.0#
            objval0 = 0.0#

            Dim obj As Double
            status = IpoptReturnCode.Invalid_Problem_Definition

            F = 1000.0

            If UsePreviousSolution AndAlso prevsol IsNot Nothing Then

                j = 0
                For i = 0 To n
                    initval(j) = prevsol(j)
                    lconstr(j) = 0.0#
                    uconstr(j) = Vz(i) * F
                    glow(i) = 0.0#
                    gup(i) = F
                    If initval(j) > uconstr(j) Then initval(j) = uconstr(j) * L1 / F
                    j += 1
                Next
                For i = 0 To n
                    initval(j) = prevsol(j)
                    lconstr(j) = 0.0#
                    uconstr(j) = Vz(i) * F
                    If initval(j) > uconstr(j) Then initval(j) = uconstr(j) * L2 / F
                    j += 1
                Next
                For i = 0 To n
                    initval(j) = prevsol(j)
                    lconstr(j) = 0.0#
                    uconstr(j) = Vz(i) * F
                    If initval(j) > uconstr(j) Then initval(j) = uconstr(j) * Sx / F
                    j += 1
                Next

            Else

                Dim rnl = _nl.Flash_PT(Vz, P, T, PP)

                L1 = rnl(0)
                V = rnl(1)
                Vx1 = rnl(2)
                Vy = rnl(3)

                If L1 = 0.0 Then
                    L1 = 0.01
                    V = 0.99
                End If

                L2 = 0.01
                Sx = 0.01

                Dim sum = V + L1 + L2 + Sx

                V = V / sum
                L1 = L1 / sum
                L2 = L2 / sum
                Sx = Sx / sum

                V *= 1000.0
                L1 *= 1000.0
                L2 *= 1000.0
                Sx *= 1000.0

                Dim VTfus As Double() = PP.RET_VTF

                For i = 0 To n
                    If Vz(i) > 0.0 And T < VTfus(i) Then
                        Vs(i) = Vz(i)
                        Vy(i) = 0.0
                        Vx1(i) = 0.0
                        Vx2(i) = 0.0
                    End If
                Next

                Sx = Vs.SumY

                Dim stresult = StabTest2(T, P, Vx1, PP.RET_VTC, PP)

                If stresult.Count > 0 Then

                    Dim validsolutions = stresult.Where(Function(s) s.Max > 0.5).ToList()

                    If validsolutions.Count > 0 Then
                        Vx2 = validsolutions(0)
                    Else
                        Vx2 = stresult(0)
                    End If

                    Dim maxl As Double = MathEx.Common.Max(Vx2)
                    Dim imaxl As Integer = Array.IndexOf(Vx2, maxl)

                    If F * Vz(imaxl) < F * Vx1(imaxl) Then L2 = F * Vz(imaxl) Else L2 = F * Vx1(imaxl)

                    L1 = F - L2 - V - Sx

                    Vx1(imaxl) = 1.0E-20

                    If L1 < 0.0# Then
                        L1 = Abs(L1)
                        L2 = F - L1 - V - Sx
                    End If

                    If L2 < 0.0# Then
                        V += L2
                        L2 = Abs(L2)
                    End If

                End If

                Vy = Vy.NormalizeY()
                Vx1 = Vx1.NormalizeY()
                Vx2 = Vx2.NormalizeY()
                Vs = Vs.NormalizeY()

                j = 0
                For i = 0 To n
                    initval(j) = Vx1(i) * L1
                    lconstr(j) = 0.0#
                    uconstr(j) = Vz(i) * F
                    glow(i) = 0.0#
                    gup(i) = F
                    If initval(j) > uconstr(j) Then initval(j) = uconstr(j) * L1 / F
                    j += 1
                Next
                For i = 0 To n
                    initval(j) = Vx2(i) * L2
                    lconstr(j) = 0.0#
                    uconstr(j) = Vz(i) * F
                    If initval(j) > uconstr(j) Then initval(j) = uconstr(j) * L2 / F
                    j += 1
                Next
                For i = 0 To n
                    initval(j) = Vs(i) * Sx
                    lconstr(j) = 0.0#
                    uconstr(j) = Vz(i) * F
                    If initval(j) > uconstr(j) Then initval(j) = uconstr(j) * Sx / F
                    j += 1
                Next

            End If

            Select Case Me.Solver
                Case OptimizationMethod.Limited_Memory_BGFS
                    Dim variables(3 * n + 2) As OptBoundVariable
                    For i = 0 To 3 * n + 2
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim solver As New L_BFGS_B
                    solver.Tolerance = etol
                    solver.MaxFunEvaluations = maxit_e * 10
                    initval = solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    If solver.FunEvaluations = solver.MaxFunEvaluations Then
                        Throw New Exception("PT Flash: Maximum iterations exceeded.")
                    End If
                    solver = Nothing
                Case OptimizationMethod.Truncated_Newton
                    Dim variables(3 * n + 2) As OptBoundVariable
                    For i = 0 To 3 * n + 2
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim solver As New TruncatedNewton
                    solver.Tolerance = etol
                    solver.MaxFunEvaluations = maxit_e * 10
                    initval = solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    If solver.FunEvaluations = solver.MaxFunEvaluations Then
                        Throw New Exception("PT Flash: Maximum iterations exceeded.")
                    End If
                    solver = Nothing
                Case OptimizationMethod.Simplex
                    Dim variables(3 * n + 2) As OptBoundVariable
                    For i = 0 To 3 * n + 2
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim solver As New Simplex
                    solver.Tolerance = etol
                    solver.MaxFunEvaluations = maxit_e * 10
                    initval = solver.ComputeMin(AddressOf FunctionValue, variables)
                    If solver.FunEvaluations = solver.MaxFunEvaluations Then
                        Throw New Exception("PT Flash: Maximum iterations exceeded.")
                    End If
                    solver = Nothing
                Case Else
                    Using problem As New Ipopt(initval.Length, lconstr, uconstr, n + 1, glow, gup, (n + 1) * 3, 0,
                            AddressOf eval_f, AddressOf eval_g,
                            AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                        problem.AddOption("tol", etol)
                        problem.AddOption("max_iter", maxit_e * 10)
                        problem.AddOption("mu_strategy", "adaptive")
                        'problem.AddOption("mehrotra_algorithm", "yes")
                        problem.AddOption("hessian_approximation", "limited-memory")
                        'problem.SetIntermediateCallback(AddressOf intermediate)
                        'solve the problem 
                        status = problem.SolveProblem(initval, obj, g, Nothing, Nothing, Nothing)
                        Select Case status
                            Case IpoptReturnCode.Diverging_Iterates,
                                  IpoptReturnCode.Error_In_Step_Computation,
                                   IpoptReturnCode.Infeasible_Problem_Detected
                                Throw New Exception("PT Flash: IPOPT failed to converge.")
                            Case IpoptReturnCode.Maximum_Iterations_Exceeded
                                Throw New Exception("PT Flash: Maximum iterations exceeded.")
                        End Select
                    End Using
            End Select

            For i = 0 To initval.Length - 1
                If Double.IsNaN(initval(i)) Then initval(i) = 0.0#
            Next

            Gz = FunctionValue(initval)

            Dim mbr As Double = MassBalanceResidual()

            If Gz > Gz0 Or mbr > 0.01 * n Then
                Throw New Exception("PT Flash: Invalid solution.")
            End If

            prevsol = initval

            If Sx < 0.01 Then Sx = 0.0
            If L2 < 0.01 Then L2 = 0.0

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add("The four-phase algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & F)

            IObj?.Paragraphs.Add(String.Format("Converged Value for Vapor Phase Molar Fraction (V): {0}", V / F))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 1 Molar Fraction (L1): {0}", L1 / F))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 2 Molar Fraction (L2): {0}", L2 / F))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Solid Phase Molar Fraction (S): {0}", Sx / F))

            IObj?.Paragraphs.Add(String.Format("Converged Value for Vapor Phase Molar Composition: {0}", Vy.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 1 Molar Composition: {0}", Vx1.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Liquid Phase 2 Molar Composition: {0}", Vx2.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Converged Value for Solid Phase Molar Composition: {0}", Vs.ToMathArrayString))

            'order liquid phases by density

            Dim dens1, dens2 As Double
            dens1 = PP.AUX_LIQDENS(T, Vx1, P)
            dens2 = PP.AUX_LIQDENS(T, Vx2, P)

            If dens1 <= dens2 Then
                result = New Object() {L1 / F, V / F, Vx1, Vy, ecount, L2 / F, Vx2, Sx / F, Vs}
            Else
                result = New Object() {L2 / F, V / F, Vx2, Vy, ecount, L1 / F, Vx1, Sx / F, Vs}
            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [GM]: Converged in " & ecount & " iterations. Status: " & [Enum].GetName(GetType(IpoptReturnCode), status) & ". Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & result(4) & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

out:        Return result

        End Function

        Public Function MassBalanceResidual() As Double

            Dim n = fi.Length - 1

            Dim mbr As Double = 0

            For i As Integer = 0 To n
                mbr += F * fi(i) - V * Vy(i) - L1 * Vx1(i) - L2 * Vx2(i) - Sx * Vs(i)
            Next

            Return Math.Abs(mbr)

        End Function

        'Function Values

        Public Function FunctionValue(ByVal x() As Double) As Double

            _IObj?.SetCurrent

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "FunctionValue", "Gibbs Minimization Function Calculator", "", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add(String.Format("Input Variables: {0}", x.ToMathArrayString))

            If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim pval As Double = 0.0#
            Dim fcv(n), fcl(n), fcl2(n), fcs(n) As Double

            soma_x1 = 0
            For i = 0 To n
                soma_x1 += x(i)
            Next
            soma_x2 = 0
            For i = 0 To n
                soma_x2 += x(i + n + 1)
            Next
            soma_s = 0
            For i = 0 To n
                soma_s += x(i + 2 * n + 2)
            Next

            Sx = soma_s
            L = soma_x1 + soma_x2
            L1 = soma_x1
            L2 = soma_x2
            V = F - Sx - L1 - L2

            For i = 0 To n
                If L1 <> 0.0# Then Vx1(i) = x(i) / L1 Else Vx1(i) = 0.0#
                If L2 <> 0.0# Then Vx2(i) = x(i + n + 1) / L2 Else Vx2(i) = 0.0#
                If Sx <> 0.0# Then Vs(i) = x(i + 2 * n + 2) / Sx Else Vs(i) = 0.0#
                If V <> 0.0# Then Vy(i) = (fi(i) * F - Vx1(i) * L1 - Vx2(i) * L2 - Sx * Vs(i)) / V Else Vs(i) = 0.0#
                If Vy(i) <= 0.0# Then Vy(i) = 1.0E-20
                If Vx1(i) <= 0.0# Then Vx1(i) = 1.0E-20
                If Vx2(i) <= 0.0# Then Vx2(i) = 1.0E-20
                If Vs(i) <= 0.0# Then Vs(i) = 1.0E-20
            Next

            soma_y = 0
            For i = 0 To n
                soma_y += Vy(i)
            Next
            For i = 0 To n
                If soma_y <> 0.0# Then Vy(i) /= soma_y
            Next

            If Settings.EnableParallelProcessing Then

                Dim task1 As Task = New Task(Sub()
                                                 fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                                             End Sub)
                Dim task4 As Task = New Task(Sub()
                                                 fcs = proppack.DW_CalcSolidFugCoeff(Tf, Pf)
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                task4.Start()
                Task.WaitAll(task1, task2, task3, task4)

            Else

                IObj?.SetCurrent()
                fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                IObj?.SetCurrent()
                fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                IObj?.SetCurrent()
                fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                IObj?.SetCurrent()
                fcs = proppack.DW_CalcSolidFugCoeff(Tf, Pf)

            End If

            Gv = 0
            Gl1 = 0
            Gl2 = 0
            Gs = 0
            For i = 0 To n
                If Vy(i) <> 0 Then Gv += Vy(i) * V * Log(fcv(i) * Vy(i))
                If Vx1(i) <> 0 Then Gl1 += Vx1(i) * L1 * Log(fcl(i) * Vx1(i))
                If Vx2(i) <> 0 Then Gl2 += Vx2(i) * L2 * Log(fcl2(i) * Vx2(i))
                If Vs(i) <> 0 Then Gs += Vs(i) * Sx * Log(fcs(i) * Vs(i))
            Next

            IObj?.Paragraphs.Add(String.Format("Calculated Vapor Phase fugacity coefficients: {0}", fcv.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Calculated Liquid Phase 1 fugacity coefficients: {0}", fcl.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Calculated Liquid Phase 2 fugacity coefficients: {0}", fcl2.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Calculated Solid Phase fugacity coefficients: {0}", fcs.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Calculated Vapor Phase molar fraction: {0}", V))
            IObj?.Paragraphs.Add(String.Format("Calculated Liquid Phase 1 molar fraction: {0}", L1))
            IObj?.Paragraphs.Add(String.Format("Calculated Liquid Phase 2 molar fraction: {0}", L2))
            IObj?.Paragraphs.Add(String.Format("Calculated Solid Phase molar fraction: {0}", Sx))

            IObj?.Paragraphs.Add(String.Format("Calculated Vapor Phase composition: {0}", Vy.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Calculated Liquid Phase 1 composition: {0}", Vx1.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Calculated Liquid Phase 2 composition: {0}", Vx2.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Calculated Solid Phase composition: {0}", Vs.ToMathArrayString))

            Gm = Gv + Gl1 + Gl2 + Gs + pval

            IObj?.Paragraphs.Add(String.Format("Calculated Gibbs Energy Value: {0}", Gm))

            WriteDebugInfo("[GM] V = " & Format(V, "N4") & ", L1 = " & Format(L1, "N4") & ", L2 = " & Format(L2, "N4") & ", S = " & Format(Sx, "N4") & " / GE = " & Format(Gm * 8.314 * Tf / 1000, "N2") & " kJ/kmol")

            ecount += 1

            Return Gm

        End Function

        Public Function FunctionGradient(ByVal x() As Double) As Double()

            Dim g(x.Length - 1) As Double
            Dim fcv(n), fcl(n), fcl2(n), fcs(n) As Double

            soma_x1 = 0
            For i = 0 To n
                soma_x1 += x(i)
            Next
            soma_x2 = 0
            For i = 0 To n
                soma_x2 += x(i + n + 1)
            Next
            soma_s = 0
            For i = 0 To n
                soma_s += x(i + 2 * n + 2)
            Next

            Sx = soma_s
            L = soma_x1 + soma_x2
            L1 = soma_x1
            L2 = soma_x2
            V = F - Sx - L1 - L2

            For i = 0 To n
                If L1 <> 0.0# Then Vx1(i) = x(i) / L1 Else Vx1(i) = 0.0#
                If L2 <> 0.0# Then Vx2(i) = x(i + n + 1) / L2 Else Vx2(i) = 0.0#
                If Sx <> 0.0# Then Vs(i) = x(i + 2 * n + 2) / Sx Else Vs(i) = 0.0#
                If V <> 0.0# Then Vy(i) = (fi(i) * F - Vx1(i) * L1 - Vx2(i) * L2 - Sx * Vs(i)) / V Else Vs(i) = 0.0#
                If Vy(i) <= 0.0# Then Vy(i) = 1.0E-20
                If Vx1(i) <= 0.0# Then Vx1(i) = 1.0E-20
                If Vx2(i) <= 0.0# Then Vx2(i) = 1.0E-20
                If Vs(i) <= 0.0# Then Vs(i) = 1.0E-20
            Next

            soma_y = 0
            For i = 0 To n
                soma_y += Vy(i)
            Next
            For i = 0 To n
                If soma_y <> 0.0# Then Vy(i) /= soma_y
            Next

            If Settings.EnableParallelProcessing Then

                Dim task1 As Task = New Task(Sub()
                                                 fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                                             End Sub)
                Dim task4 As Task = New Task(Sub()
                                                 fcs = proppack.DW_CalcSolidFugCoeff(Tf, Pf)
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                task4.Start()
                Task.WaitAll(task1, task2, task3, task4)

            Else

                fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                fcs = proppack.DW_CalcSolidFugCoeff(Tf, Pf)

            End If

            Dim j As Integer = 0
            For i = 0 To n
                If Vx1(i) <> 0 And Vy(i) <> 0 Then g(j) = Log(fcl(i) * Vx1(i)) - Log(fcv(i) * Vy(i))
                j += 1
            Next
            For i = 0 To n
                If Vx2(i) <> 0 And Vy(i) <> 0 Then g(j) = Log(fcl2(i) * Vx2(i)) - Log(fcv(i) * Vy(i))
                j += 1
            Next
            For i = 0 To n
                If Vs(i) <> 0 And Vy(i) <> 0 Then g(j) = Log(fcs(i) * Vs(i)) - Log(fcv(i) * Vy(i))
                j += 1
            Next

            Return g

        End Function

        'Public Function FunctionGradient(ByVal x() As Double) As Double()

        '    Dim epsilon As Double = 0.001

        '    Dim f1, f2 As Double
        '    Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
        '    Dim j, k As Integer

        '    For j = 0 To x.Length - 1
        '        For k = 0 To x.Length - 1
        '            x1(k) = x(k)
        '            x2(k) = x(k)
        '        Next
        '        If x(j) <> 0.0# Then
        '            x1(j) = x(j) * (1.0# + epsilon)
        '            x2(j) = x(j) * (1.0# - epsilon)
        '        Else
        '            x1(j) = x(j) + epsilon
        '            x2(j) = x(j) - epsilon
        '        End If
        '        f1 = FunctionValue(x1)
        '        f2 = FunctionValue(x2)
        '        g(j) = (f2 - f1) / (x2(j) - x1(j))
        '    Next

        '    Return g

        'End Function

        Private Function FunctionHessian(ByVal x() As Double) As Double()

            Dim epsilon As Double = 0.001

            Dim f2() As Double = Nothing
            Dim f3() As Double = Nothing
            Dim h((x.Length) * (x.Length) - 1), x2(x.Length - 1), x3(x.Length - 1) As Double
            Dim m, k As Integer

            m = 0
            For i = 0 To x.Length - 1
                For j = 0 To x.Length - 1
                    If i <> j Then
                        x2(j) = x(j)
                        x3(j) = x(j)
                    Else
                        x2(j) = x(j) * (1 + epsilon)
                        x3(j) = x(j) * (1 - epsilon)
                    End If
                Next
                If Settings.EnableParallelProcessing Then

                    Dim task1 As Task = New Task(Sub()
                                                     f2 = FunctionGradient(x2)
                                                 End Sub)
                    Dim task2 As Task = New Task(Sub()
                                                     f3 = FunctionGradient(x3)
                                                 End Sub)
                    task1.Start()
                    task2.Start()
                    Task.WaitAll(task1, task2)

                Else
                    f2 = FunctionGradient(x2)
                    f3 = FunctionGradient(x3)
                End If
                For k2 = 0 To x.Length - 1
                    h(m) = (f2(k2) - f3(k)) / (x2(i) - x3(i))
                    If Double.IsNaN(h(m)) Then h(m) = 0.0#
                    m += 1
                Next
            Next

            Return h

        End Function

        'IPOPT

        Public Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean
            Dim fval As Double = FunctionValue(x)
            obj_value = fval
            Return True
        End Function

        Public Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean
            Dim g As Double() = FunctionGradient(x)
            grad_f = g
            Return True
        End Function

        Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
            For i = 0 To m - 1
                g(i) = fi(i) * F - x(i) - x(i + m) - x(i + 2 * m)
            Next
            Return True
        End Function

        Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(),
         ByRef jCol As Integer(), ByRef values As Double()) As Boolean

            Dim k As Integer

            If values Is Nothing Then

                Dim row(nele_jac - 1), col(nele_jac - 1) As Integer

                k = 0
                For i = 0 To m - 1
                    row(i) = i
                    row(i + m) = i
                    col(i) = i
                    col(i + m) = i + m
                Next

                iRow = row
                jCol = col

            Else

                Dim res(nele_jac - 1) As Double

                For i = 0 To nele_jac - 1
                    res(i) = -1
                Next

                values = res

            End If
            Return True
        End Function

        Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(),
         ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean

            If values Is Nothing Then

                Dim row(nele_hess - 1), col(nele_hess - 1) As Integer

                iRow = row
                jCol = col

            Else

                values = FunctionHessian(x)

            End If

            Return True

        End Function

        Public Function intermediate(ByVal alg_mod As IpoptAlgorithmMode, ByVal iter_count As Integer, ByVal obj_value As Double,
                                     ByVal inf_pr As Double, ByVal inf_du As Double, ByVal mu As Double,
                                     ByVal d_norm As Double, ByVal regularization_size As Double, ByVal alpha_du As Double,
                                     ByVal alpha_pr As Double, ByVal ls_trials As Integer) As Boolean
            objval0 = objval
            objval = obj_value
            Return True
        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            UsePreviousSolution = True

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

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vs(n), Vp(n), Ki(n), fi(n)

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
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} kJ/kg", H))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for T: {0} K", T))

            For j = 0 To 4

                cnt = 0
                x1 = Tref

                Do

                    IObj?.SetCurrent()

                    Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PH", "PH Flash Newton Iteration", "Pressure-Enthalpy Flash Algorithm Convergence Iteration Step")

                    IObj2?.Paragraphs.Add(String.Format("This is the Newton convergence loop iteration #{0}. DWSIM will use the current value of T to calculate the phase distribution by calling the Flash_PT routine.", cnt))

                    IObj2?.SetCurrent()
                    fx = Herror(x1, {P, Vz, PP})

                    If Abs(fx) < tolEXT Then Exit Do

                    IObj2?.SetCurrent()
                    fx2 = Herror(x1 + epsilon(j), {P, Vz, PP})

                    IObj2?.Paragraphs.Add(String.Format("Current Enthalpy error: {0}", fx))

                    dfdx = (fx2 - fx) / epsilon(j)
                    dx = fx / dfdx

                    x1 = x1 - dx

                    IObj2?.Paragraphs.Add(String.Format("Updated Temperature estimate: {0} K", x1))

                    IObj2?.Close()

                    cnt += 1

                Loop Until cnt > maxitEXT Or Double.IsNaN(x1)

                IObj?.Paragraphs.Add(String.Format("The PH Flash algorithm converged in {0} iterations. Final Temperature value: {1} K", cnt, x1))

                T = x1

                If Not Double.IsNaN(T) And Not Double.IsInfinity(T) And Not cnt > maxitEXT Then
                    If T > Tmin And T < Tmax Then Exit For
                End If

            Next

            If Double.IsNaN(T) Or cnt > maxitEXT Then

alt:
                Dim bo As New BrentOpt.Brent
                bo.DefineFuncDelegate(AddressOf Herror)
                WriteDebugInfo("PH Flash: Newton's method failed. Starting fallback Brent's method calculation for " & Tmin & " <= T <= " & Tmax)

                T = bo.BrentOpt(Tmin, Tmax, 25, tolEXT, maxitEXT, {P, Vz, PP})

            End If

            If T <= Tmin Or T >= Tmax Then
                Dim ex As New Exception("PH Flash: Invalid result: Temperature did not converge.")
                ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                Throw ex
            End If

            IObj?.SetCurrent()
            'Dim tmp As Object = Flash_PT(Vz, P, T, PP)
            Dim tmp As Object = _lastsolution

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx1(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, Sx, Vs}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            UsePreviousSolution = True

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

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n), Vs(n)

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

                    If Abs(fx) < tolEXT Then Exit Do

                    IObj2?.SetCurrent()
                    fx2 = Serror(x1 + epsilon(j), {P, Vz, PP})

                    IObj2?.Paragraphs.Add(String.Format("Current Entropy error: {0}", fx))

                    dfdx = (fx2 - fx) / epsilon(j)
                    dx = fx / dfdx

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
                WriteDebugInfo("PS Flash: Newton's method failed. Starting fallback Brent's method calculation for " & Tmin & " <= T <= " & Tmax)

                T = bo.BrentOpt(Tmin, Tmax, 25, tolEXT, maxitEXT, {P, Vz, PP})

            End If

            If T <= Tmin Or T >= Tmax Then
                Dim ex As New Exception("PS Flash: Invalid result: Temperature did not converge.")
                ex.Data.Add("DetailedDescription", "The Flash Algorithm was unable to converge to a solution.")
                ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                Throw ex
            End If


            'Dim tmp As Object = Flash_PT(Vz, P, T, PP)
            Dim tmp As Object = _lastsolution

            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            ecount = tmp(4)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx1(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Close()

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, Sx, Vs}

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", "PH Flash Objective Function (Error)", "Pressure-Enthalpy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified enthalpies.")

            IObj?.SetCurrent()

            Dim tmp = Me.Flash_PT(Vz, Pf, T, proppack)

            _lastsolution = tmp

            Dim n = Vz.Length - 1

            Dim L1, L2, V, Sx, Vx1(), Vx2(), Vy(), Vs() As Double

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

            If V > 0 Then _Hv = proppack.DW_CalcEnthalpy(Vy, T, Pf, State.Vapor)
            If L1 > 0 Then _Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, Pf, State.Liquid)
            If L2 > 0 Then _Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, Pf, State.Liquid)
            If Sx > 0 Then _Hs = proppack.DW_CalcSolidEnthalpy(T, Vs, proppack.DW_GetConstantProperties())

            Dim mmg, mml, mml2, mms As Double
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx1)
            mml2 = proppack.AUX_MMM(Vx2)
            mms = proppack.AUX_MMM(Vs)

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hv -
                (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl1 -
                (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl2 -
                (mms * Sx / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hs

            OBJ_FUNC_PH_FLASH = herr

            IObj?.Paragraphs.Add(String.Format("Specified Enthalpy: {0} kJ/kg", Hf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/kg", herr))

            IObj?.Close()

            WriteDebugInfo("PH Flash: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", "PS Flash Objective Function (Error)", "Pressure-Entropy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified entropies.")

            IObj?.SetCurrent()

            Dim tmp = Me.Flash_PT(Vz, Pf, T, proppack)

            _lastsolution = tmp

            Dim n = Vz.Length - 1

            Dim L1, L2, V, Sx, Vx1(), Vx2(), Vy(), Vs() As Double

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
            _Ss = 0.0#

            If V > 0 Then _Sv = proppack.DW_CalcEntropy(Vy, T, Pf, State.Vapor)
            If L1 > 0 Then _Sl1 = proppack.DW_CalcEntropy(Vx1, T, Pf, State.Liquid)
            If L2 > 0 Then _Sl2 = proppack.DW_CalcEntropy(Vx2, T, Pf, State.Liquid)
            If Sx > 0 Then _Ss = proppack.DW_CalcSolidEnthalpy(T, Vs, proppack.DW_GetConstantProperties()) / T

            Dim mmg, mml, mml2, mms As Double
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx1)
            mml2 = proppack.AUX_MMM(Vx2)
            mms = proppack.AUX_MMM(Vs)

            Dim serr As Double = Sf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sv -
                (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl1 -
                (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl2 -
                (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Ss

            OBJ_FUNC_PS_FLASH = serr

            IObj?.Paragraphs.Add(String.Format("Specified Entropy: {0} kJ/[kg.K]", Sf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", serr))

            IObj?.Close()

            WriteDebugInfo("PS Flash: Current T = " & T & ", Current S Error = " & serr)

        End Function

        Function Herror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PH_FLASH(Tt, Sf, Pf, fi)
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, Pf, fi)
        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, Vspec As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            _nl.FlashSettings = Me.FlashSettings
            Return _nl.Flash_PV(Vz, P, Vspec, Tref, PP, ReuseKI, PrevKi)
        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, Vspec As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            _nl.FlashSettings = Me.FlashSettings
            Return _nl.Flash_TV(Vz, T, Vspec, Pref, PP, ReuseKI, PrevKi)
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace
