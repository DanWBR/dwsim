'    Michelsen's Three-Phase Gibbs Minimization w/ Nested Loops Flash Algorithms
'    Copyright 2012-2015 Daniel Wagner O. de Medeiros
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

Imports Cureos.Numerics
Imports DotNumerics.Optimization
Imports System.Threading.Tasks
Imports DotNumerics
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class GibbsMinimization3P

        Inherits FlashAlgorithm

        Private _nl As New DWSIMDefault
        Private _nl3p As New NestedLoops3PV2

        Public ForceTwoPhaseOnly As Boolean = False
        Public L1sat As Double = 0.0#
        Public ThreePhase As Boolean = False
        Dim n, ecount As Integer
        Dim etol As Double = 0.01
        Dim itol As Double = 0.01
        Dim maxit_i As Integer = 1000
        Dim maxit_e As Integer = 1000
        Dim Vn(n) As String
        Dim Vx1(n), Vx2(n), Vy(n), Vv(n), Vl(n), fi(n), Vp(n), Ki(n) As Double
        Dim Vx(n), Vx_ant(n), Vy_ant(n), Ki_ant(n) As Double
        Dim F, L, L1, L2, V, Tf, Pf, Hf, Sf As Double
        Dim DHv, DHl, DHl1, DHl2, Hv0, Hvid, Hlid1, Hlid2, Hm, Hv, Hl1, Hl2 As Double
        Dim DSv, DSl, DSl1, DSl2, Sv0, Svid, Slid1, Slid2, Sm, Sv, Sl1, Sl2 As Double
        Dim DGv, DGl, DGl1, DGl2, Gv0, Gvid, Glid1, Glid2, Gm, Gv, Gl1, Gl2 As Double
        Dim MMv, MMl1, MMl2 As Double
        Dim Pb, Pd, Pmin, Pmax, Px, soma_x1, soma_x2, soma_y, soma_x, Tmin, Tmax As Double
        Dim proppack As PropertyPackages.PropertyPackage
        Dim objval, objval0 As Double

        Dim Vx1_ant(n), Vx2_ant(n), Ki2(n), Ki2_ant(n) As Double
        Dim Vant, T, Tant, P As Double
        Dim Ki1(n) As Double

        Public Property Solver As OptimizationMethod = OptimizationMethod.IPOPT

        Public Enum ObjFuncType As Integer
            MinGibbs = 0
            BubblePointT = 1
            BubblePointP = 2
            DewPointT = 3
            DewPointP = 4
        End Enum

        Dim objfunc As ObjFuncType = ObjFuncType.MinGibbs

        Public Function CheckSolution() As Boolean

            If V < 0.0# Then Return False
            If L < 0.0# Then Return False
            If L1 < 0.0# Then Return False
            If L2 < 0.0# Then Return False

            If ThreePhase Then
                For i As Integer = 0 To n
                    If (fi(i) * F - Vy(i) * V - Vx1(i) * L1 - Vx2(i) * L2) / F > 0.01 Then Return False
                Next
            Else
                For i As Integer = 0 To n
                    If (fi(i) * F - Vy(i) * V - Vx1(i) * L) / F > 0.01 Then Return False
                Next
            End If

            Return True

        End Function

        Private Function GetSolver(solver As OptimizationMethod) As SwarmOps.Optimizer

            Select Case solver
                Case OptimizationMethod.DifferentialEvolution
                    Return New SwarmOps.Optimizers.DE()
                Case OptimizationMethod.GradientDescent
                    Return New SwarmOps.Optimizers.GD()
                Case OptimizationMethod.LocalUnimodalSampling
                    Return New SwarmOps.Optimizers.LUS()
                Case OptimizationMethod.ManyOptimizingLiaisons
                    Return New SwarmOps.Optimizers.MOL()
                Case OptimizationMethod.Mesh
                    Return New SwarmOps.Optimizers.MESH()
                Case OptimizationMethod.ParticleSwarm
                    Return New SwarmOps.Optimizers.PS()
                Case OptimizationMethod.ParticleSwarmOptimization
                    Return New SwarmOps.Optimizers.PSO()
                Case Else
                    Return Nothing
            End Select

        End Function

        Public Overrides Function Flash_PT(ByVal Vz() As Double, ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Calculator.CheckParallelPInvoke()

            Me.Solver = [Enum].Parse(Me.Solver.GetType, Me.FlashSettings(FlashSetting.GM_OptimizationMethod))

            Dim i, j, k As Integer

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance)
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki(n), fi(n)

            Dim result As Object = Nothing

            Vn = PP.RET_VNAMES()

            Tf = T
            Pf = P

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
                result = New Object() {L, V, Vx1, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
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

            Dim Vmin, Vmax, g0 As Double
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

            g0 = 0.0#
            For i = 0 To n
                g0 += Vz(i) * (Ki(i) - 1) / (V + (1 - V) * Ki(i))
            Next

            If g0 > 0 Then Vmin = V Else Vmax = V

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
                    Vx1(i) = Vy(i) / Ki(i)
                    If Vy(i) < 0 Then Vy(i) = 0
                    If Vx1(i) < 0 Then Vx1(i) = 0
                Else
                    Vy(i) = 0
                    Vx1(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            Vx1 = Vx1.NormalizeY
            Vy = Vy.NormalizeY

            Dim initval(n) As Double
            Dim lconstr(n) As Double
            Dim uconstr(n) As Double
            Dim finalval(n) As Double

            'F = 1000.0#

            Dim maxy As Double = Vy.MaxY
            Dim imaxy As Integer = Array.IndexOf(Vy, maxy)

            If maxy * V > Vz(imaxy) Then
                V = Vz(imaxy) / maxy * 0.8
            End If

            V = V '* F

            If V <= 0.0# Then V = 0.2

            For i = 0 To n
                initval(i) = Vy(i) * V
                lconstr(i) = 0.0#
                uconstr(i) = Vz(i)
            Next

            ecount = 0

            ThreePhase = False

            objfunc = ObjFuncType.MinGibbs

            objval = 0.0#
            objval0 = 0.0#

            Dim obj As Double
            Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found

            objval = 0.0#
            objval0 = 0.0#

            Select Case Me.Solver
                Case OptimizationMethod.Limited_Memory_BGFS
                    Dim variables(n) As OptBoundVariable
                    For i = 0 To n
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim solver As New L_BFGS_B
                    solver.Tolerance = etol
                    solver.MaxFunEvaluations = maxit_e
                    initval = solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    solver = Nothing
                Case OptimizationMethod.Truncated_Newton
                    Dim variables(n) As OptBoundVariable
                    For i = 0 To n
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim solver As New TruncatedNewton
                    solver.Tolerance = etol
                    solver.MaxFunEvaluations = maxit_e
                    initval = solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    solver = Nothing
                Case OptimizationMethod.Simplex
                    Dim variables(n) As OptBoundVariable
                    For i = 0 To n
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim solver As New Simplex
                    solver.Tolerance = etol
                    solver.MaxFunEvaluations = maxit_e
                    initval = solver.ComputeMin(AddressOf FunctionValue, variables)
                    solver = Nothing
                Case OptimizationMethod.IPOPT
                    Calculator.CheckParallelPInvoke()
                    Using problem As New Ipopt(initval.Length, lconstr, uconstr, 0, Nothing, Nothing, _
                           0, 0, AddressOf eval_f, AddressOf eval_g, _
                           AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                        problem.AddOption("tol", etol)
                        problem.AddOption("max_iter", maxit_e)
                        problem.AddOption("mu_strategy", "adaptive")
                        problem.AddOption("hessian_approximation", "limited-memory")
                        status = problem.SolveProblem(initval, obj, Nothing, Nothing, Nothing, Nothing)
                    End Using
                Case OptimizationMethod.DifferentialEvolution, OptimizationMethod.GradientDescent, OptimizationMethod.LocalUnimodalSampling,
                    OptimizationMethod.ManyOptimizingLiaisons, OptimizationMethod.Mesh, OptimizationMethod.ParticleSwarm, OptimizationMethod.ParticleSwarmOptimization

                    SwarmOps.Globals.Random = New RandomOps.MersenneTwister()

                    Dim sproblem As New GibbsProblem(Me) With {._Dim = initval.Length, ._LB = lconstr, ._UB = uconstr, ._INIT = initval, ._Name = "Gibbs"}
                    sproblem.MaxIterations = maxit_e * initval.Length * 10
                    sproblem.MinIterations = maxit_e * 10
                    sproblem.Tolerance = 0.0000000000000001
                    Dim opt As SwarmOps.Optimizer = GetSolver(Solver)
                    opt.Problem = sproblem
                    opt.RequireFeasible = True
                    Dim sresult = opt.Optimize(opt.DefaultParameters)

                    If Not sresult.Feasible Or Not CheckSolution() Then Throw New Exception("PT Flash [GM]: Feasible solution not found after " & sresult.Iterations & " iterations.")

                    initval = sresult.Parameters

            End Select

            For i = 0 To initval.Length - 1
                If Double.IsNaN(initval(i)) Then initval(i) = 0.0#
            Next

            Ki = Vy.DivideY(Vx1)

            'check if the algorithm converged to the trivial solution.
            If PP.AUX_CheckTrivial(Ki) Then
                'rollback to NL PT flash.
                WriteDebugInfo("PT Flash [GM]: Converged to the trivial solution at specified conditions. Rolling back to Nested-Loops PT-Flash...")
                result = _nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
            ElseIf status = IpoptReturnCode.Maximum_Iterations_Exceeded Then
                'retry with NL PT flash.
                WriteDebugInfo("PT Flash [GM]: Maximum iterations exceeded. Recalculating with Nested-Loops PT-Flash...")
                result = _nl.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
            Else
                FunctionValue(initval)
                result = New Object() {L, V, Vx1, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            End If

            'if two-phase only, no need to do stability check on the liquid phase.

            If ForceTwoPhaseOnly = False Then

                ' check if there is a liquid phase

                If result(0) > 0 Then ' we have a liquid phase

                    'If result(1) > 0.01 And n = 1 Then
                    '    'the liquid phase cannot be unstable when there's also vapor and only two compounds in the system.
                    '    Return result
                    'End If

                    Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                    Dim nc As Integer = Vz.Length - 1

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
                        Dim m As Double = LBound(stresult(1), 1)
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
                            For i = 0 To Vz.Length - 1
                                vx2est(i) = stresult(1)(k, i)
                            Next
                        Else
                            For i = 0 To Vz.Length - 1
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

                            Dim initval2(2 * n + 1) As Double
                            Dim lconstr2(2 * n + 1) As Double
                            Dim uconstr2(2 * n + 1) As Double
                            Dim finalval2(2 * n + 1) As Double
                            Dim glow(n), gup(n), g(n) As Double

                            Dim maxl As Double = MathEx.Common.Max(vx2est)
                            Dim imaxl As Integer = Array.IndexOf(vx2est, maxl)

                            F = 1000.0#
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
                                If Vz(i) <> 0 Then
                                    initval2(i) = Vy(i) * V - vx2est(i) * L2
                                    If initval2(i) < 0.0# Then initval2(i) = 0.0#
                                Else
                                    initval2(i) = 0.0#
                                End If
                                lconstr2(i) = 0.0#
                                uconstr2(i) = fi(i) * F
                                glow(i) = 0.0#
                                gup(i) = 1000.0#
                                If initval2(i) > uconstr2(i) Then initval2(i) = uconstr2(i)
                            Next
                            For i = n + 1 To 2 * n + 1
                                If Vz(i - n - 1) <> 0 Then
                                    initval2(i) = (vx2est(i - n - 1) * L2)
                                    If initval2(i) < 0 Then initval2(i) = 0
                                Else
                                    initval2(i) = 0.0#
                                End If
                                lconstr2(i) = 0.0#
                                uconstr2(i) = fi(i - n - 1) * F
                                If initval2(i) > uconstr2(i) Then initval2(i) = uconstr2(i)
                            Next

                            ecount = 0

                            ThreePhase = True

                            objval = 0.0#
                            objval0 = 0.0#

                            status = IpoptReturnCode.Invalid_Problem_Definition

                            Select Case Me.Solver
                                Case OptimizationMethod.Limited_Memory_BGFS
                                    Dim variables(2 * n + 1) As OptBoundVariable
                                    For i = 0 To 2 * n + 1
                                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval2(i), False, lconstr2(i), uconstr2(i))
                                    Next
                                    Dim solver As New L_BFGS_B
                                    solver.Tolerance = etol
                                    solver.MaxFunEvaluations = maxit_e
                                    initval2 = solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                                    solver = Nothing
                                Case OptimizationMethod.Truncated_Newton
                                    Dim variables(2 * n + 1) As OptBoundVariable
                                    For i = 0 To 2 * n + 1
                                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval2(i), False, lconstr2(i), uconstr2(i))
                                    Next
                                    Dim solver As New TruncatedNewton
                                    solver.Tolerance = etol
                                    solver.MaxFunEvaluations = maxit_e
                                    initval2 = solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                                    solver = Nothing
                                Case OptimizationMethod.Simplex
                                    Dim variables(2 * n + 1) As OptBoundVariable
                                    For i = 0 To 2 * n + 1
                                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval2(i), False, lconstr2(i), uconstr2(i))
                                    Next
                                    Dim solver As New Simplex
                                    solver.Tolerance = etol
                                    solver.MaxFunEvaluations = maxit_e
                                    initval2 = solver.ComputeMin(AddressOf FunctionValue, variables)
                                    solver = Nothing
                                Case OptimizationMethod.IPOPT
                                    Using problem As New Ipopt(initval2.Length, lconstr2, uconstr2, n + 1, glow, gup, (n + 1) * 2, 0, _
                                            AddressOf eval_f, AddressOf eval_g, _
                                            AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                                        problem.AddOption("tol", etol)
                                        problem.AddOption("max_iter", maxit_e)
                                        problem.AddOption("mu_strategy", "adaptive")
                                        'problem.AddOption("mehrotra_algorithm", "yes")
                                        problem.AddOption("hessian_approximation", "limited-memory")
                                        'problem.SetIntermediateCallback(AddressOf intermediate)
                                        'solve the problem 
                                        status = problem.SolveProblem(initval2, obj, g, Nothing, Nothing, Nothing)
                                    End Using
                                Case OptimizationMethod.DifferentialEvolution, OptimizationMethod.GradientDescent, OptimizationMethod.LocalUnimodalSampling,
                                        OptimizationMethod.ManyOptimizingLiaisons, OptimizationMethod.Mesh, OptimizationMethod.ParticleSwarm, OptimizationMethod.ParticleSwarmOptimization

                                    SwarmOps.Globals.Random = New RandomOps.MersenneTwister()

                                    Dim sproblem As New GibbsProblem(Me) With {._Dim = initval2.Length, ._LB = lconstr2, ._UB = uconstr2, ._INIT = initval2, ._Name = "Gibbs3P"}
                                    sproblem.MaxIterations = maxit_e * initval2.Length * 10
                                    sproblem.MinIterations = maxit_e * 10
                                    sproblem.Tolerance = 1.0E-20
                                    Dim opt As SwarmOps.Optimizer = GetSolver(Solver)
                                    opt.Problem = sproblem
                                    opt.RequireFeasible = True
                                    Dim sresult = opt.Optimize(opt.DefaultParameters)

                                    If Not sresult.Feasible Or Not CheckSolution() Then Throw New Exception("PT Flash [GM]: Feasible solution not found after " & sresult.Iterations & " iterations.")

                                    initval2 = sresult.Parameters

                            End Select

                            For i = 0 To initval2.Length - 1
                                If Double.IsNaN(initval2(i)) Then initval2(i) = 0.0#
                            Next

                            'check if maximum iterations exceeded.
                            If status = IpoptReturnCode.Maximum_Iterations_Exceeded Then
                                'retry with NL PT flash.
                                WriteDebugInfo("PT Flash [GM]: Maximum iterations exceeded. Recalculating with Nested-Loops PT-Flash...")
                                result = _nl3p.Flash_PT(Vz, P, T, PP, ReuseKI, PrevKi)
                                Return result
                            End If

                            FunctionValue(initval2)

                            'order liquid phases by mixture NBP
                            Dim VNBP = PP.RET_VTB()
                            Dim nbp1 As Double = 0
                            Dim nbp2 As Double = 0

                            For i = 0 To n
                                nbp1 += Vx1(i) * VNBP(i)
                                nbp2 += Vx2(i) * VNBP(i)
                            Next

                            If nbp1 >= nbp2 Then
                                result = New Object() {L1 / F, V / F, Vx1, Vy, ecount, L2 / F, Vx2, 0.0#, PP.RET_NullVector}
                            Else
                                result = New Object() {L2 / F, V / F, Vx2, Vy, ecount, L1 / F, Vx1, 0.0#, PP.RET_NullVector}
                            End If

                        End If

                    End If

                End If

            End If

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [GM]: Converged in " & ecount & " iterations. Status: " & [Enum].GetName(GetType(IpoptReturnCode), status) & ". Time taken: " & dt.TotalMilliseconds & " ms")

out:        Return result

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim i, n, ecount As Integer
            Dim Tb, Td As Double
            Dim Span(1) As PointF

            Dim ErrRes As Object

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
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance)
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance)

            Dim Tmin, Tmax As Double

            Tmax = 2000.0#
            Tmin = 50.0#

            If Tref = 0.0# Then Tref = 298.15

            ' ============= Calculate Dew point and boiling point as span endpoints =================

            Dim alreadymt As Boolean = False

            If Settings.EnableParallelProcessing Then

                Dim task1 As Task = New Task(Sub()
                                                 Dim ErrRes1 = Herror("PV", 0, P, Vz, PP)
                                                 Span(0).X = 0
                                                 Span(0).Y = ErrRes1(0)
                                                 Tb = ErrRes1(1)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 Dim ErrRes2 = Herror("PV", 1, P, Vz, PP)
                                                 Span(1).X = 1
                                                 Span(1).Y = ErrRes2(0)
                                                 Td = ErrRes2(1)
                                             End Sub)
                task1.Start()
                task2.Start()
                Task.WaitAll(task1, task2)

            Else
                ErrRes = Herror("PV", 0, P, Vz, PP)
                Tb = ErrRes(1)
                Span(0).X = 0
                Span(0).Y = ErrRes(0) 'Enthalpy at boiling point

                ErrRes = Herror("PV", 1, P, Vz, PP)
                Td = ErrRes(1)
                Span(1).X = 1
                Span(1).Y = ErrRes(0) 'Enthalpy at dew point
            End If

            If Span(0).Y > 0 And Span(1).Y < 0 Then
                'specified enthalpy requires partial evaporation 
                'calculate vapour fraction
                Dim Vn, Hn As Double

                ecount = 0
                Do
                    'First step: Interpolation of solutions
                    ecount += 1
                    Vn = Span(0).X + (Span(1).X - Span(0).X) * (0 - Span(0).Y) / (Span(1).Y - Span(0).Y) 'new vapour fraction by interpolation
                    ErrRes = Herror("PV", Vn, P, Vz, PP)
                    Hn = ErrRes(0)
                    If Abs(Hn) < itol Then Exit Do 'Solution within tolerance, exit search

                    'Adjust search span
                    If Hn < 0 Then
                        Span(1).X = Vn
                        Span(1).Y = Hn
                    Else
                        Span(0).X = Vn
                        Span(0).Y = Hn
                    End If

                    'Second step: Span dividing
                    ecount += 1
                    Vn = (Span(0).X + Span(1).X) / 2 'new vapour fraction by span division
                    ErrRes = Herror("PV", Vn, P, Vz, PP)
                    Hn = ErrRes(0)
                    If Abs(Hn) < itol Then Exit Do 'Solution within tolerance, exit search

                    'Adjust search span
                    If Hn < 0 Then
                        Span(1).X = Vn
                        Span(1).Y = Hn
                    Else
                        Span(0).X = Vn
                        Span(0).Y = Hn
                    End If
                Loop Until ecount > maxitEXT

                T = ErrRes(1)
                V = Vn
                L1 = ErrRes(3)
                L2 = ErrRes(4)
                Vy = ErrRes(5)
                Vx1 = ErrRes(6)
                Vx2 = ErrRes(7)
                For i = 0 To n
                    Ki(i) = Vy(i) / Vx1(i)
                Next

            ElseIf Span(1).Y > 0 Then 'only gas phase
                'calculate temperature

                Dim H1, H2, T1, T2 As Double
                ecount = 0
                T = Td
                H1 = Span(1).Y
                Do
                    ecount += 1
                    T1 = T
                    T2 = T1 + 1
                    H2 = Hf - proppack.DW_CalcEnthalpy(Vz, T2, P, State.Vapor)
                    T = T1 + (T2 - T1) * (0 - H1) / (H2 - H1)
                    H1 = Hf - proppack.DW_CalcEnthalpy(Vz, T, P, State.Vapor)
                Loop Until Abs(H1) < itol Or ecount > maxitEXT

                L1 = 0
                V = 1
                Vy = Vz.Clone
                Vx1 = Vz.Clone
                Vx2 = Vz.Clone
                L2 = 0
                For i = 0 To n
                    Ki(i) = 1
                Next

                If T <= Tmin Or T >= Tmax Then Throw New Exception("PH Flash [NL3PV3]: Invalid result: Temperature did not converge.")
            Else
                'specified enthalpy requires pure liquid 
                'calculate temperature

                Dim H1, H2, T1, T2 As Double
                ecount = 0
                T = Tb
                H1 = Span(0).Y
                Do
                    ecount += 1
                    T1 = T
                    T2 = T1 - 1
                    H2 = Herror("PT", T2, P, Vz, PP)(0)
                    T = T1 + (T2 - T1) * (0 - H1) / (H2 - H1)
                    ErrRes = Herror("PT", T, P, Vz, PP)
                    H1 = ErrRes(0)
                Loop Until Abs(H1) < itol Or ecount > maxitEXT

                V = 0
                L1 = ErrRes(3)
                L2 = ErrRes(4)
                Vy = ErrRes(5)
                Vx1 = ErrRes(6)
                Vx2 = ErrRes(7)

                For i = 0 To n
                    Ki(i) = Vy(i) / Vx1(i)
                Next

                If T <= Tmin Or T >= Tmax Or ecount > maxitEXT Then Throw New Exception("PH Flash [NL3PV3]: Invalid result: Temperature did not converge.")
            End If

            If ecount > maxitEXT Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))

            d2 = Date.Now
            dt = d2 - d1

            WriteDebugInfo("PH Flash [NL-3PV3]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms")

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

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
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance)
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance)

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

                    If Settings.EnableParallelProcessing Then

                        Dim task1 As Task = New Task(Sub()
                                                         fx = Serror(x1, {P, Vz, PP})
                                                     End Sub)
                        Dim task2 As Task = New Task(Sub()
                                                         fx2 = Serror(x1 + epsilon(j), {P, Vz, PP})
                                                     End Sub)
                        task1.Start()
                        task2.Start()
                        Task.WaitAll(task1, task2)

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

            Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal Type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object

            Dim n = Vz.Length - 1
            Dim L1, L2, V, Vx1(), Vx2(), Vy() As Double

            If Type = "PT" Then
                Dim tmp = Me.Flash_PT(Vz, P, X, PP)
                L1 = tmp(0)
                V = tmp(1)
                Vx1 = tmp(2)
                Vy = tmp(3)
                L2 = tmp(5)
                Vx2 = tmp(6)
                T = X
            Else
                Dim tmp = Me.Flash_PV(Vz, P, X, 298.15, PP)
                L1 = tmp(0)
                V = tmp(1)
                Vx1 = tmp(2)
                Vy = tmp(3)
                T = tmp(4)
                L2 = tmp(7)
                Vx2 = tmp(8)
            End If

            Dim _Hv, _Hl1, _Hl2 As Double

            _Hv = 0.0#
            _Hl1 = 0.0#
            _Hl2 = 0.0#

            Dim alreadymt As Boolean = False

            If Settings.EnableParallelProcessing Then

                Dim task1 As Task = New Task(Sub()
                                                 If V > 0 Then _Hv = proppack.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 If L1 > 0 Then _Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, P, State.Liquid)
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 If L2 > 0 Then _Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)

            Else
                If V > 0 Then _Hv = proppack.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                If L1 > 0 Then _Hl1 = proppack.DW_CalcEnthalpy(Vx1, T, P, State.Liquid)
                If L2 > 0 Then _Hl2 = proppack.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
            End If

            Dim mmg, mml, mml2 As Double
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx1)
            mml2 = proppack.AUX_MMM(Vx2)

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2)) * _Hv - (mml * L1 / (mmg * V + mml * L1 + mml2 * L2)) * _Hl1 - (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2)) * _Hl2
            OBJ_FUNC_PH_FLASH = {herr, T, V, L1, L2, Vy, Vx1, Vx2}

            WriteDebugInfo("PH Flash [NL-3PV3]: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object) As Object

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

            WriteDebugInfo("PS Flash [NL-3PV3]: Current T = " & T & ", Current S Error = " & serr)

        End Function

        Function Herror(ByVal type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object
            proppack.CurrentMaterialStream.Flowsheet.CheckStatus()
            Return OBJ_FUNC_PH_FLASH(type, X, P, Vz, PP)
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            proppack.CurrentMaterialStream.Flowsheet.CheckStatus()
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, Pf, fi)
        End Function

        Function ESTIMAR_T_H(ByVal HT As Double, ByVal Tref As Double, ByVal TIPO As String, ByVal P As Double, ByVal Vz As Array) As Double

            Dim maxit As Integer = Convert.ToInt32(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations))
            Dim tol As Double = Convert.ToDouble(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance))

            Dim cnt As Integer = 0
            Dim Tant, Tant2, fi_, fip_, dfdT, fi_ant, fi_ant2 As Double

            Tf = Tref
            Do
                fi_ant2 = fi_ant
                fi_ant = fi_
                If TIPO = "L" Then
                    fi_ = HT - proppack.DW_CalcEnthalpy(Vz, Tf, Pf, State.Liquid)
                Else
                    fi_ = HT - proppack.DW_CalcEnthalpy(Vz, Tf, Pf, State.Vapor)
                End If
                If cnt <= 1 Then
                    Tant2 = Tant
                    Tant = Tf
                    Tf = Tf * 1.1
                Else
                    Tant2 = Tant
                    Tant = Tf
                    fip_ = HT - proppack.DW_CalcEnthalpy(Vz, Tf + 0.01, Pf, State.Vapor)
                    dfdT = (fip_ - fi_) / 0.01
                    Tf = Tf - fi_ / dfdT
                End If
                If Tf < 0 Then
                    Tf = Tant * 1.01
                End If
                cnt += 1
                If cnt >= maxit Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))
            Loop Until Math.Abs(fi_ / HT) < tol Or Double.IsNaN(Tf) Or Abs(Tf - Tant) < tol

            Return Tf

        End Function

        Function ESTIMAR_T_S(ByVal ST As Double, ByVal Tref As Double, ByVal TIPO As String, ByVal P As Double, ByVal Vz As Array) As Double

            Dim maxit As Integer = Convert.ToInt32(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations))
            Dim tol As Double = Convert.ToDouble(Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance))

            Dim cnt As Integer = 0
            Dim Tant, Tant2, fi_, fip_, dfdT, fi_ant, fi_ant2 As Double

            Tf = Tref
            Do
                fi_ant2 = fi_ant
                fi_ant = fi_
                If TIPO = "L" Then
                    fi_ = ST - proppack.DW_CalcEntropy(Vz, Tf, Pf, State.Liquid)
                Else
                    fi_ = ST - proppack.DW_CalcEntropy(Vz, Tf, Pf, State.Vapor)
                End If
                If cnt <= 1 Then
                    Tant2 = Tant
                    Tant = Tf
                    Tf = Tf * 1.1
                Else
                    Tant2 = Tant
                    Tant = Tf
                    fip_ = ST - proppack.DW_CalcEntropy(Vz, Tf + 0.01, Pf, State.Vapor)
                    dfdT = (fip_ - fi_) / 0.01
                    Tf = Tf - fi_ / dfdT
                End If
                If Tf < 0 Then
                    Tf = Tant * 1.01
                End If
                cnt += 1
                If cnt >= maxit Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))
            Loop Until Math.Abs(fi_ / ST) < tol Or Double.IsNaN(Tf) Or Abs(Tf - Tant) < tol

            Return Tf

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, j, k As Integer

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            Dim _nl As New DWSIMDefault

            Dim result As Object = _nl.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)

            P = result(4)

            If result(0) > 0 And Not ForceTwoPhaseOnly Then

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = Vz.Length - 1

                If nt = -1 Then nt = nc
                n = nc

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
                        For i = 0 To Vz.Length - 1
                            vx2est(i) = stresult(1)(k, i)
                        Next
                    Else
                        For i = 0 To Vz.Length - 1
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

            WriteDebugInfo("TV Flash [GM-3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, j, k As Integer

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            Dim _nl As New DWSIMDefault

            Dim result As Object = _nl.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)

            T = result(4)

            If result(0) > 0 And Not ForceTwoPhaseOnly Then

                Dim nt As Integer = Me.StabSearchCompIDs.Length - 1
                Dim nc As Integer = Vz.Length - 1

                If nt = -1 Then nt = nc
                n = nc

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
                        For i = 0 To Vz.Length - 1
                            vx2est(i) = stresult(1)(k, i)
                        Next
                    Else
                        For i = 0 To Vz.Length - 1
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

            WriteDebugInfo("PV Flash [GM-3P]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return result

        End Function

        Public Function Flash_PV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi() As Double = Nothing) As Object

            Dim i As Integer

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance)
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki1(n), Ki2(n), fi(n)
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

            WriteDebugInfo("PV Flash [GM-3P]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", T = " & T)

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
                Dim Vx1ant(n), Vx2ant(n), Vyant(n)
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

                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))

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

                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))

                ecount += 1

                WriteDebugInfo("PV Flash [GM-3P]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", T = " & T)

            Loop

out:        Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        Public Function Flash_TV_3P(ByVal Vz() As Double, ByVal Vest As Double, ByVal L1est As Double, ByVal L2est As Double, ByVal VyEST As Double(), ByVal Vx1EST As Double(), ByVal Vx2EST As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackage) As Object

            Dim i As Integer

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance)
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance)
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            proppack = PP

            ReDim Vn(n), Vx1(n), Vx2(n), Vy(n), Vp(n), Ki1(n), Ki2(n), fi(n)
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

            WriteDebugInfo("TV Flash [GM-3P]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", P = " & P)

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
                Dim Vx1ant(n), Vx2ant(n), Vyant(n)
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

                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))

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

                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt"))

                ecount += 1

                WriteDebugInfo("TV Flash [GM-3P]: Iteration #" & ecount & ", VF = " & V & ", L1 = " & L1 & ", P = " & P)

            Loop

out:        Return New Object() {L1, V, Vx1, Vy, P, ecount, Ki1, L2, Vx2, 0.0#, PP.RET_NullVector}

        End Function

        'Function Values

        Public Function FunctionValue(ByVal x() As Double) As Double

            proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim pval As Double = 0.0#
            Dim fcv(n), fcl(n), fcl2(n) As Double

            Select Case objfunc

                Case ObjFuncType.MinGibbs

                    If Not ThreePhase Then

                        soma_y = MathEx.Common.Sum(x)
                        V = soma_y
                        L = 1 - soma_y

                        For i = 0 To x.Length - 1
                            If V <> 0.0# Then Vy(i) = Abs(x(i) / V) Else Vy(i) = 0.0#
                            If L <> 0.0# Then Vx1(i) = Abs((fi(i) - x(i)) / L) Else Vx1(i) = 0.0#
                        Next

                        If Settings.EnableParallelProcessing Then

                            Dim task1 As Task = New Task(Sub()
                                                             fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                                                         End Sub)
                            Dim task2 As Task = New Task(Sub()
                                                             fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                                                         End Sub)
                            task1.Start()
                            task2.Start()
                            Task.WaitAll(task1, task2)

                        Else
                            fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                            fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                        End If

                        Gv = 0
                        Gl1 = 0
                        For i = 0 To x.Length - 1
                            If Vy(i) <> 0.0# Then Gv += Vy(i) * V * Log(fcv(i) * Vy(i))
                            If Vx1(i) <> 0.0# Then Gl1 += Vx1(i) * L * Log(fcl(i) * Vx1(i))
                        Next

                        Gm = Gv + Gl1

                        WriteDebugInfo("[GM] V = " & Format(V, "N4") & ", L = " & Format(L, "N4") & " / GE = " & Format(Gm * 8.314 * Tf, "N2") & " kJ/kmol")

                    Else

                        soma_y = 0
                        For i = 0 To x.Length - n - 2
                            soma_y += x(i)
                        Next
                        soma_x2 = 0
                        For i = x.Length - n - 1 To x.Length - 1
                            soma_x2 += x(i)
                        Next
                        V = soma_y
                        L = F - soma_y
                        L2 = soma_x2
                        L1 = F - V - L2

                        pval = 0.0#
                        For i = 0 To n
                            If V <> 0.0# Then Vy(i) = (x(i) / V) Else Vy(i) = 0.0#
                            If L2 <> 0.0# Then Vx2(i) = (x(i + n + 1) / L2) Else Vx2(i) = 0.0#
                            If L1 <> 0.0# Then Vx1(i) = ((fi(i) * F - Vy(i) * V - Vx2(i) * L2) / L1) Else Vx1(i) = 0.0#
                            If Vy(i) < 0.0# Then Vy(i) = 1.0E-20
                            If Vx1(i) < 0.0# Then Vx1(i) = 1.0E-20
                            If Vx2(i) < 0.0# Then Vx2(i) = 1.0E-20
                        Next

                        soma_x1 = 0
                        For i = 0 To n
                            soma_x1 += Vx1(i)
                        Next
                        For i = 0 To n
                            If soma_x1 <> 0.0# Then Vx1(i) /= soma_x1
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
                            task1.Start()
                            task2.Start()
                            task3.Start()
                            Task.WaitAll(task1, task2, task3)

                        Else
                            fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                            fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                            fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                        End If

                        Gv = 0
                        Gl1 = 0
                        Gl2 = 0
                        For i = 0 To n
                            If Vy(i) <> 0 Then Gv += Vy(i) * V * Log(fcv(i) * Vy(i))
                            If Vx1(i) <> 0 Then Gl1 += Vx1(i) * L1 * Log(fcl(i) * Vx1(i))
                            If Vx2(i) <> 0 Then Gl2 += Vx2(i) * L2 * Log(fcl2(i) * Vx2(i))
                        Next

                        Gm = Gv + Gl1 + Gl2 + pval

                        WriteDebugInfo("[GM] V = " & Format(V / 1000, "N4") & ", L1 = " & Format(L1 / 1000, "N4") & ", L2 = " & Format(L2 / 1000, "N4") & " / GE = " & Format(Gm * 8.314 * Tf / 1000, "N2") & " kJ/kmol")

                    End If

                    ecount += 1

                    Return Gm

                Case Else

                    L1 = L1sat
                    L2 = 1 - V - L1

                    Dim sumx As Double = 0
                    For i = 0 To n
                        sumx += x(i)
                    Next

                    Select Case objfunc
                        Case ObjFuncType.BubblePointP, ObjFuncType.DewPointP
                            Pf = x(n + 1)
                        Case ObjFuncType.BubblePointT, ObjFuncType.DewPointT
                            Tf = x(n + 1)
                    End Select

                    Select Case objfunc
                        Case ObjFuncType.BubblePointP, ObjFuncType.BubblePointT
                            For i = 0 To n
                                Vy(i) = x(i) / sumx
                                Vx1(i) = fi(i)
                                Vx2(i) = ((L1 + 0.0000000001) * Vx1(i) - (V + 0.0000000001) * Vy(i)) / L2
                                If L2 = 0.0# Then Vx2(i) = 0
                            Next
                        Case ObjFuncType.DewPointP, ObjFuncType.DewPointT
                            For i = 0 To n
                                Vy(i) = fi(i)
                                Vx1(i) = x(i) / sumx
                                Vx2(i) = ((L1 + 0.0000000001) * Vx1(i) - (V + 0.0000000001) * Vy(i)) / L2
                                If L2 = 0.0# Then Vx2(i) = 0
                            Next
                    End Select

                    For i = 0 To n
                        If Vx2(i) <= 0 Then Vx2(i) = 1.0E-20
                    Next

                    soma_x2 = 0
                    For i = 0 To n
                        soma_x2 += Vx2(i)
                    Next
                    For i = 0 To n
                        Vx2(i) /= soma_x2
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
                        task1.Start()
                        task2.Start()
                        task3.Start()
                        Task.WaitAll(task1, task2, task3)

                    Else
                        fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                        fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                        fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                    End If

                    Gv = 0
                    Gl1 = 0
                    Gl2 = 0
                    For i = 0 To n
                        If Vy(i) <> 0 Then Gv += Vy(i) * V * Log(fcv(i) * Vy(i))
                        If Vx1(i) <> 0 Then Gl1 += Vx1(i) * L1 * Log(fcl(i) * Vx1(i))
                        If Vx2(i) <> 0 Then Gl2 += Vx2(i) * L2 * Log(fcl2(i) * Vx2(i))
                    Next

                    pval = 0.0#

                    Gm = Gv + Gl1 + Gl2 + pval

                    ecount += 1

                    Return Gm

            End Select

        End Function

        Public Function FunctionGradient(ByVal x() As Double) As Double()

            Dim g(x.Length - 1) As Double
            Dim epsilon As Double = 0.000001
            Dim fcv(x.Length - 1), fcl(x.Length - 1), fcl2(x.Length - 1) As Double
            Dim i As Integer

            Select Case objfunc

                Case ObjFuncType.MinGibbs

                    If Not ThreePhase Then

                        soma_y = MathEx.Common.Sum(x)
                        V = soma_y
                        L = 1 - soma_y

                        For i = 0 To x.Length - 1
                            Vy(i) = Abs(x(i) / V)
                            Vx1(i) = Abs((fi(i) - x(i)) / L)
                        Next

                        If Settings.EnableParallelProcessing Then

                            Dim task1 As Task = New Task(Sub()
                                                             fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                                                         End Sub)
                            Dim task2 As Task = New Task(Sub()
                                                             fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                                                         End Sub)
                            task1.Start()
                            task2.Start()
                            Task.WaitAll(task1, task2)

                        Else
                            fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                            fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                        End If

                        For i = 0 To x.Length - 1
                            If Vy(i) <> 0 And Vx1(i) <> 0 Then g(i) = Log(fcv(i) * Vy(i) / (fcl(i) * Vx1(i)))
                        Next

                    Else

                        soma_y = 0
                        For i = 0 To x.Length - n - 2
                            soma_y += x(i)
                        Next
                        soma_x2 = 0
                        For i = x.Length - n - 1 To x.Length - 1
                            soma_x2 += x(i)
                        Next
                        V = soma_y
                        L = F - soma_y
                        L2 = soma_x2
                        L1 = F - V - L2

                        For i = 0 To n
                            If V <> 0.0# Then Vy(i) = (x(i) / V) Else Vy(i) = 0.0#
                            If L2 <> 0.0# Then Vx2(i) = (x(i + n + 1) / L2) Else Vx2(i) = 0.0#
                            If L1 <> 0.0# Then Vx1(i) = ((fi(i) * F - Vy(i) * V - Vx2(i) * L2) / L1) Else Vx1(i) = 0.0#
                            If Vy(i) < 0.0# Then Vy(i) = 1.0E-20
                            If Vx1(i) < 0.0# Then Vx1(i) = 1.0E-20
                            If Vx2(i) < 0.0# Then Vx2(i) = 1.0E-20
                        Next

                        soma_x1 = 0
                        For i = 0 To n
                            soma_x1 += Vx1(i)
                        Next
                        For i = 0 To n
                            Vx1(i) /= soma_x1
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
                            task1.Start()
                            task2.Start()
                            task3.Start()
                            Task.WaitAll(task1, task2, task3)

                        Else
                            fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                            fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                            fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                        End If

                        For i = 0 To x.Length - n - 2
                            If Vy(i) <> 0 And Vx2(i) <> 0 Then g(i) = Log(fcv(i) * Vy(i)) - Log(fcl(i) * Vx1(i))
                        Next
                        For i = x.Length - n - 1 To (x.Length - 1)
                            If Vx1(i - (x.Length - n - 1)) <> 0 And Vx2(i - (x.Length - n - 1)) <> 0 Then g(i) = Log(fcl2(i - (x.Length - n - 1)) * Vx2(i - (x.Length - n - 1))) - Log(fcl(i - (x.Length - n - 1)) * Vx1(i - (x.Length - n - 1)))
                        Next

                    End If

                    Return g

                Case Else

                    L1 = L1sat
                    L2 = 1 - V - L1

                    Dim sumx As Double = 0
                    For i = 0 To n
                        sumx += x(i)
                    Next

                    Select Case objfunc
                        Case ObjFuncType.BubblePointP, ObjFuncType.DewPointP
                            Pf = x(n + 1)
                        Case ObjFuncType.BubblePointT, ObjFuncType.DewPointT
                            Tf = x(n + 1)
                    End Select

                    Select Case objfunc
                        Case ObjFuncType.BubblePointP, ObjFuncType.BubblePointT
                            For i = 0 To n
                                Vy(i) = x(i) / sumx
                                Vx1(i) = fi(i)
                                Vx2(i) = ((L1 + 0.0000000001) * Vx1(i) - (V + 0.0000000001) * Vy(i)) / L2
                                If L2 = 0.0# Then Vx2(i) = 0
                            Next
                        Case ObjFuncType.DewPointP, ObjFuncType.DewPointT
                            For i = 0 To n
                                Vy(i) = fi(i)
                                Vx1(i) = x(i) / sumx
                                Vx2(i) = ((L1 + 0.0000000001) * Vx1(i) - (V + 0.0000000001) * Vy(i)) / L2
                                If L2 = 0.0# Then Vx2(i) = 0
                            Next
                    End Select

                    For i = 0 To n
                        If Vx2(i) <= 0 Then Vx2(i) = 1.0E-20
                    Next

                    soma_x2 = 0
                    For i = 0 To n
                        soma_x2 += Vx2(i)
                    Next
                    For i = 0 To n
                        Vx2(i) /= soma_x2
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
                        task1.Start()
                        task2.Start()
                        task3.Start()
                        Task.WaitAll(task1, task2, task3)

                    Else
                        fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                        fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                        fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                    End If

                    Select Case objfunc
                        Case ObjFuncType.BubblePointP, ObjFuncType.BubblePointT
                            For i = 0 To n
                                If Vy(i) <> 0 And Vx1(i) <> 0 Then g(i) = Log(fcl(i) * Vx1(i)) - Log(fcv(i) * Vy(i))
                            Next
                        Case ObjFuncType.DewPointP, ObjFuncType.DewPointT
                            For i = 0 To n
                                If Vy(i) <> 0 And Vx1(i) <> 0 Then g(i) = Log(fcv(i) * Vy(i)) - Log(fcl(i) * Vx1(i))
                            Next
                    End Select
                    Dim xg1, xg2 As Double()
                    xg1 = x.Clone
                    xg2 = x.Clone
                    xg2(n + 1) *= 1.01
                    g(n + 1) = (FunctionValue(xg2) - FunctionValue(xg1)) / (0.01 * xg1(n + 1))

                    Return g

            End Select


        End Function

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
                g(i) = fi(i) * F - x(i) - x(i + m)
            Next
            Return True
        End Function

        Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(), _
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

        Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(), _
         ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean

            If values Is Nothing Then

                Dim row(nele_hess - 1), col(nele_hess - 1) As Integer

                'k = 0
                'For i = 0 To n - 1
                '    For j = 0 To n - 1
                '        row(k) = i
                '        col(k) = j
                '        k += 1
                '    Next
                'Next

                iRow = row
                jCol = col

            Else

                values = FunctionHessian(x)

            End If

            Return True

        End Function

        Public Function intermediate(ByVal alg_mod As IpoptAlgorithmMode, ByVal iter_count As Integer, ByVal obj_value As Double, _
                                     ByVal inf_pr As Double, ByVal inf_du As Double, ByVal mu As Double, _
                                     ByVal d_norm As Double, ByVal regularization_size As Double, ByVal alpha_du As Double, _
                                     ByVal alpha_pr As Double, ByVal ls_trials As Integer) As Boolean
            objval0 = objval
            objval = obj_value
            Return True
        End Function

    End Class

    Public Class GibbsProblem

        Inherits SwarmOps.Problem

        Public _Dim As Integer, _LB(), _UB(), _INIT() As Double, _Name As String

        Private _gf As GibbsMinimization3P
        Private _fit As Double

        Sub New(gf As GibbsMinimization3P)
            _gf = gf
        End Sub

        Public Overrides ReadOnly Property Dimensionality As Integer
            Get
                Return _Dim
            End Get
        End Property

        Public Overrides ReadOnly Property LowerBound As Double()
            Get
                Return _LB
            End Get
        End Property

        Public Overrides ReadOnly Property LowerInit As Double()
            Get
                Return _INIT
            End Get
        End Property
        Public Overrides ReadOnly Property UpperInit As Double()
            Get
                Return _INIT
            End Get
        End Property

        Public Overrides ReadOnly Property MinFitness As Double
            Get
                Return Double.MinValue
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return _Name
            End Get
        End Property

        Public Overrides ReadOnly Property UpperBound As Double()
            Get
                Return _UB
            End Get
        End Property

        Public Overrides ReadOnly Property HasGradient As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function Gradient(x() As Double, ByRef v() As Double) As Integer

            v = _gf.FunctionGradient(x)

            Return 0

        End Function

        Public Overrides Function Fitness(parameters() As Double) As Double

            Return _gf.FunctionValue(parameters)

        End Function

        Public Overrides Function Feasible(parameters() As Double) As Boolean

            Dim n = parameters.Length / 2 - 1

            If _gf.threephase Then
                Dim constraints(n) As Double
                _gf.eval_g(n + 1, parameters, False, n + 1, constraints)
                Dim valid As Boolean = True
                For i = 0 To n
                    If constraints(i) < 0.0# Or constraints(i) > 1000.0# Then
                        valid = False
                        Exit For
                    End If
                Next
                Return MyBase.Feasible(parameters) And valid
            Else
                Return MyBase.Feasible(parameters)
            End If

        End Function

    End Class

End Namespace
