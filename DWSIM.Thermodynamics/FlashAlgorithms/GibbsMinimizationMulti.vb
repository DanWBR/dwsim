'    Multiphase Gibbs Minimization Flash Algorithms
'    Copyright 2012-2022 Daniel Wagner O. de Medeiros
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
Imports Cureos.Numerics
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses

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

        Dim Solutions As List(Of Double())
        Dim GibbsEnergyValues As List(Of Double)

        Dim G0 As Double

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
                Return "Gibbs Minimization SVLLE"
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

            'If Me.Solver = OptimizationMethod.IPOPT Then Calculator.CheckParallelPInvoke()

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

            Gzv = 0.0
            Gzl = 0.0
            Gzs = 0.0
            For i = 0 To n
                If Vz(i) > 0.0 Then
                    Gzv += Vz(i) * Log(fczv(i) * Vz(i))
                    Gzl += Vz(i) * Log(fczl(i) * Vz(i))
                    Gzs += Vz(i) * Log(fczs(i) * Vz(i))
                End If
            Next

            Gz0 = {Gzv, Gzl, Gzs}.Min * 1000

            Dim mixphase As PhaseName

            If Gz0 = Gzv * 1000 Then
                mixphase = PhaseName.Vapor
            ElseIf Gz0 = Gzl * 1000 Then
                mixphase = PhaseName.Liquid
            Else
                mixphase = PhaseName.Solid
            End If

            G0 = Gz0

            'Calculate Ki`s

            If Not ReuseKI Then
                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Ki(i) = Vp(i) / P
                    If Double.IsNaN(Ki(i)) Or Double.IsInfinity(Ki(i)) Then Ki(i) = 1.0E+20
                    i += 1
                Loop Until i = n + 1
            Else
                For i = 0 To n
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Ki(i) = PrevKi(i)
                    If Double.IsNaN(Ki(i)) Or Double.IsInfinity(Ki(i)) Then Ki(i) = 1.0E+20
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

            Solutions = New List(Of Double())
            GibbsEnergyValues = New List(Of Double)

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

                Dim Vz2 = Vz.Clone

                Dim VTfus As Double() = PP.RET_VTF

                For i = 0 To n
                    If Vz(i) > 0.0 And T < VTfus(i) Or PP.ForcedSolids.Contains(Vn(i)) Then
                        Vs(i) = Vz(i)
                        Vz2(i) = 0.0
                    End If
                Next

                Sx = Vs.Sum

                Dim rnl = _nl.Flash_PT(Vz2, P, T, PP)

                L1 = rnl(0) * (1 - Sx)
                V = rnl(1) * (1 - Sx)
                Vx1 = rnl(2)
                Vy = rnl(3)

                If L1 = 0.0 Then
                    L1 = 0.0001
                    V = 0.9999
                End If

                L2 = 0.000001

                Dim sum = V + L1 + L2 + Sx

                V = V / sum
                L1 = L1 / sum
                L2 = L2 / sum
                Sx = Sx / sum

                V *= 1000.0
                L1 *= 1000.0
                L2 *= 1000.0
                Sx *= 1000.0

                Dim stresult = StabTest2(T, P, Vx1, PP.RET_VTC, PP)

                If stresult.Count > 0 Then

                    Dim validsolutions = stresult.Where(Function(s) s.Max > 0.5).ToList()

                    If validsolutions.Count > 0 Then
                        'select the composition which gives the lowest gibbs energy.
                        Dim Gt0 As Double = 100000.0, Gt As Double, ft() As Double, it As Integer
                        i = 0
                        For Each trialcomp In validsolutions
                            ft = PP.DW_CalcFugCoeff(trialcomp, T, P, State.Liquid)
                            Gt = 0.0
                            For j = 0 To n
                                If Vz(j) > 0.0 Then
                                    Gt += trialcomp(j) * Log(ft(j) * trialcomp(j))
                                End If
                            Next
                            If Gt < Gt0 Then
                                Gt0 = Gt
                                it = i
                            End If
                            i += 1
                        Next
                        Vx2 = validsolutions(it)
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
                    gup(i) = Vz(i) * F
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

            Dim esolvID = FlashSettings(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolver)
            Dim esolv As IExternalNonLinearMinimizationSolver = Nothing
            If proppack.Flowsheet IsNot Nothing Then
                If proppack.Flowsheet.ExternalSolvers.ContainsKey(esolvID) Then
                    esolv = proppack.Flowsheet.ExternalSolvers(esolvID)
                End If
            End If

            If esolv Is Nothing Then

                Dim IPOPT_Failure As Boolean = True

                Dim problem As Ipopt = Nothing
                Dim ex0 As New Exception

                Try
                    problem = New Ipopt(initval.Length, lconstr, uconstr, 0, Nothing, Nothing, 0, 0,
                           AddressOf eval_f, AddressOf eval_g,
                           AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                    problem.AddOption("print_level", 1)
                    problem.AddOption("tol", 0.0000000001)
                    problem.AddOption("max_iter", 100)
                    problem.AddOption("mu_strategy", "adaptive")
                    problem.AddOption("expect_infeasible_problem", "yes")
                    problem.AddOption("hessian_approximation", "limited-memory")
                    problem.SetIntermediateCallback(AddressOf intermediate)
                    'solve the problem 
                    status = problem.SolveProblem(initval, obj, Nothing, Nothing, Nothing, Nothing)
                    IPOPT_Failure = False
                Catch ex As Exception
                    ex0 = ex
                Finally
                    problem?.Dispose()
                    problem = Nothing
                End Try

                If IPOPT_Failure Then Throw New Exception("Failed to load IPOPT library: " + ex0.Message)

                Select Case status
                    Case IpoptReturnCode.Infeasible_Problem_Detected,
                        IpoptReturnCode.Maximum_Iterations_Exceeded,
                        IpoptReturnCode.User_Requested_Stop,
                        IpoptReturnCode.Solve_Succeeded,
                        IpoptReturnCode.Solved_To_Acceptable_Level
                        'get solution with lowest gibbs energy
                        initval = Solutions(GibbsEnergyValues.IndexOf(GibbsEnergyValues.Min))
                    Case IpoptReturnCode.Diverging_Iterates,
                          IpoptReturnCode.Error_In_Step_Computation,
                          IpoptReturnCode.Internal_Error,
                          IpoptReturnCode.Invalid_Number_Detected,
                          IpoptReturnCode.Invalid_Option,
                          IpoptReturnCode.NonIpopt_Exception_Thrown,
                          IpoptReturnCode.Unrecoverable_Exception
                        Throw New Exception("PT Flash: IPOPT failed to converge.")
                End Select

            Else

                initval = esolv.Solve(AddressOf FunctionValue, AddressOf FunctionGradient, Nothing,
                                        initval, lconstr, uconstr, 100, 0.0000000001)

            End If

            For i = 0 To initval.Length - 1
                If Double.IsNaN(initval(i)) Then initval(i) = 0.0#
            Next

            Gz = FunctionValue(initval)

            Dim mbr As Double = MassBalanceResidual()

            If mbr > 0.01 * n Then
                Throw New Exception("PT Flash: Invalid solution.")
            End If

            If Gz > Gz0 Then
                'mixture is stable. no phase split.
                Select Case mixphase
                    Case PhaseName.Vapor
                        V = F
                        L1 = 0
                        L2 = 0
                        Sx = 0
                        Vy = Vz
                    Case PhaseName.Liquid
                        L1 = F
                        L2 = 0
                        V = 0
                        Sx = 0
                        Vx1 = Vz
                    Case PhaseName.Solid
                        Sx = 1
                        V = 0
                        L1 = 0
                        L2 = 0
                        Vs = Vz
                End Select
            End If

            prevsol = initval

            'check if vapor and liquid1 phases are inverted

            If TypeOf PP Is PengRobinsonPropertyPackage Or TypeOf PP Is PengRobinson1978PropertyPackage Then

                Dim fugvv, fugvl As Double()

                fugvv = PP.DW_CalcFugCoeff(Vy, T, P, State.Vapor)
                fugvl = PP.DW_CalcFugCoeff(Vy, T, P, State.Liquid)

                If fugvv.SubtractY(fugvl).AbsSqrSumY < 0.000001 Then

                    Dim phase = IdentifyPhase(Vy, P, T, PP, "PR")

                    If phase = "L" Then

                        Dim Vhold, Vyhold() As Double
                        Vhold = V
                        V = L1
                        L1 = Vhold

                        Vyhold = Vy.Clone
                        Vy = Vx1.Clone
                        Vx1 = Vyhold

                    End If

                End If

            ElseIf TypeOf PP Is SRKPropertyPackage Then

                Dim fugvv, fugvl As Double()

                fugvv = PP.DW_CalcFugCoeff(Vy, T, P, State.Vapor)
                fugvl = PP.DW_CalcFugCoeff(Vy, T, P, State.Liquid)

                If fugvv.SubtractY(fugvl).AbsSqrSumY < 0.000001 Then

                    Dim phase = IdentifyPhase(Vy, P, T, PP, "SRK")

                    If phase = "L" Then

                        Dim Vhold, Vyhold() As Double
                        Vhold = V
                        V = L1
                        L1 = Vhold

                        Vyhold = Vy.Clone
                        Vy = Vx1.Clone
                        Vx1 = Vyhold

                    End If

                End If

            End If

            If Sx < 1.0 Then Sx = 0.0
            If L2 < 1.0 Then
                L2 = 0.0
            Else
                'check if liquid phases are the same
                Dim diffv As Double() = Vx2.Clone
                For i = 0 To n
                    diffv(i) = Math.Abs(Vx1(i) - Vx2(i))
                Next
                If diffv.SumY() < 0.01 * n Then
                    'the liquid phases are the same.
                    L1 = L1 + L2
                    L2 = 0.0
                End If
                'check if liquid2/vapor phases are the same
                Dim diffv2 As Double() = Vx2.Clone
                For i = 0 To n
                    diffv2(i) = Math.Abs(Vx2(i) - Vy(i))
                Next
                If diffv2.SumY() < 0.01 * n Then
                    'the liquid2/vapor phases are the same.
                    V = V + L2
                    L2 = 0.0
                End If
            End If

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
                result = New Object() {L1 / F, V / F, Vx1, Vy, ecount, L2 / F, Vx2, Sx / F, Vs, Ki1, Ki2}
            Else
                result = New Object() {L2 / F, V / F, Vx2, Vy, ecount, L1 / F, Vx1, Sx / F, Vs, Ki2, Ki1}
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
            If V < 0.0 Then V = -V

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

                Dim task1 As Task = TaskHelper.Run(Sub()
                                                       fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                                                   End Sub)
                Dim task2 As Task = TaskHelper.Run(Sub()
                                                       fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                                                   End Sub)
                Dim task3 As Task = TaskHelper.Run(Sub()
                                                       fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                                                   End Sub)
                Dim task4 As Task = TaskHelper.Run(Sub()
                                                       fcs = proppack.DW_CalcSolidFugCoeff(Tf, Pf)
                                                   End Sub)
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

            pval = MassBalanceResidual() ^ 2

            Gm = Gv + Gl1 + Gl2 + Gs + pval

            IObj?.Paragraphs.Add(String.Format("Calculated Gibbs Energy Value: {0}", Gm))

            WriteDebugInfo("[GM] V = " & Format(V, "N4") & ", L1 = " & Format(L1, "N4") & ", L2 = " & Format(L2, "N4") & ", S = " & Format(Sx, "N4") & " / GE = " & Format(Gm * 8.314 * Tf / 1000, "N2") & " kJ/kmol")

            ecount += 1

            Solutions.Add(x)
            GibbsEnergyValues.Add(Gm)

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
            If V < 0.0 Then V = -V

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

                Dim task1 As Task = TaskHelper.Run(Sub()
                                                       fcv = proppack.DW_CalcFugCoeff(Vy, Tf, Pf, State.Vapor)
                                                   End Sub)
                Dim task2 As Task = TaskHelper.Run(Sub()
                                                       fcl = proppack.DW_CalcFugCoeff(Vx1, Tf, Pf, State.Liquid)
                                                   End Sub)
                Dim task3 As Task = TaskHelper.Run(Sub()
                                                       fcl2 = proppack.DW_CalcFugCoeff(Vx2, Tf, Pf, State.Liquid)
                                                   End Sub)
                Dim task4 As Task = TaskHelper.Run(Sub()
                                                       fcs = proppack.DW_CalcSolidFugCoeff(Tf, Pf)
                                                   End Sub)
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

            Dim epsilon As Double = 0.1

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

                    Dim task1 As Task = TaskHelper.Run(Sub()
                                                           f2 = FunctionGradient(x2)
                                                       End Sub)
                    Dim task2 As Task = TaskHelper.Run(Sub()
                                                           f3 = FunctionGradient(x3)
                                                       End Sub)
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
            'For i = 0 To m - 1
            '    g(i) = -x(i) - x(i + m) - x(i + 2 * m) + fi(i) * F
            'Next
            Return True
        End Function

        Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(),
         ByRef jCol As Integer(), ByRef values As Double()) As Boolean

            'If values Is Nothing Then

            '    Dim row(nele_jac - 1), col(nele_jac - 1) As Integer

            '    For i = 0 To m - 1
            '        row(i) = i
            '        row(i + m) = i
            '        col(i) = i
            '        col(i + m) = i + m
            '    Next

            '    iRow = row
            '    jCol = col

            'Else

            '    Dim res(nele_jac - 1) As Double

            '    For i = 0 To nele_jac - 1
            '        res(i) = -1
            '    Next

            '    values = res

            'End If
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
            If alg_mod = IpoptAlgorithmMode.RegularMode And Math.Abs(objval - objval0) <= 0.0000000001 Then
                Return False
            Else
                Return True
            End If
            Return True
        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops With {.DisableParallelCalcs = True}
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT
            UsePreviousSolution = True
            Dim result As Object = Nothing
            Try
                result = nl.Flash_PH(Vz, P, H, Tref, PP, ReuseKI, PrevKi)
                UsePreviousSolution = False
                Return result
            Catch ex As Exception
                UsePreviousSolution = False
                Throw ex
            End Try

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops With {.DisableParallelCalcs = True}
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT
            UsePreviousSolution = True
            Dim result As Object = Nothing
            Try
                result = nl.Flash_PS(Vz, P, S, Tref, PP, ReuseKI, PrevKi)
                UsePreviousSolution = False
                Return result
            Catch ex As Exception
                UsePreviousSolution = False
                Throw ex
            End Try

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
