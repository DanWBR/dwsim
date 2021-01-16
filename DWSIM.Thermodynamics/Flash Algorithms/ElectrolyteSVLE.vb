'    Flash Algorithm for Electrolyte solutions
'    Copyright 2013-2016 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.Math
Imports System.Xml.Linq
Imports System.Linq

Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.Common
Imports Ciloci.Flee
Imports DWSIM.Interfaces.Enums
Imports Cureos.Numerics
Imports DotNumerics.Optimization


Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class ElectrolyteSVLE

        Inherits FlashAlgorithm

        Public Overrides ReadOnly Property InternalUseOnly As Boolean
            Get
                Return True
            End Get
        End Property

        Public proppack As PropertyPackage

        Dim nl3 As New NestedLoopsSLE With {.SolidSolution = False}

        Dim tmpx As Double(), tmpdx As Double()

        Dim N0 As New Dictionary(Of String, Double)
        Dim DN As New Dictionary(Of String, Double)
        Dim N As New Dictionary(Of String, Double)
        Dim Hf, Hl, Hv, Hs, T, P, P0, Ninerts, Winerts, E(,) As Double
        Dim r, c, els, comps As Integer

        Private IdealCalc As Boolean = False

        Public Property ReactionSet As String = "DefaultSet"
        Public Property Reactions As List(Of String)
        Public Property ReactionExtents As Dictionary(Of String, Double)
        Public Property ComponentIDs As List(Of String)
        Public Property CompoundProperties As List(Of Interfaces.ICompoundConstantProperties)
        Public Property ComponentConversions As Dictionary(Of String, Double)

        Public Property MaximumIterations As Integer = 100
        Public Property Tolerance As Double = 0.001

        Public Property ObjectiveFunctionHistory As New List(Of Double)

        Public Property CalculateChemicalEquilibria As Boolean = False

        Public Property OptimizeInitialEstimates As Boolean = True

        Public Property UseIPOPTSolver As Boolean = True

        Public Property AlternateBoundsInitializer As Boolean = False

        Public Property RigorousEnergyBalance As Boolean = True

        Private Vxl0, Vf0 As Double()

        Private LoopVarF, LoopVarX As Double, LoopVarVz As Double(), LoopVarState As State

        Public Overloads Function Flash_PT(Vx As Array, T As Double, P As Double) As Dictionary(Of String, Object)

            'This flash algorithm is for Electrolye/Salt systems with Water as the single solvent.
            'The vapor and solid phases are considered to be ideal.
            'Chemical equilibria is calculated using the reactions enabled in the default reaction set.

            'currently in testing/alpha stage.
            proppack.Flowsheet?.ShowMessage("Warning: the electrolyte flash algorithm is currently in alpha/testing stage. Results from electrolyte calculations should not be used in a production environment!", Interfaces.IFlowsheet.MessageType.Warning)

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
            Dim K(n), Vnf(n), Vnl(n), Vnl_ant(n), Vxl(n), Vns(n), Vxs(n), Vnv(n), Vxv(n), Vxv_ant(n), Vf(n), V, S, L, Vp(n) As Double
            Dim sumN As Double = 0

            'get water index in the array.

            Vnf = Vx.Clone

            Vf0 = Vx.Clone

            Dim wid As Integer = CompoundProperties.IndexOf((From c As Interfaces.ICompoundConstantProperties In CompoundProperties Select c Where c.Name = "Water").SingleOrDefault)

            Dim nl As New NestedLoops() With {.LimitVaporFraction = True}

            nl3.CompoundProperties = CompoundProperties

            Dim flashresult = nl.CalculateEquilibrium(FlashSpec.P, FlashSpec.T, P, T, proppack, Vx, Nothing, 0.0#)
            If flashresult.ResultException IsNot Nothing Then Throw flashresult.ResultException

            V = flashresult.GetVaporPhaseMoleFraction
            L = flashresult.GetLiquidPhase1MoleFraction
            S = 0.0#

            Vnv = flashresult.VaporPhaseMoleAmounts.ToArray
            Vnl = flashresult.LiquidPhase1MoleAmounts.ToArray

            Dim ids As New List(Of String)
            For i = 0 To n
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = proppack.AUX_PVAPi(i, T)
                If Double.IsNaN(Vp(i)) Then Vp(i) = 0.0#
            Next

            'get the default reaction set.

            Vf = Vx.Clone

            Dim int_count As Integer = 0
            Dim L_ant As Double = 0.0#
            Dim V_ant As Double = 0.0#
            Dim Lerr As Double = 0.0

            Dim rext As Double() = Nothing
            Dim result As Object

            sumN = 1.0#

            If L > 0.0# Then

                Do

                    'calculate chemical equilibria between ions, salts and water. 

                    For i = 0 To n
                        Vxl(i) = Vnl(i) / Vnl.Sum()
                    Next

                    Vxl0 = Vxl.Clone

                    ''SolveChemicalEquilibria' returns the equilibrium molar amounts in the liquid phase, including precipitates.

                    If CalculateChemicalEquilibria And Vnl(wid) > 0.0# And Vp(wid) < P Then

                        If int_count > 0 Then
                            For i = 0 To rext.Count - 1
                                rext(i) = 0.0
                            Next
                        End If

                        result = SolveChemicalEquilibria(Vnl, T, P, ids, rext)

                        Vnl = result(0).clone

                        rext = result(1)

                    End If

                    For i = 0 To n
                        Vxl(i) = Vnl(i) / Vnl.Sum()
                    Next

                    Vnf = Vnl.AddY(Vnv)

                    sumN = Vnf.Sum

                    Vf = Vnf.NormalizeY

                    L_ant = L

                    flashresult = nl.CalculateEquilibrium(FlashSpec.P, FlashSpec.T, P, T, proppack, Vf, Nothing, 0.0#)
                    If flashresult.ResultException IsNot Nothing Then Throw flashresult.ResultException

                    V = flashresult.GetVaporPhaseMoleFraction
                    L = flashresult.GetLiquidPhase1MoleFraction
                    S = 0.0#

                    Vxl = flashresult.GetLiquidPhase1MoleFractions
                    Vxv = flashresult.GetVaporPhaseMoleFractions

                    If L > 0 Then

                        If proppack.RET_VTF.SumY > 0.0 OrElse proppack.ForcedSolids.Count > 0 Then

                            result = nl3.Flash_SL(Vxl, P, T, proppack)

                            S = result(1) * L
                            L = result(0) * L

                            Vxl = result(3)
                            Vxs = result(4)

                        End If

                    End If

                    Vnv = Vxv.MultiplyConstY(V * sumN)
                    Vnl = Vxl.MultiplyConstY(L * sumN)
                    Vns = Vxl.MultiplyConstY(S * sumN)

                    Lerr = Abs(L - L_ant) ^ 2

                    If Lerr < 0.001 Then Exit Do

                    If int_count > MaximumIterations Then Throw New Exception("Chemical Equilibrium Solver error: Reached the maximum number of external iterations without converging.")

                    int_count += 1

                    proppack.CurrentMaterialStream.Flowsheet?.CheckStatus()

                Loop Until int_count > MaximumIterations

            End If

            'return flash calculation results.

            Dim results As New Dictionary(Of String, Object)

            results.Add("MixtureMoleFlows", Vnf)
            results.Add("VaporPhaseMoleFraction", V)
            results.Add("LiquidPhaseMoleFraction", L)
            results.Add("SolidPhaseMoleFraction", S)
            results.Add("VaporPhaseMolarComposition", Vxv)
            results.Add("LiquidPhaseMolarComposition", Vxl)
            results.Add("SolidPhaseMolarComposition", Vxs)
            results.Add("LiquidPhaseActivityCoefficients", activcoeff)
            results.Add("MoleSum", sumN)

            Return results

        End Function

        Private Function SolveChemicalEquilibria(ByVal Vnl As Array, ByVal T As Double, ByVal P As Double, ByVal ids As List(Of String), Optional ByVal prevx As Double() = Nothing) As Array

            Dim i, j As Integer

            'solves the chemical equilibria for the liquid phase.

            If Me.ReactionExtents Is Nothing Then Me.ReactionExtents = New Dictionary(Of String, Double)
            If Me.Reactions Is Nothing Then Me.Reactions = New List(Of String)
            If Me.ComponentConversions Is Nothing Then Me.ComponentConversions = New Dictionary(Of String, Double)
            If Me.ComponentIDs Is Nothing Then Me.ComponentIDs = New List(Of String)

            Me.Reactions.Clear()
            Me.ReactionExtents.Clear()

            Dim rx As Interfaces.IReaction

            P0 = 101325

            Dim rxn As Interfaces.IReaction

            'check active reactions (equilibrium only) in the reaction set
            For Each rxnsb As Interfaces.IReactionSetBase In proppack.CurrentMaterialStream.Flowsheet.ReactionSets(Me.ReactionSet).Reactions.Values
                If proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Equilibrium And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    Me.ReactionExtents.Add(rxnsb.ReactionID, 0)
                    rxn = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID)
                    'equilibrium constant calculation
                    Select Case rxn.KExprType
                        Case KOpt.Constant
                            'rxn.ConstantKeqValue = rxn.ConstantKeqValue
                        Case KOpt.Expression
                            Dim expr As New Ciloci.Flee.ExpressionContext
                            expr.Imports.AddType(GetType(System.Math))
                            expr.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                            expr.Variables.Add("T", T)
                            rxn.ConstantKeqValue = Exp(expr.CompileGeneric(Of Double)(rxn.Expression).Evaluate)
                        Case KOpt.Gibbs
                            Dim id(rxn.Components.Count - 1) As String
                            Dim stcoef(rxn.Components.Count - 1) As Double
                            Dim bcidx As Integer = 0
                            j = 0
                            For Each sb As Interfaces.IReactionStoichBase In rxn.Components.Values
                                id(j) = sb.CompName
                                stcoef(j) = sb.StoichCoeff
                                If sb.IsBaseReactant Then bcidx = j
                                j += 1
                            Next
                            Dim DelG_RT = proppack.AUX_DELGig_RT(298.15, T, id, stcoef, bcidx, True)
                            rxn.ConstantKeqValue = Exp(-DelG_RT)
                    End Select
                End If
            Next

            If Me.Reactions.Count > 0 Then

                Me.ComponentConversions.Clear()
                Me.ComponentIDs.Clear()

                'r: number of reactions
                'c: number of components
                'i,j: iterators

                i = 0
                For Each rxid As String In Me.Reactions
                    rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                    j = 0
                    For Each comp As Interfaces.IReactionStoichBase In rx.Components.Values
                        If Not Me.ComponentIDs.Contains(comp.CompName) Then
                            Me.ComponentIDs.Add(comp.CompName)
                            Me.ComponentConversions.Add(comp.CompName, 0)
                        End If
                        j += 1
                    Next
                    i += 1
                Next

                r = Me.Reactions.Count - 1
                c = Me.ComponentIDs.Count - 1

                ReDim E(c, r)

                'E: matrix of stoichometric coefficients
                i = 0
                For Each rxid As String In Me.Reactions
                    rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                    j = 0
                    For Each cname As String In Me.ComponentIDs
                        If rx.Components.ContainsKey(cname) Then
                            E(j, i) = rx.Components(cname).StoichCoeff
                        Else
                            E(j, i) = 0
                        End If
                        j += 1
                    Next
                    i += 1
                Next

                Dim fm0(c), N0tot As Double

                N0.Clear()
                DN.Clear()
                N.Clear()

                For Each cname As String In Me.ComponentIDs
                    N0.Add(cname, Vnl(ids.IndexOf(cname)))
                    DN.Add(cname, 0)
                    N.Add(cname, Vnl(ids.IndexOf(cname)))
                Next

                N0.Values.CopyTo(fm0, 0)

                N0tot = 1.0#
                Ninerts = N0tot - Sum(fm0)

                'upper and lower bounds for reaction extents (just an estimate, will be different when a single compound appears in multiple reactions)

                Dim lbound(Me.ReactionExtents.Count - 1) As Double
                Dim ubound(Me.ReactionExtents.Count - 1) As Double
                Dim var1 As Double

                If Not AlternateBoundsInitializer Then

                    i = 0
                    For Each rxid As String In Me.Reactions
                        rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                        j = 0
                        For Each comp As Interfaces.IReactionStoichBase In rx.Components.Values
                            var1 = -N0(comp.CompName) / comp.StoichCoeff
                            If j = 0 Then
                                lbound(i) = var1
                                ubound(i) = var1
                            Else
                                If var1 < lbound(i) Then lbound(i) = var1
                                If var1 > ubound(i) Then ubound(i) = var1
                            End If
                            j += 1
                        Next
                        i += 1
                    Next

                Else

                    Dim nvars As New List(Of Double)
                    Dim pvars As New List(Of Double)

                    i = 0
                    For Each rxid As String In Me.Reactions
                        nvars.Clear()
                        pvars.Clear()
                        rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                        For Each comp As Interfaces.IReactionStoichBase In rx.Components.Values
                            If comp.StoichCoeff < 0 Then pvars.Add(-N0(comp.CompName) / comp.StoichCoeff)
                            If comp.StoichCoeff > 0 Then nvars.Add(-N0(comp.CompName) / comp.StoichCoeff)
                        Next
                        lbound(i) = nvars.Max
                        ubound(i) = pvars.Min
                        i += 1
                    Next

                End If

                Me.T = T
                Me.P = P

                'initial estimates for the reaction extents

                Dim REx(r), iest(r), x(r), fx As Double

                If Not prevx Is Nothing Then
                    For i = 0 To r
                        REx(i) = prevx(i) * 0.99
                    Next
                Else
                    i = 0
                    For Each rxnsb As Interfaces.IReactionSetBase In proppack.CurrentMaterialStream.Flowsheet.ReactionSets(Me.ReactionSet).Reactions.Values
                        If proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Equilibrium And rxnsb.IsActive Then
                            rxn = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID)
                            REx(i) = 0.0 'lbound(i) + 0.5 * (ubound(i) - lbound(i))
                            i += 1
                        End If
                    Next
                End If

                'check if there is enough reactant to proceeed with chemical equilibrium calculation

                If ubound.SumY > 0.0# Then

                    If OptimizeInitialEstimates Then

                        'optimize initial estimates

                        If UseIPOPTSolver Then

                            Calculator.CheckParallelPInvoke()

                            IdealCalc = True

                            Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found

                            Using problem As New Ipopt(iest.Length, lbound, ubound, 0, Nothing, Nothing,
                           0, 0, AddressOf eval_f, AddressOf eval_g,
                           AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                                problem.AddOption("tol", Tolerance)
                                problem.AddOption("max_iter", MaximumIterations)
                                problem.AddOption("mu_strategy", "adaptive")
                                problem.AddOption("hessian_approximation", "limited-memory")
                                problem.SetIntermediateCallback(AddressOf intermediate)
                                status = problem.SolveProblem(REx, fx, Nothing, Nothing, Nothing, Nothing)
                            End Using

                            If status = IpoptReturnCode.Maximum_Iterations_Exceeded Then
                                Throw New Exception("Chemical Equilibrium Solver error: Reached the maximum number of internal iterations without converging.")
                            End If

                            iest = REx.Clone

                        Else

                            Dim extvars(r) As OptBoundVariable
                            For i = 0 To r
                                extvars(i) = New OptBoundVariable("ex" & CStr(i + 1), REx(i), False, lbound(i), ubound(i))
                            Next

                            Dim extsolver As New Simplex
                            extsolver.Tolerance = 0.001
                            extsolver.MaxFunEvaluations = 1000
                            iest = extsolver.ComputeMin(Function(xvar() As Double)

                                                            ObjectiveFunctionHistory.Clear()

                                                            IdealCalc = True

                                                            Dim intvars(r) As OptBoundVariable
                                                            For i = 0 To r
                                                                intvars(i) = New OptBoundVariable("x" & CStr(i + 1), xvar(i), False, lbound(i), ubound(i))
                                                            Next
                                                            Dim intsolver As New Simplex
                                                            intsolver.Tolerance = 0.01
                                                            intsolver.MaxFunEvaluations = 100
                                                            x = intsolver.ComputeMin(AddressOf FunctionValue2N, intvars)

                                                            intsolver = Nothing

                                                            fx = Me.FunctionValue2N(x)

                                                            Return fx

                                                        End Function, extvars)

                        End If

                    Else

                        For i = 0 To r
                            iest(i) = REx(i)
                        Next

                    End If

                    'use the best set of initial estimates for the reaction extents

                    For i = 0 To r
                        x(i) = iest(i)
                        If x(i) < lbound(i) Then x(i) = lbound(i) * 1.1
                        If x(i) > ubound(i) Then x(i) = ubound(i) * 0.9
                    Next

                    Dim variables(r) As OptBoundVariable
                    For i = 0 To r
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), x(i), False, lbound(i), ubound(i))
                    Next

                    ObjectiveFunctionHistory.Clear()

                    IdealCalc = False

                    If UseIPOPTSolver Then

                        Calculator.CheckParallelPInvoke()

                        Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found

                        Using problem As New Ipopt(iest.Length, lbound, ubound, 0, Nothing, Nothing,
                           0, 0, AddressOf eval_f, AddressOf eval_g,
                           AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                            problem.AddOption("tol", Tolerance)
                            problem.AddOption("max_iter", MaximumIterations)
                            problem.AddOption("mu_strategy", "adaptive")
                            problem.AddOption("hessian_approximation", "limited-memory")
                            problem.SetIntermediateCallback(AddressOf intermediate)
                            status = problem.SolveProblem(iest, fx, Nothing, Nothing, Nothing, Nothing)
                        End Using

                        If status = IpoptReturnCode.Maximum_Iterations_Exceeded Then
                            Throw New Exception("Chemical Equilibrium Solver error: Reached the maximum number of internal iterations without converging.")
                        End If

                        x = iest.Clone

                    Else

                        Dim solver As New Simplex
                        solver.Tolerance = Tolerance
                        solver.MaxFunEvaluations = MaximumIterations
                        x = solver.ComputeMin(AddressOf FunctionValue2N, variables)

                        If solver.FunEvaluations >= solver.MaxFunEvaluations Then
                            Throw New Exception("Chemical Equilibrium Solver error: Reached the maximum number of internal iterations without converging.")
                        End If

                    End If

                    ' check objective function value
                    ' an acceptable solution should have a function value less than one at least because the 
                    ' simplex solver doesn't always obey the tolerance value

                    fx = Me.FunctionValue2N(x)

                    If Double.IsNaN(fx) Then
                        Throw New Exception("Chemical Equilibrium Solver error: general numerical error.")
                    End If

                End If

                'compound conversions

                For Each sb As String In ids
                    If Me.ComponentConversions.ContainsKey(sb) Then
                        Me.ComponentConversions(sb) = -DN(sb) / N0(sb)
                    End If
                Next

                ' return equilibrium molar amounts in the liquid phase.

                Dim Vnl2 As Double() = Vnl.Clone

                For Each s As String In N.Keys
                    Vnl2(ids.IndexOf(s)) = Abs(N(s))
                Next

                Dim nc As Integer = Vnl.Length - 1

                Dim mtot As Double = 0
                For i = 0 To nc
                    mtot += Vnl2(i)
                Next

                'For i = 0 To nc
                '    Vx(i) = Vx(i) '/ mtot
                'Next

                Return New Object() {Vnl2, x}

            Else

                Return New Object() {Vnl, Nothing}

            End If

        End Function

        'IPOPT

        Public Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean
            Dim fval As Double = FunctionValue2N(x)
            obj_value = fval
            Return True
        End Function

        Public Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean
            Dim g As Double() = FunctionGradient(x)
            grad_f = g
            Return True
        End Function

        Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
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

        Public Function intermediate(ByVal alg_mod As IpoptAlgorithmMode, ByVal iter_count As Integer, ByVal obj_value As Double,
                         ByVal inf_pr As Double, ByVal inf_du As Double, ByVal mu As Double,
                         ByVal d_norm As Double, ByVal regularization_size As Double, ByVal alpha_du As Double,
                         ByVal alpha_pr As Double, ByVal ls_trials As Integer) As Boolean

            proppack.CurrentMaterialStream.Flowsheet?.ShowMessage("Chemical Equilibrium Error = " & obj_value & " (Acceptable Value = " & Tolerance & ")", Interfaces.IFlowsheet.MessageType.Information)

            If obj_value < Tolerance Then Return False Else Return True

        End Function

        Private Function FunctionValue2N(ByVal x() As Double) As Double

            Dim i, j, nc, wid As Integer

            nc = Me.CompoundProperties.Count - 1

            i = 0
            For Each s As String In N.Keys
                DN(s) = 0
                For j = 0 To r
                    DN(s) += E(i, j) * x(j)
                Next
                i += 1
            Next

            For Each s As String In DN.Keys
                N(s) = N0(s) + DN(s)
                'If N(s) < 0.0# Then N(s) = -N(s)
            Next

            Dim Vxl(nc) As Double

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0.0#
            Dim mtotal As Double = 0.0#
            Dim molality(nc) As Double

            For i = 0 To nc
                Vxl(i) = Vxl0(i)
            Next

            For i = 0 To N.Count - 1
                For j = 0 To nc
                    If CompoundProperties(j).Name = ComponentIDs(i) Then
                        Vxl(j) = N(ComponentIDs(i)) / N.Values.Sum
                        If Vxl(i) < 0 Then Vxl(i) = Abs(Vxl(i))
                        Exit For
                    End If
                Next
            Next

            Dim val1, val2 As Double
            Dim pen_val As Double = 0.0

            i = 0
            Do
                If CompoundProperties(i).Name = "Water" Then
                    wid = i
                    wtotal += Vxl(i) * CompoundProperties(i).Molar_Weight / 1000
                End If
                mtotal += Vxl(i)
                i += 1
            Loop Until i = nc + 1

            val1 = N0.Values.ToArray.SumY * proppack.AUX_MMM(Vxl0)
            val2 = N.Values.ToArray.SumY * proppack.AUX_MMM(Vxl)

            pen_val += 100000 * (val1 - val2) ^ 2

            For Each s As String In N.Keys
                If N(s) < 0.0# Then pen_val += (N(s) * 100) ^ 2
            Next

            Dim Xsolv As Double = 1

            'solvent density without solids and ions

            Dim Vxns(nc) As Double

            For i = 0 To nc
                If Not CompoundProperties(i).IsSalt And Not CompoundProperties(i).IsIon Then
                    Vxns(i) = Vxl(i)
                End If
            Next

            'Vxns = Vxns.NormalizeY

            Dim wden As Double = 0.0#
            If TypeOf proppack Is ExUNIQUACPropertyPackage Then
                wden = CType(proppack, ExUNIQUACPropertyPackage).m_elec.LiquidDensity(Vxns, T, CompoundProperties)
            ElseIf TypeOf proppack Is ElectrolyteNRTLPropertyPackage Then
                wden = CType(proppack, ElectrolyteNRTLPropertyPackage).m_elec.LiquidDensity(Vxns, T, CompoundProperties)
            End If

            i = 0
            Do
                molality(i) = Vxl(i) / wtotal * wden / 1000
                i += 1
            Loop Until i = nc + 1

            Dim activcoeff(nc) As Double

            If IdealCalc Then
                For i = 0 To nc
                    activcoeff(i) = 1.0#
                Next
            Else
                If TypeOf proppack Is ExUNIQUACPropertyPackage Then
                    activcoeff = CType(proppack, ExUNIQUACPropertyPackage).m_uni.GAMMA_MR(T, Vxl.Clone, CompoundProperties)
                ElseIf TypeOf proppack Is ElectrolyteNRTLPropertyPackage Then
                    activcoeff = CType(proppack, ElectrolyteNRTLPropertyPackage).m_enrtl.GAMMA_MR(T, Vxl.Clone, CompoundProperties)
                End If
            End If

            Dim CP(nc) As Double
            Dim f1, f2 As Double
            Dim prod(Me.Reactions.Count - 1) As Double

            For i = 0 To nc
                If CompoundProperties(i).IsIon Then
                    'CP(i) = molality(i) * activcoeff(i)
                    CP(i) = Vxl(i) * activcoeff(i)
                ElseIf CompoundProperties(i).IsSalt Then
                    CP(i) = 1.0#
                Else
                    CP(i) = Vxl(i) * activcoeff(i)
                End If
            Next

            For i = 0 To Me.Reactions.Count - 1
                prod(i) = 1
                For Each s As String In Me.ComponentIDs
                    With proppack.CurrentMaterialStream.Flowsheet.Reactions(Me.Reactions(i))
                        If .Components.ContainsKey(s) Then
                            For j = 0 To nc
                                If CompoundProperties(j).Name = s Then
                                    If CP(j) <> 0.0 Then
                                        prod(i) *= CP(j) ^ .Components(s).StoichCoeff
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                    End With
                Next
            Next

            Dim fval As Double = 0

            For i = 0 To Me.Reactions.Count - 1
                With proppack.CurrentMaterialStream.Flowsheet.Reactions(Me.Reactions(i))
                    f1 += Log(.ConstantKeqValue)
                    f2 += Log(prod(i))
                    fval += (f1 - f2) ^ 2
                End With
            Next

            proppack.CurrentMaterialStream.Flowsheet?.CheckStatus()

            If Double.IsNaN(f2) Or Double.IsInfinity(f2) Then

                fval = pen_val

                ObjectiveFunctionHistory.Add(fval)

                Return fval

            Else

                fval += pen_val

                ObjectiveFunctionHistory.Add(fval)

                Return fval

            End If

        End Function

        Public Function FunctionGradient(ByVal x() As Double) As Double()

            Dim epsilon As Double = 0.1

            Dim f1, f2 As Double
            Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
            Dim j, k As Integer

            For j = 0 To x.Length - 1
                For k = 0 To x.Length - 1
                    x1(k) = x(k)
                    x2(k) = x(k)
                Next
                If x(j) <> 0.0# Then
                    x1(j) = x(j) * (1.0# + epsilon)
                    x2(j) = x(j) * (1.0# - epsilon)
                Else
                    x1(j) = x(j) + epsilon
                    x2(j) = x(j) - epsilon
                End If
                f1 = FunctionValue2N(x1)
                f2 = FunctionValue2N(x2)
                g(j) = (f2 - f1) / (x2(j) - x1(j))
            Next

            Return g

        End Function

        Public Overloads Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double) As Dictionary(Of String, Object)

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, Pf As Double

            d1 = Date.Now

            n = Vz.Length - 1

            Vf0 = Vz.Clone

            Hf = H
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n)

            If Tref = 0.0# Then Tref = 298.15
            If Double.IsNaN(Tref) Then Tref = 298.15

            Me.P = P

            Dim cnt As Integer = 0

            Dim fx, fx2, dfdx, x1, x0, dx As Double

            x1 = Tref

            Dim prevset = CalculateChemicalEquilibria

            CalculateChemicalEquilibria = RigorousEnergyBalance

            Do

                If cnt < 2 Then

                    fx = Herror({x1})
                    fx2 = Herror({x1 + 0.1})

                    dfdx = (fx2 - fx) / 0.1

                Else

                    fx2 = fx
                    fx = Herror({x1})

                    dfdx = (fx - fx2) / (x1 - x0)

                End If

                If Abs(fx) <= 0.01 Then Exit Do

                dx = fx / dfdx

                x0 = x1
                x1 = x1 - dx

                If Double.IsNaN(x1) Or cnt > 25 Then
                    Throw New Exception("PH Flash [Electrolyte]: Invalid result: Temperature did not converge.")
                End If

                cnt += 1

            Loop

            CalculateChemicalEquilibria = prevset

            T = x1

            Dim tmp As Object = Flash_PT(Vz, T, P)

            Dim S, Vs(), Vnf(), sumN As Double

            sumN = tmp("MoleSum")
            L = tmp("LiquidPhaseMoleFraction")
            V = tmp("VaporPhaseMoleFraction")
            S = tmp("SolidPhaseMoleFraction")
            Vx = tmp("LiquidPhaseMolarComposition")
            Vy = tmp("VaporPhaseMolarComposition")
            Vs = tmp("SolidPhaseMolarComposition")
            Vnf = tmp("MixtureMoleFlows")

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [Electrolyte]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            'return flash calculation results.

            Dim results As New Dictionary(Of String, Object)

            results.Add("MixtureMoleFlows", Vnf)
            results.Add("VaporPhaseMoleFraction", V)
            results.Add("LiquidPhaseMoleFraction", L)
            results.Add("SolidPhaseMoleFraction", S)
            results.Add("VaporPhaseMolarComposition", Vy)
            results.Add("LiquidPhaseMolarComposition", Vx)
            results.Add("SolidPhaseMolarComposition", Vs)
            results.Add("MoleSum", sumN)
            results.Add("Temperature", T)
            results.Add("LiquidPhaseActivityCoefficients", tmp("LiquidPhaseActivityCoefficients"))

            Return results

        End Function

        Function Herror(ByVal x() As Double) As Double
            Return OBJ_FUNC_PH_FLASH(x(0), P, Vf0.Clone)
        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal P As Double, ByVal Vz As Object) As Double

            Dim tmp As Dictionary(Of String, Object) = Flash_PT(Vz, T, P)

            Dim FW0, FW, L, V, S, Vx(), Vy(), Vs(), sumN, _Hv, _Hl, _Hs As Double

            Dim n = Vz.Length - 1

            sumN = tmp("MoleSum")
            L = tmp("LiquidPhaseMoleFraction")
            V = tmp("VaporPhaseMoleFraction")
            S = tmp("SolidPhaseMoleFraction")
            Vx = tmp("LiquidPhaseMolarComposition")
            Vy = tmp("VaporPhaseMolarComposition")
            Vs = tmp("SolidPhaseMolarComposition")

            _Hv = 0
            _Hl = 0
            _Hs = 0

            Dim mmm, mmg, mml, mms As Double

            If V > 0.0# Then _Hv = proppack.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L > 0.0# Then _Hl = proppack.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
            If S > 0.0# Then _Hs = proppack.DW_CalcSolidEnthalpy(T, Vs, CompoundProperties)

            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx)
            mms = proppack.AUX_MMM(Vs)

            mmm = V * mmg + L * mml + S * mml

            FW0 = 0.001 * proppack.AUX_MMM(Vz) 'kg
            FW = 0.001 * sumN * mmm 'kg

            Dim herr As Double = FW0 * Hf - FW * (((mmg * V / (mmg * V + mml * L + mms * S)) * _Hv + (mml * L / (mmg * V + mml * L + mms * S)) * _Hl + (mms * S / (mmg * V + mml * L + mms * S)) * _Hs))

            Return herr

            WriteDebugInfo("PH Flash [Electrolyte]: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Private Function EnthalpyTx(ByVal x As Double, ByVal otherargs As Object) As Double

            Dim er As Double = LoopVarF - proppack.DW_CalcEnthalpy(LoopVarVz, x, LoopVarX, LoopVarState)
            Return er

        End Function

        Public Overloads Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double) As Object

            Dim n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            Dim maxitINT As Integer = 100
            Dim maxitEXT As Integer = 100
            Dim tolINT As Double = 0.00001
            Dim tolEXT As Double = 0.00001

            n = Vz.Length - 1

            Dim Vx(n), Vy(n), Vp(n), Vcalc, Vspec, P, x, x0, x00, fx, fx0, fx00, Pmin, Pmax As Double

            Dim nl As New NestedLoops
            Dim flashresult = nl.CalculateEquilibrium(FlashSpec.T, FlashSpec.VAP, T, 0.0#, proppack, Vz, Nothing, Pref)
            Pmax = flashresult.CalculatedPressure
            flashresult = nl.CalculateEquilibrium(FlashSpec.T, FlashSpec.VAP, T, 1.0#, proppack, Vz, Nothing, Pref)
            Pmin = flashresult.CalculatedPressure

            P = Pmin + (1 - V) * (Pmax - Pmin)

            ecount = 0
            Vspec = V
            x = P

            Dim tmp As Dictionary(Of String, Object)

            Do

                tmp = Flash_PT(Vz, T, x)

                Vcalc = tmp("VaporPhaseMoleFraction")

                fx00 = fx0
                fx0 = fx

                fx = Vspec - Vcalc

                If Abs(fx) < tolEXT Then Exit Do

                x00 = x0
                x0 = x

                If ecount <= 1 Then
                    x *= 0.99
                Else
                    x = x - fx * (x - x00) / (fx - fx00)
                    If Double.IsNaN(x) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))
                End If

                ecount += 1

                If ecount > maxitEXT Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

            Loop

            P = x

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("TV Flash [Sour Water]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Dim results As New Dictionary(Of String, Object)

            results.Add("MixtureMoleFlows", tmp("MixtureMoleFlows"))
            results.Add("VaporPhaseMoleFraction", tmp("VaporPhaseMoleFraction"))
            results.Add("LiquidPhaseMoleFraction", tmp("LiquidPhaseMoleFraction"))
            results.Add("SolidPhaseMoleFraction", tmp("SolidPhaseMoleFraction"))
            results.Add("VaporPhaseMolarComposition", tmp("VaporPhaseMolarComposition"))
            results.Add("LiquidPhaseMolarComposition", tmp("LiquidPhaseMolarComposition"))
            results.Add("SolidPhaseMolarComposition", tmp("SolidPhaseMolarComposition"))
            results.Add("MoleSum", tmp("MoleSum"))
            results.Add("Pressure", P)
            results.Add("LiquidPhaseActivityCoefficients", tmp("LiquidPhaseActivityCoefficients"))

            Return results

        End Function

        Public Overloads Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double) As Object

            Dim n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            Dim maxitINT As Integer = 100
            Dim maxitEXT As Integer = 100
            Dim tolINT As Double = 0.00001
            Dim tolEXT As Double = 0.00001

            n = Vz.Length - 1

            Dim Vx(n), Vy(n), Vp(n), gamma(n), Vcalc, Vspec, T, x, x0, x00, fx, fx0, fx00, Pcalc As Double

            Dim nl As New NestedLoops

            ecount = 0
            Vspec = V
            x = Tref

            Dim tmp As Dictionary(Of String, Object)

            If Vspec = 0.0# Then

                'bubble point

                Do

                    Vp = proppack.RET_VPVAP(x)

                    tmp = Flash_PT(Vz, x, P)

                    gamma = tmp("LiquidPhaseActivityCoefficients")

                    fx00 = fx0
                    fx0 = fx

                    Pcalc = 0.0#
                    For i = 0 To n
                        If Not CompoundProperties(i).IsIon And Not CompoundProperties(i).IsSalt Then
                            Pcalc += gamma(i) * Vp(i)
                        End If
                    Next

                    fx = P - Pcalc

                    If Abs(fx) < 1.0# Then Exit Do

                    x00 = x0
                    x0 = x

                    If ecount <= 1 Then
                        x += 1.0#
                    Else
                        If ecount < 10 Then
                            x = x - 0.1 * fx * (x - x00) / (fx - fx00)
                        Else
                            x = x - fx * (x - x00) / (fx - fx00)
                        End If
                        If Double.IsNaN(x) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))
                    End If

                    ecount += 1

                    If ecount > 1000 Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

                Loop

            Else

                Do

                    tmp = Flash_PT(Vz, x, P)

                    Vcalc = tmp("VaporPhaseMoleFraction")

                    fx00 = fx0
                    fx0 = fx

                    fx = Vspec - Vcalc

                    If Abs(fx) < tolEXT Then Exit Do

                    x00 = x0
                    x0 = x

                    If ecount <= 1 Then
                        x += 1.0#
                    Else
                        x = x - 0.3 * fx * (x - x00) / (fx - fx00)
                        If Double.IsNaN(x) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))
                    End If

                    ecount += 1

                    If ecount > maxitEXT Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

                Loop

            End If

            T = x

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PV Flash [Sour Water]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Dim results As New Dictionary(Of String, Object)

            results.Add("MixtureMoleFlows", tmp("MixtureMoleFlows"))
            results.Add("VaporPhaseMoleFraction", tmp("VaporPhaseMoleFraction"))
            results.Add("LiquidPhaseMoleFraction", tmp("LiquidPhaseMoleFraction"))
            results.Add("SolidPhaseMoleFraction", tmp("SolidPhaseMoleFraction"))
            results.Add("VaporPhaseMolarComposition", tmp("VaporPhaseMolarComposition"))
            results.Add("LiquidPhaseMolarComposition", tmp("LiquidPhaseMolarComposition"))
            results.Add("SolidPhaseMolarComposition", tmp("SolidPhaseMolarComposition"))
            results.Add("MoleSum", tmp("MoleSum"))
            results.Add("Temperature", T)
            results.Add("LiquidPhaseActivityCoefficients", tmp("LiquidPhaseActivityCoefficients"))

            Return results

        End Function

        Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(),
         ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean

            If values Is Nothing Then

                Dim row(nele_hess - 1), col(nele_hess - 1) As Integer

                iRow = row
                jCol = col

            Else

            End If

            Return True

        End Function

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Electrolyte
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "Electrolyte Flash"
            End Get
        End Property

        Public Overrides Function Flash_PH(Vz() As Double, P As Double, H As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            proppack = PP
            Dim results = Flash_PH(Vz, P, H, Tref)
            'Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            With results
                Return New Object() {results("LiquidPhaseMoleFraction"), results("VaporPhaseMoleFraction"), results("LiquidPhaseMolarComposition"),
                                     results("VaporPhaseMolarComposition"), results("Temperature"), 1, PP.RET_NullVector, 0.0#, PP.RET_NullVector, results("SolidPhaseMoleFraction"), results("SolidPhaseMolarComposition")}
            End With
        End Function

        Public Overrides Function Flash_PS(Vz() As Double, P As Double, S As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            Throw New NotImplementedException("Pressure-Entropy Flash Not Implemented")
        End Function

        Public Overrides Function Flash_PT(Vz() As Double, P As Double, T As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            proppack = PP
            Dim results = Flash_PT(Vz, T, P)
            With results
                Return New Object() {results("LiquidPhaseMoleFraction"), results("VaporPhaseMoleFraction"), results("LiquidPhaseMolarComposition"),
                                     results("VaporPhaseMolarComposition"), 1, 0.0#, PP.RET_NullVector, results("SolidPhaseMoleFraction"), results("SolidPhaseMolarComposition")}
            End With
        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, V As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            proppack = PP
            Dim results = Flash_PV(Vz, P, V, Tref)
            With results
                Return New Object() {results("LiquidPhaseMoleFraction"), results("VaporPhaseMoleFraction"), results("LiquidPhaseMolarComposition"),
                                     results("VaporPhaseMolarComposition"), results("Temperature"), 1, PP.RET_NullVector, 0.0#, PP.RET_NullVector, results("SolidPhaseMoleFraction"), results("SolidPhaseMolarComposition")}
            End With
        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            proppack = PP
            Dim results = Flash_TV(Vz, T, V, Pref)
            With results
                Return New Object() {results("LiquidPhaseMoleFraction"), results("VaporPhaseMoleFraction"), results("LiquidPhaseMolarComposition"),
                                     results("VaporPhaseMolarComposition"), results("Pressure"), 1, PP.RET_NullVector, 0.0#, PP.RET_NullVector, results("SolidPhaseMoleFraction"), results("SolidPhaseMolarComposition")}
            End With
        End Function

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Electrolyte SVLE"
            End Get
        End Property

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace
