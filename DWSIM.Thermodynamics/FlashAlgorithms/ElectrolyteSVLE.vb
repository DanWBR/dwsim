'    Flash Algorithm for Electrolyte solutions
'    Copyright 2013-2024 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Interfaces.Enums
Imports DotNumerics.Optimization
Imports DotNumerics.Scaling
Imports DWSIM.Interfaces

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class ElectrolyteSVLE

        Inherits FlashAlgorithm

        Public Shared ExternalSolver As IExternalNonLinearSystemSolver

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

        Public Property ReactionSet As String = "DefaultSet"
        Public Property Reactions As List(Of String)
        Public Property ReactionExtents As Dictionary(Of String, Double)
        Public Property ComponentIDs As List(Of String)
        Public Property CompoundProperties As List(Of Interfaces.ICompoundConstantProperties)
        Public Property ComponentConversions As Dictionary(Of String, Double)

        Public Property MaximumIterations As Integer = 100
        Public Property Tolerance As Double = 0.001

        Private PenaltyValueScheme As Integer = 0

        Private Pval As Double = 0.0

        Private Vxl0, Vf0 As Double()

        Private MinVal, MaxVal As Double

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

                    If Vnl(wid) > 0.0# And Vp(wid) < P Then

                        If int_count = 0 Then
                            result = SolveChemicalEquilibria(True, Vnl, T, P, ids)
                        Else
                            result = SolveChemicalEquilibria(False, Vnl, T, P, ids)
                        End If

                        Vnl = result.clone

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

            activcoeff = proppack.RET_UnitaryVector()

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

        Private Function SolveChemicalEquilibria(ByVal initialize As Boolean, ByVal Vnl As Double(), ByVal T As Double, ByVal P As Double, ByVal ids As List(Of String)) As Array

            Dim i, j As Integer

            'solves the chemical equilibria for the liquid phase.

            If Me.ReactionExtents Is Nothing Then Me.ReactionExtents = New Dictionary(Of String, Double)
            If Me.Reactions Is Nothing Then Me.Reactions = New List(Of String)
            If Me.ComponentConversions Is Nothing Then Me.ComponentConversions = New Dictionary(Of String, Double)
            If Me.ComponentIDs Is Nothing Then Me.ComponentIDs = New List(Of String)

            Me.Reactions.Clear()
            'Me.ReactionExtents.Clear()

            Dim rx As Interfaces.IReaction

            P0 = 101325

            Dim rxn As Interfaces.IReaction

            'check active reactions (equilibrium only) in the reaction set
            For Each rxnsb As Interfaces.IReactionSetBase In proppack.CurrentMaterialStream.Flowsheet.ReactionSets(Me.ReactionSet).Reactions.Values
                If proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Equilibrium And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    If Not ReactionExtents.ContainsKey(rxnsb.ReactionID) Then
                        Me.ReactionExtents.Add(rxnsb.ReactionID, 0)
                    End If
                    rxn = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID)
                    'equilibrium constant calculation
                    rxn.EvaluateK(T + rxn.Approach, proppack)
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
                            E(j, i) = 0.0
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

                Me.T = T
                Me.P = P

                'check if there is enough reactant to proceeed with chemical equilibrium calculation

                If N.Values.Sum > 0.0# Then


                    Dim lbound(Me.ReactionExtents.Count - 1) As Double
                    Dim ubound(Me.ReactionExtents.Count - 1) As Double

                    Dim bounds As New List(Of Double)

                    i = 0
                    For Each rxid As String In Me.Reactions
                        rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                        For Each comp As BaseClasses.ReactionStoichBase In rx.Components.Values
                            bounds.Add(Math.Abs(N0(comp.CompName) / comp.StoichCoeff))
                        Next
                        i += 1
                    Next

                    i = 0
                    For Each rxid As String In Me.Reactions
                        rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                        If rx.Components.ContainsKey("Water") AndAlso rx.Components("Water").StoichCoeff = -1 Then
                            lbound(i) = 0.00000000000001
                            ubound(i) = 0.1
                        Else
                            lbound(i) = -bounds.Max
                            ubound(i) = bounds.Max
                        End If
                        i += 1
                    Next

                    Dim m As Integer = 0

                    Dim REx(r), REx0(r) As Double

                    If ReactionExtents.Count > 0 Then
                        REx = ReactionExtents.Values.ToArray()
                        REx0 = ReactionExtents.Values.ToArray()
                    End If

                    'estimate initial extents
                    i = 0
                    For Each rxid As String In Me.Reactions
                        If REx(i) = 0.0 Then
                            rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                            If rx.Components.ContainsKey("Water") AndAlso rx.Components("Water").StoichCoeff = -1 Then
                                REx(i) = 0.0000001
                            Else
                                Dim keq = rx.EvaluateK(T + rx.Approach, proppack)
                                If keq > 1000000 Then
                                    REx(i) = -N0(rx.BaseReactant) / rx.Components(rx.BaseReactant).StoichCoeff
                                ElseIf keq < 0.00001 Then
                                    REx(i) = -N0(rx.BaseReactant) / rx.Components(rx.BaseReactant).StoichCoeff
                                Else
                                    REx(i) = -N0(rx.BaseReactant) / rx.Components(rx.BaseReactant).StoichCoeff / (keq ^ 0.05)
                                End If
                                If REx(i) < lbound(i) Then REx(i) = lbound(i)
                                If REx(i) > ubound(i) Then REx(i) = ubound(i)
                            End If
                        End If
                        i += 1
                    Next

                    Dim W0 = Vnl.Sum * proppack.AUX_MMM(Vnl) / 1000

                    MinVal = Math.Min(lbound.Min, REx.Min)
                    MaxVal = Math.Max(ubound.Max, REx.Max)

                    'solve using newton's method

                    Dim x(r), newx(r) As Double

                    Dim errval, sumerr As Double

                    x = REx

                    Dim xs As Double

                    Dim variables As New List(Of Double)
                    Dim lbounds As New List(Of Double)
                    Dim ubounds As New List(Of Double)
                    For i = 0 To r
                        xs = Scaler.Scale(x(i), MinVal, MaxVal, 0.0, 1.0)
                        variables.Add(xs)
                        lbounds.Add(0.0)
                        ubounds.Add(1.0)
                    Next

                    newx = variables.ToArray()

                    Dim solver1 As New Optimization.NewtonSolver With
                    {
                        .MaxIterations = MaximumIterations,
                        .Tolerance = Tolerance,
                        .EnableDamping = True,
                        .Epsilon = 0.00000000000001
                    }

                    Dim feq = Sub(newx2)

                                  solver1.Reset()

                                  errval = 0.0
                                  sumerr = 0.0

                                  Dim fval As Double()

                                  If ExternalSolver Is Nothing Then

                                      newx = solver1.Solve(Function(x1)
                                                               fval = FunctionValue2N(x1)
                                                               Return fval
                                                           End Function, newx)

                                  Else

                                      newx = ExternalSolver.Solve(Function(x1)
                                                                      fval = FunctionValue2N(x1)
                                                                      Return fval
                                                                  End Function, Nothing, Nothing,
                                                                            newx, MaximumIterations, Tolerance)

                                  End If

                                  errval = FunctionValue2N(newx).AbsSqrSumY()

                                  If Math.Abs(errval) > Tolerance Or Math.Abs(Pval) > 0.01 Then
                                      Throw New Exception("Error calculating equilibrium composition.")
                                  End If

                                  newx = newx.Select(Function(xi) Scaler.UnScale(Math.Exp(xi), MinVal, MaxVal, 0.0, 1.0)).ToArray

                                  sumerr = x.SubtractY(newx).AbsSqrSumY

                                  x = newx

                              End Sub

                    Dim HasError As Boolean = True
                    Try
                        PenaltyValueScheme = 0
                        feq.Invoke(variables.ToArray())
                        HasError = False
                    Catch ex As Exception
                        HasError = True
                    End Try
                    If HasError Then
                        Try
                            PenaltyValueScheme = 1
                            feq.Invoke(variables.ToArray())
                            HasError = False
                        Catch ex As Exception
                            HasError = True
                        End Try
                        If HasError Then
                            Try
                                PenaltyValueScheme = 2
                                feq.Invoke(variables.ToArray())
                                HasError = False
                            Catch ex As Exception
                                HasError = True
                            End Try
                            If HasError Then
                                PenaltyValueScheme = 3
                                feq.Invoke(variables.ToArray())
                            End If
                        End If
                    End If

                    REx = x

                    i = 0
                    For Each r As String In Me.Reactions
                        ReactionExtents(r) = REx(i)
                        i += 1
                    Next

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
                    If Vnl2(i) < 1.0E-50 Then
                        Vnl2(i) = 0.0
                    End If
                    mtot += Vnl2(i)
                Next

                Return Vnl2

            Else

                Return Vnl

            End If

        End Function

        Private Function FunctionValue2N(x() As Double) As Double()

            If Double.IsNaN(x.Sum) Then Throw New Exception("Convergence Error")

            Dim nc = Me.CompoundProperties.Count - 1

            Dim i, j As Integer

            Dim unscaled_extents(r) As Double

            For i = 0 To r
                Dim val0 = x(i) 'Math.Pow(Math.E, x(i))
                unscaled_extents(i) = Scaler.UnScale(val0, MinVal, MaxVal, 0.0, 1.0)
            Next

            i = 0
            For Each s As String In N.Keys
                DN(s) = 0
                For j = 0 To r
                    DN(s) += E(i, j) * unscaled_extents(j)
                Next
                i += 1
            Next

            For Each s As String In DN.Keys
                N(s) = N0(s) + DN(s)
            Next

            Dim Vxl(nc) As Double

            'calculate molality considering 1 mol of mixture.

            Dim mtotal As Double = 0.0#
            Dim molality(nc) As Double

            For i = 0 To nc
                Vxl(i) = Vxl0(i)
            Next

            For i = 0 To N.Count - 1
                For j = 0 To nc
                    If CompoundProperties(j).Name = ComponentIDs(i) Then
                        Vxl(j) = N(ComponentIDs(i)) / (N.Values.Sum + Ninerts)
                        Exit For
                    End If
                Next
            Next

            Dim wtotal As Double = N.Values.Sum * proppack.AUX_MMM(Vxl) / 1000

            Dim Xsolv As Double = 1

            'solvent density without solids and ions

            Dim Vxns(nc) As Double

            For i = 0 To nc
                If Not CompoundProperties(i).IsSalt And Not CompoundProperties(i).IsIon Then
                    Vxns(i) = Vxl(i)
                End If
            Next

            Dim wden = CType(proppack, ElectrolyteBasePropertyPackage).m_elec.LiquidDensity(Vxns, T, CompoundProperties)

            i = 0
            Do
                molality(i) = Vxl(i) / wtotal * wden / 1000
                i += 1
            Loop Until i = nc + 1

            Dim activcoeff(nc) As Double

            activcoeff = proppack.RET_UnitaryVector()

            Dim CP(nc) As Double
            Dim prod(Me.Reactions.Count - 1) As Double

            For i = 0 To nc
                If CompoundProperties(i).IsIon Then
                    CP(i) = molality(i) * activcoeff(i)
                ElseIf CompoundProperties(i).IsSalt Then
                    CP(i) = molality(i) * activcoeff(i)
                Else
                    CP(i) = Vxl(i) * activcoeff(i)
                End If
            Next

            For i = 0 To Me.Reactions.Count - 1
                prod(i) = 1.0
                For Each s As String In Me.ComponentIDs
                    With proppack.CurrentMaterialStream.Flowsheet.Reactions(Me.Reactions(i))
                        If .Components.ContainsKey(s) Then
                            For j = 0 To nc
                                If CompoundProperties(j).Name = s Then
                                    If CP(j) > 1.0E-45 Then
                                        prod(i) *= CP(j) ^ .Components(s).StoichCoeff
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                    End With
                Next
            Next

            Dim ktot, prodtot As Double

            ktot = 1.0
            prodtot = 1.0

            Dim reaction As IReaction

            Pval = 0.0 'ReturnPenaltyValue(Vxl)

            Dim f(x.Length - 1), k(x.Length - 1) As Double

            For i = 0 To Me.Reactions.Count - 1
                reaction = proppack.CurrentMaterialStream.Flowsheet.Reactions(Me.Reactions(i))
                k(i) = reaction.EvaluateK(T + reaction.Approach, proppack)
                f(i) = Math.Log(Math.Abs(k(i) / prod(i)))
                If Math.Abs(Pval) > 0.01 Then
                    If PenaltyValueScheme = 0 Then
                    ElseIf PenaltyValueScheme = 1 Then
                        f(i) *= Pval
                    ElseIf PenaltyValueScheme = 2 Then
                        If Math.Abs(Pval * f(i)) > 1.0 Then
                            f(i) = Pval * f(i)
                        Else
                            f(i) = Pval / f(i)
                        End If
                    ElseIf PenaltyValueScheme = 3 Then
                        If Math.Abs(f(i)) >= 0.0 And Math.Abs(f(i)) < 1.0 Then
                            f(i) = Pval / f(i)
                        Else
                            f(i) = Pval * f(i)
                        End If
                    End If
                End If
            Next

            Return f

        End Function

        Private Function ReturnPenaltyValue(Vx() As Double) As Double

            'calculate penalty functions for constraint variables

            Dim i As Integer
            Dim n As Integer = Vx.Length - 1

            Dim con_lc(n), con_uc(n), con_val(n) As Double
            Dim pen_val As Double
            Dim delta1, delta2 As Double

            For i = 0 To n
                con_lc(i) = 0.0#
                con_uc(i) = 1.0#
                con_val(i) = Vx(i)
            Next

            pen_val = 0
            For i = 0 To n
                delta1 = con_val(i) - con_lc(i)
                delta2 = con_val(i) - con_uc(i)
                If delta1 < 0 Then
                    pen_val += delta1 * 100 * (i + 1) ^ 2
                ElseIf delta2 > 1 Then
                    pen_val += delta2 * 100 * (i + 1) ^ 2
                Else
                    pen_val += 0.0#
                End If
            Next

            If Double.IsNaN(pen_val) Then pen_val = 0.0#

            Return pen_val

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
            If flashresult.ResultException IsNot Nothing Then Throw flashresult.ResultException
            Pmax = flashresult.CalculatedPressure
            flashresult = nl.CalculateEquilibrium(FlashSpec.T, FlashSpec.VAP, T, 1.0#, proppack, Vz, Nothing, Pref)
            If flashresult.ResultException IsNot Nothing Then Throw flashresult.ResultException
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
