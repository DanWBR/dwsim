'    Flash Algorithm for Electrolyte solutions
'    Copyright 2013-2016 Daniel Wagner O. de Medeiros
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

        Public proppack As PropertyPackage

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

        Public Property MaximumIterations As Integer = 10000
        Public Property Tolerance As Double = 1.0E-30

        Public Property ObjectiveFunctionHistory As New List(Of Double)

        Public Property CalculateChemicalEquilibria As Boolean = True

        Private Vx0 As Double()

        Private LoopVarF, LoopVarX As Double, LoopVarVz As Double(), LoopVarState As State

        Public Overloads Function Flash_PT(Vx As Array, T As Double, P As Double) As Dictionary(Of String, Object)

            'This flash algorithm is for Electrolye/Salt systems with Water as the single solvent.
            'The vapor and solid phases are considered to be ideal.
            'Chemical equilibria is calculated using the reactions enabled in the default reaction set.

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
            Dim Vnf(n), Vnl(n), Vnl_ant(n), Vxl(n), Vns(n), Vxs(n), Vnv(n), Vxv(n), Vf(n), V, S, L, Vp(n), err As Double
            Dim sumN As Double = 0

            'get water index in the array.

            Vnf = Vx.Clone

            Dim wid As Integer = CompoundProperties.IndexOf((From c As Interfaces.ICompoundConstantProperties In CompoundProperties Select c Where c.Name = "Water").SingleOrDefault)

            'calculate water vapor pressure.

            Dim Psat = proppack.AUX_PVAPi(wid, T)

            For i = 0 To n
                If i <> wid Then
                    Vxv(i) = 0.0#
                Else
                    Vxv(i) = 1.0#
                End If
            Next

            If Vnf(wid) = 0.0# Then

                'only solids in the stream (no liquid water).

                V = 0.0#
                L = 0.0#
                S = 1.0#
                For i = 0 To n
                    Vxs(i) = Vnf(i)
                    Vns(i) = Vnf(i)
                Next
                Vf = Vnf
                sumN = 1.0#

            Else

                V = 0.0#
                L = 1.0#
                S = 0.0#

                'calculate SLE.

                Dim ids As New List(Of String)
                For i = 0 To n
                    ids.Add(CompoundProperties(i).Name)
                    Vp(i) = proppack.AUX_PVAPi(i, T)
                    If Double.IsNaN(Vp(i)) Then Vp(i) = 0.0#
                Next

                'get the default reaction set.

                Me.Vx0 = Vx.Clone

                Vnl = Vx.Clone
                Vf = Vx.Clone

                Dim int_count As Integer = 0
                Dim L_ant As Double = 0.0#

                Dim rext As Double() = Nothing
                Dim result As Object

                'Do

                'calculate chemical equilibria between ions, salts and water. 

                ''SolveChemicalEquilibria' returns the equilibrium molar amounts in the liquid phase, including precipitates.

                If CalculateChemicalEquilibria And Vp(wid) < P Then
                    result = SolveChemicalEquilibria(Vnf, T, P, ids, rext)
                    Vnl = result(0).clone
                    rext = result(1)
                End If

                For i = 0 To n
                    Vxl(i) = Vnl(i) / Vnl.Sum()
                Next

                'calculate activity coefficients.

                If TypeOf proppack Is ExUNIQUACPropertyPackage Then
                    activcoeff = CType(proppack, ExUNIQUACPropertyPackage).m_uni.GAMMA_MR(T, Vxl.Clone, CompoundProperties)
                ElseIf TypeOf proppack Is LIQUAC2PropertyPackage Then
                    activcoeff = CType(proppack, LIQUAC2PropertyPackage).m_uni.GAMMA_MR(T, Vxl.Clone, CompoundProperties)
                End If

                If activcoeff(wid) = 0.0# Then
                    'possibly only solids in this stream. will make arrangements so the maximum solubility is very small.
                    For i = 0 To n
                        activcoeff(i) = 10000000000.0
                    Next
                End If

                Dim Vxlmax(n) As Double

                'calculate maximum solubilities for solids/precipitates.

                For i = 0 To n
                    If CompoundProperties(i).IsSalt Then
                        Vxlmax(i) = 0.0#
                    ElseIf CompoundProperties(i).TemperatureOfFusion <> 0.0# And i <> wid Then
                        Vxlmax(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                        If Vxlmax(i) > 1 Then Vxlmax(i) = 1.0#
                    Else
                        If CompoundProperties(i).IsHydratedSalt Then
                            Vxlmax(i) = 0.0# 'in the absence of enthalpy/temperature of fusion, I'll assume that the hydrated salt will always precipitate, if present.
                        Else
                            Vxlmax(i) = 1.0#
                        End If
                    End If
                Next

                'mass balance.

                For i = 0 To n
                    Vnl_ant(i) = Vnl(i)
                    If Vnl(i) > Vxlmax(i) * L Then
                        Vnl(i) = Vxlmax(i) * L
                        Vns(i) = Vnl_ant(i) - Vxlmax(i) * L
                    Else
                        Vns(i) = 0
                    End If
                    If Vns(i) < 0.0# Then Vns(i) = 0.0#
                Next

                For i = 0 To n
                    If Vp(i) > P Then
                        Vnv(i) = Vnl(i) + Vns(i)
                        Vns(i) = 0.0#
                        Vnl(i) = 0.0#
                    End If
                Next

                'liquid mole amounts

                L_ant = L
                L = Vnl.Sum()
                S = Vns.Sum()
                V = Vnv.Sum()

                For i = 0 To n
                    If Sum(Vnl) <> 0.0# Then Vxl(i) = Vnl(i) / Sum(Vnl) Else Vxl(i) = 0.0#
                    If Sum(Vns) <> 0.0# Then Vxs(i) = Vns(i) / Sum(Vns) Else Vxs(i) = 0.0#
                    If Sum(Vnv) <> 0.0# Then Vxv(i) = Vnv(i) / Sum(Vnv) Else Vxv(i) = 0.0#
                Next

                sumN = 0
                For i = 0 To n
                    'Vf(i) = Vnv(i) + Vnl(i) + Vns(i)
                    sumN += Vnv(i) + Vnl(i) + Vns(i)
                Next

                err = 0
                For i = 0 To n
                    err += Abs(Vnl(i) - Vnl_ant(i)) ^ 2
                Next
                err += (L - L_ant) ^ 2

                'If err < Tolerance And int_count > 0 Then Exit Do

                int_count += 1

                If Not proppack.CurrentMaterialStream.Flowsheet Is Nothing Then proppack.CurrentMaterialStream.Flowsheet.CheckStatus()

                'Loop Until int_count > MaximumIterations

                If int_count > MaximumIterations Then
                    Throw New Exception("Chemical Equilibrium Solver error: Reached the maximum number of external iterations without converging.")
                End If

            End If

            L = Vnl.Sum()
            S = Vns.Sum()
            V = Vnv.Sum()

            For i = 0 To n
                If Sum(Vnl) <> 0.0# Then Vxl(i) = Vnl(i) / Sum(Vnl) Else Vxl(i) = 0.0#
                If Sum(Vns) <> 0.0# Then Vxs(i) = Vns(i) / Sum(Vns) Else Vxs(i) = 0.0#
                If Sum(Vnv) <> 0.0# Then Vxv(i) = Vnv(i) / Sum(Vnv) Else Vxv(i) = 0.0#
            Next

            L = L / sumN
            S = S / sumN
            V = V / sumN

            'return flash calculation results.

            Dim results As New Dictionary(Of String, Object)

            results.Add("MixtureMoleFlows", Vf)
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

        Private Function SolveChemicalEquilibria(ByVal Vx As Array, ByVal T As Double, ByVal P As Double, ByVal ids As List(Of String), Optional ByVal prevx As Double() = Nothing) As Array

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
                    N0.Add(cname, Vx(ids.IndexOf(cname)))
                    DN.Add(cname, 0)
                    N.Add(cname, Vx(ids.IndexOf(cname)))
                Next

                N0.Values.CopyTo(fm0, 0)

                N0tot = 1.0#
                Ninerts = N0tot - Sum(fm0)

                'upper and lower bounds for reaction extents (just an estimate, will be different when a single compound appears in multiple reactions)

                Dim lbound(Me.ReactionExtents.Count - 1) As Double
                Dim ubound(Me.ReactionExtents.Count - 1) As Double
                Dim var1 As Double

                i = 0
                For Each rxid As String In Me.Reactions
                    rx = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxid)
                    j = 0
                    For Each comp As Interfaces.IReactionStoichBase In rx.Components.Values
                        var1 = -N0(comp.CompName) / comp.StoichCoeff
                        If j = 0 Then
                            lbound(i) = Log(1.0E-40)
                            ubound(i) = Log(var1)
                        Else
                            If Log(var1) < lbound(i) Then lbound(i) = Log(1.0E-40)
                            If Log(var1) > ubound(i) Then ubound(i) = Log(var1)
                        End If
                        j += 1
                    Next
                    i += 1
                Next

                Me.T = T
                Me.P = P

                'initial estimates for the reaction extents

                Dim REx(r), iest(r), x(r), fx As Double

                If Not prevx Is Nothing Then
                    For i = 0 To r
                        REx(i) = prevx(i) * 0.9
                    Next
                Else
                    i = 0
                    For Each rxnsb As Interfaces.IReactionSetBase In proppack.CurrentMaterialStream.Flowsheet.ReactionSets(Me.ReactionSet).Reactions.Values
                        If proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Equilibrium And rxnsb.IsActive Then
                            rxn = proppack.CurrentMaterialStream.Flowsheet.Reactions(rxnsb.ReactionID)
                            If rxn.ConstantKeqValue < 1 Then
                                REx(i) = 1.0E-20
                            Else
                                REx(i) = Exp(ubound(i)) * 0.9
                            End If
                            i += 1
                        End If
                    Next
                End If

                'optimize initial estimates

                If ubound.ExpY.SumY > 0.0# Then

                    Dim extvars(r) As OptBoundVariable
                    For i = 0 To r
                        extvars(i) = New OptBoundVariable("ex" & CStr(i + 1), Log(REx(i)), False, lbound(i), ubound(i))
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
                                                    intsolver.Tolerance = Tolerance
                                                    intsolver.MaxFunEvaluations = MaximumIterations
                                                    x = intsolver.ComputeMin(AddressOf FunctionValue2N, intvars)

                                                    intsolver = Nothing

                                                    fx = Me.FunctionValue2N(x)

                                                    Return fx

                                                End Function, extvars)

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

                    Dim solver As New Simplex
                    solver.Tolerance = Tolerance
                    solver.MaxFunEvaluations = MaximumIterations
                    x = solver.ComputeMin(AddressOf FunctionValue2N, variables)

                    If solver.FunEvaluations >= solver.MaxFunEvaluations Then
                        Throw New Exception("Chemical Equilibrium Solver error: Reached the maximum number of internal iterations without converging.")
                    End If

                    fx = Me.FunctionValue2N(x)

                    If Double.IsNaN(fx) Then
                        Throw New Exception("Chemical Equilibrium Solver error: general numerical error.")
                    End If

                End If

                'comp. conversions

                For Each sb As String In ids
                    If Me.ComponentConversions.ContainsKey(sb) Then
                        Me.ComponentConversions(sb) = -DN(sb) / N0(sb)
                    End If
                Next

                ' return equilibrium molar amounts in the liquid phase.

                Dim Vx2 As Double() = Vx.Clone

                For Each s As String In N.Keys
                    Vx2(ids.IndexOf(s)) = Abs(N(s))
                Next

                Dim nc As Integer = Vx.Length - 1

                Dim mtot As Double = 0
                For i = 0 To nc
                    mtot += Vx2(i)
                Next

                'For i = 0 To nc
                '    Vx(i) = Vx(i) '/ mtot
                'Next

                Return New Object() {Vx2, x}

            Else

                Return New Object() {Vx, Nothing}

            End If

        End Function

        Private Function FunctionValue2N(ByVal x() As Double) As Double

            Dim i, j, nc As Integer

            nc = Me.CompoundProperties.Count - 1

            i = 0
            For Each s As String In N.Keys
                DN(s) = 0
                For j = 0 To r
                    DN(s) += E(i, j) * Exp(x(j))
                Next
                i += 1
            Next

            For Each s As String In DN.Keys
                N(s) = N0(s) + DN(s)
                'If N(s) < 0.0# Then N(s) = -N(s)
            Next

            Dim Vx(nc) As Double

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0.0#
            Dim mtotal As Double = 0.0#
            Dim molality(nc) As Double

            For i = 0 To nc
                Vx(i) = Vx0(i)
            Next

            For i = 0 To N.Count - 1
                For j = 0 To nc
                    If CompoundProperties(j).Name = ComponentIDs(i) Then
                        Vx(j) = N(ComponentIDs(i))
                        Exit For
                    End If
                Next
            Next

            Dim val1, val2 As Double

            val1 = Vx0.SumY * proppack.AUX_MMM(Vx0)
            val2 = Vx.SumY * proppack.AUX_MMM(Vx)

            Dim pen_val As Double = (val1 - val2) ^ 2

            i = 0
            Do
                If CompoundProperties(i).Name = "Water" Then
                    wtotal += Vx(i) * CompoundProperties(i).Molar_Weight / 1000
                End If
                mtotal += Vx(i)
                i += 1
            Loop Until i = nc + 1

            Dim Xsolv As Double = 1

            Dim wden As Double = 0.0#
            If TypeOf proppack Is ExUNIQUACPropertyPackage Then
                wden = CType(proppack, ExUNIQUACPropertyPackage).m_elec.LiquidDensity(Vx, T, CompoundProperties)
            ElseIf TypeOf proppack Is LIQUAC2PropertyPackage Then
                wden = CType(proppack, ExUNIQUACPropertyPackage).m_elec.LiquidDensity(Vx, T, CompoundProperties)
            End If

            i = 0
            Do
                'Vx(i) /= mtotal
                molality(i) = Vx(i) / wtotal * wden / 1000
                i += 1
            Loop Until i = nc + 1

            Dim activcoeff(nc) As Double

            If TypeOf proppack Is ExUNIQUACPropertyPackage Then
                activcoeff = CType(proppack, ExUNIQUACPropertyPackage).m_uni.GAMMA_MR(T, Vx.Clone, CompoundProperties)
            ElseIf TypeOf proppack Is LIQUAC2PropertyPackage Then
                activcoeff = CType(proppack, LIQUAC2PropertyPackage).m_uni.GAMMA_MR(T, Vx.Clone, CompoundProperties)
            End If

            If IdealCalc Then
                For i = 0 To nc
                    activcoeff(i) = 1.0#
                Next
            End If

            Dim CP(nc) As Double
            Dim f1, f2 As Double
            Dim prod(Me.Reactions.Count - 1) As Double

            For i = 0 To nc
                If CompoundProperties(i).IsIon Then
                    CP(i) = molality(i) * activcoeff(i)
                ElseIf CompoundProperties(i).IsSalt Then
                    CP(i) = 1.0#
                Else
                    CP(i) = Vx(i) * activcoeff(i)
                End If
            Next

            For i = 0 To Me.Reactions.Count - 1
                prod(i) = 1
                For Each s As String In Me.ComponentIDs
                    With proppack.CurrentMaterialStream.Flowsheet.Reactions(Me.Reactions(i))
                        If .Components.ContainsKey(s) Then
                            For j = 0 To nc
                                If CompoundProperties(j).Name = s And Not CompoundProperties(j).IsSalt Then
                                    prod(i) *= CP(j) ^ .Components(s).StoichCoeff
                                    Exit For
                                End If
                            Next
                        End If
                    End With
                Next
            Next

            For i = 0 To Me.Reactions.Count - 1
                With proppack.CurrentMaterialStream.Flowsheet.Reactions(Me.Reactions(i))
                    f1 += Log(.ConstantKeqValue)
                    f2 += Log(prod(i))
                End With
            Next

            'If Double.IsNaN(f2) Or Double.IsInfinity(f2) Then f2 = Log(1000000.0)

            Dim fval As Double = (f1 - f2) ^ 2 + pen_val

            ObjectiveFunctionHistory.Add(fval)

            Return fval

        End Function

        Private Function FunctionGradient2N(ByVal x() As Double) As Double()

            Dim epsilon As Double = Log(0.1)

            Dim f1, f2 As Double
            Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
            Dim i, j As Integer

            For i = 0 To x.Length - 1
                For j = 0 To x.Length - 1
                    If i <> j Then
                        x1(j) = x(j)
                        x2(j) = x(j)
                    Else
                        x1(j) = x(j) * (1 - epsilon)
                        x2(j) = x(j) * (1 + epsilon)
                    End If
                Next
                f1 = FunctionValue2N(x1)
                f2 = FunctionValue2N(x2)
                g(i) = (f2 - f1) / (x2(i) - x1(i))
            Next

            Return g

        End Function

        Public Overloads Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double) As Dictionary(Of String, Object)

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1) As Double
            Dim n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, Pf As Double

            d1 = Date.Now

            n = Vz.Length - 1

            Hf = H
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n)

            Vx0 = Vz.Clone

            Dim Tmin, Tmax, epsilon(4), maxDT As Double

            Tmax = 2000.0#
            Tmin = 50.0#
            maxDT = 5.0#

            epsilon(0) = 0.1
            epsilon(1) = 0.01
            epsilon(2) = 0.001
            epsilon(3) = 0.0001
            epsilon(4) = 0.00001

            Dim fx As Double
            If Tref = 0.0# Then Tref = 298.15

            Me.P = P

            Dim tvar As New OptBoundVariable(Tref, 273.15, 500)

            Dim solver As New Simplex
            solver.Tolerance = Tolerance
            solver.MaxFunEvaluations = MaximumIterations
            T = solver.ComputeMin(AddressOf Herror, New OptBoundVariable() {tvar})(0)

            fx = Herror({T})

            If Double.IsNaN(T) Or solver.FunEvaluations > MaximumIterations Then
                Throw New Exception("PH Flash [Electrolyte]: Invalid result: Temperature did not converge.")
            End If

            Dim tmp As Object = Flash_PT(Vz, T, P)

            Dim S, Vs(), sumN As Double

            sumN = tmp("MoleSum")
            L = tmp("LiquidPhaseMoleFraction")
            V = tmp("VaporPhaseMoleFraction")
            S = tmp("SolidPhaseMoleFraction")
            Vx = tmp("LiquidPhaseMolarComposition")
            Vy = tmp("VaporPhaseMolarComposition")
            Vs = tmp("SolidPhaseMolarComposition")

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [Electrolyte]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            'return flash calculation results.

            Dim results As New Dictionary(Of String, Object)

            results.Add("MixtureMoleFlows", Vz)
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
            Return OBJ_FUNC_PH_FLASH(x(0), P, Vx0)
        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal P As Double, ByVal Vz As Object) As Double

            Dim tmp As Dictionary(Of String, Object) = Flash_PT(Vz, T, P)

            Dim L, V, S, Vx(), Vy(), Vs(), sumN, _Hv, _Hl, _Hs As Double

            Dim n = Vz.Length - 1

            sumN = tmp("MoleSum")
            L = tmp("LiquidPhaseMoleFraction")
            V = tmp("VaporPhaseMoleFraction")
            S = tmp("SolidPhaseMoleFraction")
            Vx = tmp("LiquidPhaseMolarComposition")
            Vy = tmp("VaporPhaseMolarComposition")
            Vs = tmp("SolidPhaseMolarComposition")
            'Vz = tmp("MixtureMoleFlows")

            _Hv = 0
            _Hl = 0
            _Hs = 0

            Dim mmg, mml, mms As Double
            If V > 0.0# Then _Hv = proppack.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L > 0.0# Then _Hl = proppack.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
            If S > 0.0# Then _Hs = proppack.DW_CalcSolidEnthalpy(T, Vs, CompoundProperties)
            mmg = proppack.AUX_MMM(Vy)
            mml = proppack.AUX_MMM(Vx)
            mms = proppack.AUX_MMM(Vs)

            Dim herr As Double = Hf - ((mmg * V / (mmg * V + mml * L + mms * S)) * _Hv + (mml * L / (mmg * V + mml * L + mms * S)) * _Hl + (mms * S / (mmg * V + mml * L + mms * S)) * _Hs)
            OBJ_FUNC_PH_FLASH = herr ^ 2

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

            Dim Vx(n), Vy(n), Vp(n), Vcalc, Vspec, T, x, x0, x00, fx, fx0, fx00, Tmin, Tmax As Double

            Dim nl As New NestedLoops
            Dim flashresult = nl.CalculateEquilibrium(FlashSpec.P, FlashSpec.VAP, P, 0.0#, proppack, Vz, Nothing, Tref)
            Tmin = flashresult.CalculatedTemperature
            flashresult = nl.CalculateEquilibrium(FlashSpec.P, FlashSpec.VAP, P, 1.0#, proppack, Vz, Nothing, Tref)
            Tmax = flashresult.CalculatedTemperature

            If Tmin < 273.15 Then Tmin = 273.15

            T = Tmin + V * (Tmax - Tmin)

            ecount = 0
            Vspec = V
            x = T

            Dim tmp As Dictionary(Of String, Object)

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
                    x = x - fx * (x - x00) / (fx - fx00)
                    If Double.IsNaN(x) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))
                End If

                ecount += 1

                If ecount > maxitEXT Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

            Loop

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
            results.Add("Pressure", P)
            results.Add("LiquidPhaseActivityCoefficients", tmp("LiquidPhaseActivityCoefficients"))

            Return results

        End Function

        Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(), _
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
            Dim results = Flash_PH(Vz, P, H, Tref)
            'Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            With results
                Return New Object() {results("LiquidPhaseMoleFraction"), results("VaporPhaseMoleFraction"), results("LiquidPhaseMolarComposition"),
                                     results("VaporPhaseMolarComposition"), results("Temperature"), 1, PP.RET_NullVector, 0.0#, PP.RET_NullVector, results("SolidPhaseMoleFraction"), results("SolidPhaseMolarComposition")}
            End With
        End Function

        Public Overrides Function Flash_PS(Vz() As Double, P As Double, S As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            Throw New NotImplementedException
        End Function

        Public Overrides Function Flash_PT(Vz() As Double, P As Double, T As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            Dim results = Flash_PT(Vz, T, P)
            With results
                Return New Object() {results("LiquidPhaseMoleFraction"), results("VaporPhaseMoleFraction"), results("LiquidPhaseMolarComposition"),
                                     results("VaporPhaseMolarComposition"), 1, 0.0#, PP.RET_NullVector, results("SolidPhaseMoleFraction"), results("SolidPhaseMolarComposition")}
            End With
        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, V As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            Dim results = Flash_PV(Vz, P, V, Tref)
            With results
                Return New Object() {results("LiquidPhaseMoleFraction"), results("VaporPhaseMoleFraction"), results("LiquidPhaseMolarComposition"),
                                     results("VaporPhaseMolarComposition"), results("Temperature"), 1, PP.RET_NullVector, 0.0#, PP.RET_NullVector, results("SolidPhaseMoleFraction"), results("SolidPhaseMolarComposition")}
            End With
        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
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
    End Class

End Namespace
