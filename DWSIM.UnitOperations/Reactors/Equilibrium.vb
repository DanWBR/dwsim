'    Equilibrium Reactor Calculation Routines 
'    Copyright 2008-2010 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics.BaseClasses
Imports Ciloci.Flee
Imports System.Math
Imports System.Linq
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Thermodynamics
Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.Common

Namespace Reactors

    <System.Serializable()> Public Class Reactor_Equilibrium

        Inherits Reactor

        Protected m_reactionextents As New Dictionary(Of String, Double)
        Private _rex_iest As New ArrayList
        Private _components As New List(Of String)
        Private _initialestimates As New List(Of Double)
        Private _elements As String()
        Private _totalelements As Double()
        Private _ige, _fge As Double

        Dim tmpx As Double(), tmpdx As Double()

        Dim tms As MaterialStream
        Dim N0 As New Dictionary(Of String, Double)
        Dim DN As New Dictionary(Of String, Double)
        Dim N As New Dictionary(Of String, Double)
        Dim T, P, P0, Ninerts, Winerts, E(,) As Double
        Dim r, c, els, comps As Integer

#Region "Properties"

        Public Property InitialGibbsEnergy() As Double
            Get
                Return _ige
            End Get
            Set(ByVal value As Double)
                _ige = value
            End Set
        End Property

        Public Property FinalGibbsEnergy() As Double
            Get
                Return _fge
            End Get
            Set(ByVal value As Double)
                _fge = value
            End Set
        End Property

        Public ReadOnly Property ReactionExtents() As Dictionary(Of String, Double)
            Get
                Return Me.m_reactionextents
            End Get
        End Property

        Public ReadOnly Property ReactionExtentsEstimates() As ArrayList
            Get
                Return _rex_iest
            End Get
        End Property

        Public Property Elements() As String()
            Get
                Return _elements
            End Get
            Set(ByVal value As String())
                _elements = value
            End Set
        End Property

        Public ReadOnly Property ComponentIDs() As List(Of String)
            Get
                Return _components
            End Get
        End Property

        Public ReadOnly Property InitialEstimates() As List(Of Double)
            Get
                If _initialestimates Is Nothing Then _initialestimates = New List(Of Double)
                Return _initialestimates
            End Get
        End Property

        Public Property TotalElements() As Double()
            Get
                Return _totalelements
            End Get
            Set(ByVal value As Double())
                _totalelements = value
            End Set
        End Property

#End Region

#Region "Auxiliary Functions"

        Private Function FunctionGradient(ByVal x() As Double) As Double()

            Dim epsilon As Double = 0.0001

            Dim f1, f2 As Double
            Dim g(x.Length - 1), x2(x.Length - 1) As Double
            Dim i, j As Integer

            For i = 0 To x.Length - 1
                f1 = FunctionValue(x)
                For j = 0 To x.Length - 1
                    If x(j) = 0 Then
                        If i <> j Then
                            x2(j) = (x(j) + 0.000001)
                        Else
                            x2(j) = (x(j) + 0.000001) * (1 + epsilon)
                        End If
                    Else
                        If i <> j Then
                            x2(j) = x(j)
                        Else
                            x2(j) = x(j) * (1 + epsilon)
                        End If
                    End If
                Next
                f2 = FunctionValue(x2)
                g(i) = (f2 - f1) / (x2(i) - x(i))
            Next

            Return g

        End Function

        Private Function FunctionValue(ByVal x() As Double) As Double

            Dim i, j As Integer

            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage

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
            Next

            Dim fw(c), fm(c), sumfm, sum1, sumn, sumw As Double

            N.Values.CopyTo(fm, 0)

            sumfm = Sum(fm) + Ninerts

            sum1 = 0.0#
            sumn = 0.0#
            For Each s As Compound In tms.Phases(0).Compounds.Values
                If Me.ComponentIDs.Contains(s.Name) Then
                    s.MolarFlow = N(s.Name)
                    s.MoleFraction = N(s.Name) / sumfm
                    sum1 += N(s.Name) * s.ConstantProperties.Molar_Weight / 1000
                Else
                    s.MoleFraction = s.MolarFlow / sumfm
                End If
                sumn += s.MolarFlow
            Next

            tms.Phases(0).Properties.molarflow = sumn

            sumw = 0.0#
            For Each s As Compound In tms.Phases(0).Compounds.Values
                If Me.ComponentIDs.Contains(s.Name) Then
                    s.MassFlow = N(s.Name) * s.ConstantProperties.Molar_Weight / 1000
                End If
                s.MassFraction = s.MassFlow / (sum1 + Winerts)
                sumw += s.MassFlow
            Next

            tms.Phases(0).Properties.massflow = sumw

            With pp
                .CurrentMaterialStream = tms
                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                '.DW_CalcPhaseProps(PropertyPackages.Phase.Mixture)
                '.DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                '.DW_CalcPhaseProps(PropertyPackages.Phase.Liquid)
                '.DW_CalcCompMolarFlow(-1)
                '.DW_CalcCompMassFlow(-1)
                '.DW_CalcCompVolFlow(-1)
                '.DW_CalcOverallProps()
                '.DW_CalcTwoPhaseProps(PropertyPackages.Phase.Liquid, PropertyPackages.Phase.Vapor)
                '.DW_CalcVazaoVolumetrica()
                '.DW_CalcKvalue()
            End With

            Dim fugs(tms.Phases(0).Compounds.Count - 1) As Double
            Dim CP(tms.Phases(0).Compounds.Count - 1) As Double
            Dim prod(x.Length - 1) As Double
            'Dim DGf As Double

            i = 0
            For Each s As Compound In tms.Phases(2).Compounds.Values
                If s.MoleFraction > 0.0# Then
                    fugs(i) = s.FugacityCoeff.GetValueOrDefault
                    CP(i) = (fugs(i) * s.MoleFraction.GetValueOrDefault * P / P0)
                Else
                    fugs(i) = s.FugacityCoeff.GetValueOrDefault
                    CP(i) = (fugs(i) * 0.0000000001 * P / P0)
                End If
                i += 1
            Next

            For i = 0 To Me.Reactions.Count - 1
                prod(i) = 1.0#
                j = 0
                For Each s As Compound In tms.Phases(2).Compounds.Values
                    With FlowSheet.Reactions(Me.Reactions(i))
                        If .Components.ContainsKey(s.Name) Then
                            prod(i) *= CP(j) ^ .Components(s.Name).StoichCoeff
                        End If
                    End With
                    j += 1
                Next
            Next

            Dim pen_val As Double = ReturnPenaltyValue()

            Dim objfunc As Double = 0
            For i = 0 To Me.Reactions.Count - 1
                With FlowSheet.Reactions(Me.Reactions(i))
                    objfunc += Abs(prod(i) - .ConstantKeqValue)
                End With
            Next


            Dim fval As Double
            If Double.IsNaN(objfunc) Or Double.IsInfinity(objfunc) Then
                fval = pen_val
            Else
                fval = objfunc + pen_val
            End If
            Return fval

        End Function

        Private Function FunctionValue2N(ByVal x() As Double) As Double()

            Dim i, j As Integer

            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage

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
            Next

            Dim fw(c), fm(c), sumfm, sum1, sumn, sumw As Double

            N.Values.CopyTo(fm, 0)

            sumfm = Sum(fm) + Ninerts

            sum1 = 0.0#
            sumn = 0.0#
            For Each s As Compound In tms.Phases(0).Compounds.Values
                If Me.ComponentIDs.Contains(s.Name) Then
                    s.MolarFlow = N(s.Name)
                    s.MoleFraction = N(s.Name) / sumfm
                    sum1 += N(s.Name) * s.ConstantProperties.Molar_Weight / 1000
                Else
                    s.MoleFraction = s.MolarFlow / sumfm
                End If
                sumn += s.MolarFlow
            Next

            tms.Phases(0).Properties.molarflow = sumn

            sumw = 0.0#
            For Each s As Compound In tms.Phases(0).Compounds.Values
                If Me.ComponentIDs.Contains(s.Name) Then
                    s.MassFlow = N(s.Name) * s.ConstantProperties.Molar_Weight / 1000
                End If
                s.MassFraction = s.MassFlow / (sum1 + Winerts)
                sumw += s.MassFlow
            Next

            tms.Phases(0).Properties.massflow = sumw

            With pp
                .CurrentMaterialStream = tms
                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                '.DW_CalcPhaseProps(PropertyPackages.Phase.Mixture)
                '.DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                '.DW_CalcPhaseProps(PropertyPackages.Phase.Liquid)
                '.DW_CalcCompMolarFlow(-1)
                '.DW_CalcCompMassFlow(-1)
                '.DW_CalcCompVolFlow(-1)
                '.DW_CalcOverallProps()
                '.DW_CalcTwoPhaseProps(PropertyPackages.Phase.Liquid, PropertyPackages.Phase.Vapor)
                '.DW_CalcVazaoVolumetrica()
                '.DW_CalcKvalue()
            End With

            Dim CP(tms.Phases(0).Compounds.Count - 1) As Double
            Dim f(x.Length - 1) As Double

            Dim fugs(tms.Phases(0).Compounds.Count - 1), prod(x.Length - 1) As Double

            i = 0
            For Each s As Compound In tms.Phases(2).Compounds.Values
                If s.MoleFraction > 0.0# Then
                    'DGf = pp.AUX_DELGF_T(298.15, T, s.Name) * s.ConstantProperties.Molar_Weight
                    fugs(i) = s.FugacityCoeff.GetValueOrDefault
                    CP(i) = (fugs(i) * s.MoleFraction.GetValueOrDefault * P / P0)
                Else
                    fugs(i) = s.FugacityCoeff.GetValueOrDefault
                    CP(i) = (fugs(i) * 0.01 * P / P0)
                End If
                i += 1
            Next

            For i = 0 To Me.Reactions.Count - 1
                prod(i) = 1.0#
                j = 0
                For Each s As Compound In tms.Phases(2).Compounds.Values
                    With FlowSheet.Reactions(Me.Reactions(i))
                        If .Components.ContainsKey(s.Name) Then
                            prod(i) *= CP(j) ^ .Components(s.Name).StoichCoeff
                        End If
                    End With
                    j += 1
                Next
            Next

            Dim pen_val As Double = ReturnPenaltyValue()

            For i = 0 To Me.Reactions.Count - 1
                With FlowSheet.Reactions(Me.Reactions(i))
                    f(i) = prod(i) - .ConstantKeqValue + pen_val
                    If Double.IsNaN(f(i)) Or Double.IsInfinity(f(i)) Then
                        f(i) = pen_val
                    End If
                End With
            Next

            Return f

        End Function

        Private Function FunctionValue2G(ByVal x() As Double) As Double

            Dim i As Integer

            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage

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
            Next

            Dim fw(c), fm(c), sumfm, sum1, sumn, sumw As Double

            N.Values.CopyTo(fm, 0)

            sumfm = Sum(fm) + Ninerts

            sum1 = 0
            sumn = 0
            For Each s As Compound In tms.Phases(0).Compounds.Values
                If Me.ComponentIDs.Contains(s.Name) Then
                    s.MolarFlow = N(s.Name)
                    s.MoleFraction = N(s.Name) / sumfm
                    sum1 += N(s.Name) * s.ConstantProperties.Molar_Weight / 1000
                Else
                    s.MoleFraction = s.MolarFlow / sumfm
                End If
                sumn += s.MolarFlow
            Next

            tms.Phases(0).Properties.molarflow = sumn

            sumw = 0
            For Each s As Compound In tms.Phases(0).Compounds.Values
                If Me.ComponentIDs.Contains(s.Name) Then
                    s.MassFlow = N(s.Name) * s.ConstantProperties.Molar_Weight / 1000
                End If
                s.MassFraction = s.MassFlow / (sum1 + Winerts)
                sumw += s.MassFlow
            Next

            tms.Phases(0).Properties.massflow = sumw

            With pp
                .CurrentMaterialStream = tms
                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                .DW_CalcPhaseProps(PropertyPackages.Phase.Mixture)
                .DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid)
                .DW_CalcCompMolarFlow(-1)
                .DW_CalcCompMassFlow(-1)
                .DW_CalcCompVolFlow(-1)
                .DW_CalcOverallProps()
                .DW_CalcTwoPhaseProps(PropertyPackages.Phase.Liquid, PropertyPackages.Phase.Vapor)
                .DW_CalcVazaoVolumetrica()
                .DW_CalcKvalue()
            End With

            Dim fugs(tms.Phases(0).Compounds.Count - 1) As Double
            Dim CP(tms.Phases(0).Compounds.Count - 1) As Double
            Dim DGf As Double

            i = 0
            For Each s As Compound In tms.Phases(2).Compounds.Values
                If s.MoleFraction <> 0.0# Then
                    DGf = pp.AUX_DELGF_T(298.15, T, s.Name) * s.ConstantProperties.Molar_Weight
                    fugs(i) = s.FugacityCoeff.GetValueOrDefault
                    CP(i) = s.MoleFraction * (DGf + Log(fugs(i) * s.MoleFraction.GetValueOrDefault * P / P0))
                Else
                    CP(i) = 0.0#
                End If
                i += 1
            Next

            Dim pen_val As Double = ReturnPenaltyValue()

            Dim gibbs As Double = Sum(CP) * sumn * 8.314 * T

            Return gibbs

        End Function

        Private Function FunctionGradient2N(ByVal x() As Double) As Double(,)

            Dim epsilon As Double = 0.0001

            Dim f1(), f2() As Double
            Dim g(x.Length - 1, x.Length - 1), x2(x.Length - 1) As Double
            Dim i, j, k As Integer

            f1 = FunctionValue2N(x)
            For i = 0 To x.Length - 1
                For j = 0 To x.Length - 1
                    If i <> j Then
                        x2(j) = x(j)
                    Else
                        x2(j) = x(j) * (1 + epsilon)
                    End If
                Next
                f2 = FunctionValue2N(x2)
                For k = 0 To x.Length - 1
                    g(k, i) = (f2(k) - f1(k)) / (x2(i) - x(i))
                Next
            Next

            Return g

        End Function

        Public Function MinimizeError(ByVal t As Double) As Double

            Dim tmpx0 As Double() = tmpx.Clone

            For i = 0 To comps + els
                tmpx0(i) -= tmpdx(i) * t
                'If tmpx0(i) < 0 And i <= comps Then tmpx0(i) = 0.000001
            Next

            Dim abssum0 = AbsSum(FunctionValue2N(tmpx0))
            Return abssum0

        End Function

        Private Function ReturnPenaltyValue() As Double

            'calculate penalty functions for constraint variables

            Dim i As Integer
            Dim n As Integer = tms.Phases(0).Compounds.Count - 1

            Dim con_lc(n), con_uc(n), con_val(n) As Double
            Dim pen_val As Double = 0
            Dim delta1, delta2 As Double

            i = 0
            For Each comp As Compound In tms.Phases(0).Compounds.Values
                con_lc(i) = 0.0#
                con_uc(i) = 1.0#
                con_val(i) = comp.MoleFraction.GetValueOrDefault
                i += 1
            Next

            pen_val = 0
            For i = 0 To n
                delta1 = con_val(i) - con_lc(i)
                delta2 = con_val(i) - con_uc(i)
                If delta1 < 0 Then
                    pen_val += -delta1 * 100000000000.0#
                ElseIf delta2 > 1 Then
                    pen_val += -delta2 * 100000000000.0#
                Else
                    pen_val += 0.0#
                End If
            Next

            If Double.IsNaN(pen_val) Then pen_val = 0.0#

            Return pen_val

        End Function

#End Region

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

            Me._rex_iest = New ArrayList()
            Me._components = New List(Of String)

        End Sub

        Public Overrides Sub Validate()

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac10"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac11"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac11"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim i, j As Integer

            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)
            If Me.ReactionExtents Is Nothing Then Me.m_reactionextents = New Dictionary(Of String, Double)
            If Me.ReactionExtentsEstimates Is Nothing Then Me._rex_iest = New ArrayList
            If Me.ComponentConversions Is Nothing Then Me.m_componentconversions = New Dictionary(Of String, Double)

            Me.Validate()

            Me.Reactions.Clear()
            Me.ReactionExtents.Clear()
            Me.Conversions.Clear()
            Me.ComponentConversions.Clear()
            Me.DeltaQ = 0.0#
            Me.DeltaT = 0.0#

            Dim rx As Reaction
            Dim ims As MaterialStream = GetInletMaterialStream(0).Clone
            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage
            Dim ppr As New PropertyPackages.RaoultPropertyPackage()

            ims.SetFlowsheet(Me.FlowSheet)

            'Reactants Enthalpy (kJ/kg * kg/s = kW) (ISOTHERMIC)
            Dim Hr0 As Double
            Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

            Dim tmp As IFlashCalculationResult

            'Copy results to upstream MS
            Dim xl, xv, H, S, wtotalx, wtotaly As Double
            pp.CurrentMaterialStream = ims

            T = ims.Phases(0).Properties.temperature.GetValueOrDefault
            P = ims.Phases(0).Properties.pressure.GetValueOrDefault
            P0 = 101325

            Dim rxn As Reaction

            'check active reactions (equilibrium only) in the reaction set
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                If FlowSheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Equilibrium And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    Me.ReactionExtents.Add(rxnsb.ReactionID, 0)

                    rxn = FlowSheet.Reactions(rxnsb.ReactionID)

                    'equilibrium constant calculation
                    Select Case rxn.KExprType
                        Case KOpt.Constant
                            'rxn.ConstantKeqValue = rxn.ConstantKeqValue
                        Case KOpt.Expression
                            rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                            rxn.ExpContext.Imports.AddType(GetType(System.Math))
                            rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                            rxn.ExpContext.Variables.Add("T", T)
                            rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.Expression)
                            Try
                                rxn.ConstantKeqValue = Exp(rxn.Expr.Evaluate)
                            Catch ex As Exception
                                Throw New Exception("Error evaluating equilibrium constant expression for reaction '" & rxn.Name & "': " & ex.Message.ToString)
                            End Try
                        Case KOpt.Gibbs
                            Dim id(rxn.Components.Count - 1) As String
                            Dim stcoef(rxn.Components.Count - 1) As Double
                            Dim bcidx As Integer = 0
                            j = 0
                            For Each sb As ReactionStoichBase In rxn.Components.Values
                                id(j) = sb.CompName
                                stcoef(j) = sb.StoichCoeff
                                If sb.IsBaseReactant Then bcidx = j
                                j += 1
                            Next
                            Dim DelG_RT = pp.AUX_DELGig_RT(298.15, T, id, stcoef, bcidx)
                            rxn.ConstantKeqValue = Exp(-DelG_RT)
                    End Select

                End If
            Next

            T = ims.Phases(0).Properties.temperature.GetValueOrDefault
            P = ims.Phases(0).Properties.pressure.GetValueOrDefault
            P0 = 101325.0#

            pp.CurrentMaterialStream = ims
            ppr.CurrentMaterialStream = ims

            'initial estimates for reaction extents

            tms = ims.Clone()
            tms.SetFlowsheet(ims.FlowSheet)

            Me.ComponentConversions.Clear()
            Me.ComponentIDs.Clear()

            'r: number of reactions
            'c: number of components
            'i,j: iterators

            i = 0
            For Each rxid As String In Me.Reactions
                rx = FlowSheet.Reactions(rxid)
                j = 0
                For Each comp As ReactionStoichBase In rx.Components.Values
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
                rx = FlowSheet.Reactions(rxid)
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

            Dim fm0(c), N0tot, W0tot, wm0 As Double

            N0.Clear()
            DN.Clear()
            N.Clear()

            For Each cname As String In Me.ComponentIDs
                N0.Add(cname, ims.Phases(0).Compounds(cname).MolarFlow.GetValueOrDefault)
                DN.Add(cname, 0)
                N.Add(cname, ims.Phases(0).Compounds(cname).MolarFlow.GetValueOrDefault)
                wm0 += ims.Phases(0).Compounds(cname).MassFlow.GetValueOrDefault
            Next

            N0.Values.CopyTo(fm0, 0)

            N0tot = ims.Phases(0).Properties.molarflow.GetValueOrDefault
            W0tot = ims.Phases(0).Properties.massflow.GetValueOrDefault

            Ninerts = N0tot - Sum(fm0)
            Winerts = W0tot - wm0

            Dim lbound(Me.ReactionExtents.Count - 1) As Double
            Dim ubound(Me.ReactionExtents.Count - 1) As Double
            Dim var1 As Double

            i = 0
            For Each rxid As String In Me.Reactions
                rx = FlowSheet.Reactions(rxid)
                j = 0
                For Each comp As ReactionStoichBase In rx.Components.Values
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

            Dim REx(r) As Double

            For i = 0 To r
                REx(i) = (lbound(i) + ubound(i)) / 2 'Me.ReactionExtentsEstimates(i)
            Next

            Dim g0, g1 As Double

            Dim REx0(REx.Length - 1) As Double

            g0 = FunctionValue2G(REx0)

            Me.InitialGibbsEnergy = g0

            'solve using newton's method

            Dim fx(r), dfdx(r, r), dx(r), x(r), df, fval As Double
            Dim brentsolver As New BrentOpt.BrentMinimize
            brentsolver.DefineFuncDelegate(AddressOf MinimizeError)

            Dim niter As Integer

            x = REx
            niter = 0
            Do

                fx = Me.FunctionValue2N(x)
                dfdx = Me.FunctionGradient2N(x)

                Dim success As Boolean
                success = SysLin.rsolve.rmatrixsolve(dfdx, fx, r + 1, dx)

                tmpx = x
                tmpdx = dx
                df = 1
                fval = brentsolver.brentoptimize(0.1, 1.5, 0.0001, df)

                For i = 0 To r
                    x(i) -= dx(i) * df
                    If x(i) <= 0 And i <= c Then x(i) = 0.000001 * N0tot
                Next

                niter += 1

                If AbsSum(dx) = 0.0# Then
                    Throw New Exception("No solution found - reached a stationary point of the objective function (singular gradient matrix).")
                End If

            Loop Until AbsSum(fx) < 0.00001 Or niter > 249

            If niter > 249 Then
                Throw New Exception("Reached the maximum number of iterations without converging.")
            End If

            'reevaluate function

            g1 = FunctionValue2G(REx)

            Me.FinalGibbsEnergy = g1

            i = 0
            For Each r As String In Me.Reactions
                Me.ReactionExtents(r) = REx(i)
                i += 1
            Next

            Dim DHr, Hp As Double

            DHr = 0

            i = 0
            Do
                'process reaction i
                rx = FlowSheet.Reactions(Me.Reactions(i))

                Dim id(rx.Components.Count - 1) As String
                Dim stcoef(rx.Components.Count - 1) As Double
                Dim bcidx As Integer = 0
                j = 0
                For Each sb As ReactionStoichBase In rx.Components.Values
                    id(j) = sb.CompName
                    stcoef(j) = sb.StoichCoeff
                    If sb.IsBaseReactant Then bcidx = j
                    j += 1
                Next

                'Heat released (or absorbed) (kJ/s = kW) (Ideal Gas)
                DHr += rx.ReactionHeat * Me.ReactionExtents(Me.Reactions(i)) * rx.Components(rx.BaseReactant).StoichCoeff / 1000
                'DHfT = pp.AUX_DELHig_RT(298.15, 298.15, id, stcoef, bcidx)
                'DHr += DHfT * Me.ReactionExtents(Me.Reactions(i)) * rx.Components(rx.BaseReactant).StoichCoeff / 1000
                i += 1
            Loop Until i = Me.Reactions.Count

            'Ideal Gas Reactants Enthalpy (kJ/kg * kg/s = kW)
            'Hid_r += 0 'ppr.RET_Hid(298.15, ims.Phases(0).Properties.temperature.GetValueOrDefault, PropertyPackages.Phase.Mixture) * ims.Phases(0).Properties.massflow.GetValueOrDefault

            ' comp. conversions
            For Each sb As Compound In ims.Phases(0).Compounds.Values
                If Me.ComponentConversions.ContainsKey(sb.Name) Then
                    Me.ComponentConversions(sb.Name) = -DN(sb.Name) / N0(sb.Name)
                End If
            Next

            'Check to see if are negative molar fractions.
            Dim sum1 As Double = 0
            For Each subst As Compound In tms.Phases(0).Compounds.Values
                If subst.MoleFraction.GetValueOrDefault < 0 Then
                    subst.MolarFlow = 0
                Else
                    sum1 += subst.MolarFlow.GetValueOrDefault
                End If
            Next
            For Each subst As Compound In tms.Phases(0).Compounds.Values
                subst.MoleFraction = subst.MolarFlow.GetValueOrDefault / sum1
            Next

            ims = tms.Clone
            ims.SetFlowsheet(tms.FlowSheet)

            Select Case Me.ReactorOperationMode

                Case OperationMode.Adiabatic

                    Me.DeltaQ = 0.0#

                    'Products Enthalpy (kJ/kg * kg/s = kW)
                    Hp = Hr0 + DHr

                    tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P, Hp / ims.Phases(0).Properties.massflow.GetValueOrDefault, 0)
                    Dim Tout As Double = tmp.CalculatedTemperature

                    Me.DeltaT = Tout - T
                    ims.Phases(0).Properties.temperature = Tout
                    T = ims.Phases(0).Properties.temperature.GetValueOrDefault

                    ims.Calculate(True, True)

                Case OperationMode.Isothermic

                    ims.Calculate(True, True)

                    'Products Enthalpy (kJ/kg * kg/s = kW)
                    Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                    'Heat (kW)
                    Me.DeltaQ = Hp - Hr0 - DHr

                    Me.DeltaT = 0

                Case OperationMode.OutletTemperature

                    Dim Tout As Double = Me.OutletTemperature

                    Me.DeltaT = Tout - T

                    ims.Phases(0).Properties.temperature = Tout

                    ims.Calculate(True, True)

                    'Products Enthalpy (kJ/kg * kg/s = kW)
                    Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                    'Heat (kW)
                    Me.DeltaQ = Hp - Hr0 - DHr

            End Select

            Dim W As Double = ims.Phases(0).Properties.massflow.GetValueOrDefault

            'do a flash calc (calculate final temperature/enthalpy)
            tmp = pp.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, ims.Phases(0).Properties.pressure.GetValueOrDefault, ims.Phases(0).Properties.temperature.GetValueOrDefault, 0)

            'Return New Object() {xl, xv, T, P, H, S, 1, 1, Vx, Vy}
            Dim Vx(ims.Phases(0).Compounds.Count - 1), Vy(ims.Phases(0).Compounds.Count - 1), Vwx(ims.Phases(0).Compounds.Count - 1), Vwy(ims.Phases(0).Compounds.Count - 1) As Double
            xl = tmp.GetLiquidPhase1MoleFraction
            xv = tmp.GetVaporPhaseMoleFraction
            T = tmp.CalculatedTemperature
            P = tmp.CalculatedPressure
            H = tmp.CalculatedEnthalpy
            S = tmp.CalculatedEntropy
            Vx = tmp.GetLiquidPhase1MoleFractions
            Vy = tmp.GetVaporPhaseMoleFractions

            Dim ms As MaterialStream
            Dim cp As ConnectionPoint
            cp = Me.GraphicObject.InputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedFrom.Name)
                Dim comp As BaseClasses.Compound
                i = 0
                For Each comp In ms.Phases(0).Compounds.Values
                    wtotalx += Vx(i) * comp.ConstantProperties.Molar_Weight
                    wtotaly += Vy(i) * comp.ConstantProperties.Molar_Weight
                    i += 1
                Next
                i = 0
                For Each comp In ms.Phases(0).Compounds.Values
                    Vwx(i) = Vx(i) * comp.ConstantProperties.Molar_Weight / wtotalx
                    Vwy(i) = Vy(i) * comp.ConstantProperties.Molar_Weight / wtotaly
                    i += 1
                Next
            End If

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    .Phases(0).Properties.enthalpy = H * (wtotaly * xv / (wtotaly * xv + wtotalx * xl))
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = Vy(j)
                        comp.MassFraction = Vwy(j)
                        j += 1
                    Next
                    j = 0
                    For Each comp In .Phases(2).Compounds.Values
                        comp.MoleFraction = Vy(j)
                        comp.MassFraction = Vwy(j)
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = W * (wtotaly * xv / (wtotaly * xv + wtotalx * xl))
                    .Phases(0).Properties.massfraction = (wtotaly * xv / (wtotaly * xv + wtotalx * xl))
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(3).Properties.massfraction = 0
                    .Phases(3).Properties.molarfraction = 0
                    .Phases(2).Properties.massfraction = 1
                    .Phases(2).Properties.molarfraction = 1
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    .Phases(0).Properties.enthalpy = H * (wtotalx * xl / (wtotaly * xv + wtotalx * xl))
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = Vx(j)
                        comp.MassFraction = Vwx(j)
                        j += 1
                    Next
                    j = 0
                    For Each comp In .Phases(3).Compounds.Values
                        comp.MoleFraction = Vx(j)
                        comp.MassFraction = Vwx(j)
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = W * (wtotalx * xl / (wtotaly * xv + wtotalx * xl))
                    .Phases(0).Properties.massfraction = (wtotalx * xl / (wtotaly * xv + wtotalx * xl))
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(3).Properties.massfraction = 1
                    .Phases(3).Properties.molarfraction = 1
                    .Phases(2).Properties.massfraction = 0
                    .Phases(2).Properties.molarfraction = 0
                End With
            End If

            'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
            With GetInletEnergyStream(1)
                .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                .GraphicObject.Calculated = True
            End With

        End Sub

        Public Overrides Sub DeCalculate()

            Dim j As Integer

            Dim ms As MaterialStream
            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_HT_0	Pressure Drop
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)

            End Select

            Return value
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RW
                    For i = 0 To 0
                        proplist.Add("PROP_EQ_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 0
                        proplist.Add("PROP_EQ_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 0
                        proplist.Add("PROP_EQ_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_HT_0	Pressure Drop
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)

            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_HT_0	Pressure Drop
                    value = su.deltaP

            End Select

            Return value
        End Function

        Public Overrides Sub DisplayEditForm()

        End Sub
    End Class

End Namespace


