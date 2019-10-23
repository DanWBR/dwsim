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
Imports DotNumerics.Optimization

Namespace Reactors

    <System.Serializable()> Public Class Reactor_Equilibrium

        Inherits Reactor

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_ReactorConvEqGibbs

        Private _IObj As InspectorItem

        Dim tmpx As Double(), tmpdx As Double()

        Dim tms As MaterialStream

        Dim N0 As New Dictionary(Of String, Double)
        Dim DN As New Dictionary(Of String, Double)
        Dim N As New Dictionary(Of String, Double)

        Dim T, T0, P, P0, Ninerts, Winerts, E(,) As Double

        Dim r, c, els, comps As Integer


#Region "Properties"

        Public Property InitialGibbsEnergy As Double = 0.0

        Public Property FinalGibbsEnergy As Double = 0.0

        Public Property ReactionExtents As New Dictionary(Of String, Double)

        Public Property PreviousReactionExtents As New Dictionary(Of String, Double)

        Public Property ComponentIDs As New List(Of String)

        Public Property UsePreviousReactionExtents As Boolean = False
        Public Property ReactionExtentsInitializer As Double = 0.2

        Public Property InternalLoopTolerance As Double = 0.00000001
        Public Property ExternalLoopTolerance As Double = 0.5

        Public Property InternalLoopMaximumIterations As Integer = 250

        Public Property ExternalLoopMaximumIterations As Integer = 50

        Public Property DerivativePerturbation As Double = 0.0001

        Public Property AlternateBoundsInitializer As Boolean = False

        Private NoPenVal As Boolean = False

#End Region

#Region "Auxiliary Functions"

        Private Function FunctionValue2N(ByVal x() As Double) As Double()

            If Double.IsNaN(x.Sum) Then Throw New Exception("Convergence Error")

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

            pp.CurrentMaterialStream = tms

            Dim cpv(tms.Phases(0).Compounds.Count - 1), cpl(tms.Phases(0).Compounds.Count - 1), basis(tms.Phases(0).Compounds.Count - 1) As Double
            Dim f(x.Length - 1) As Double

            Dim Vz As Double() = pp.RET_VMOL(PropertyPackages.Phase.Mixture)

            Dim fugv(tms.Phases(0).Compounds.Count - 1), fugl(tms.Phases(0).Compounds.Count - 1), prod(x.Length - 1) As Double

            _IObj?.SetCurrent
            fugv = pp.DW_CalcFugCoeff(Vz, T, P, PropertyPackages.State.Vapor)
            _IObj?.SetCurrent
            fugl = pp.DW_CalcFugCoeff(Vz, T, P, PropertyPackages.State.Liquid)

            i = 0
            For Each s As Compound In tms.Phases(0).Compounds.Values
                If s.MoleFraction > 0.0# Then
                    cpv(i) = (fugv(i) * Vz(i) * P / P0)
                    cpl(i) = (fugl(i) * Vz(i))
                Else
                    cpv(i) = (fugv(i) * 0.01 * P / P0)
                    cpl(i) = (fugl(i) * 0.01)
                End If
                i += 1
            Next

            For i = 0 To Me.Reactions.Count - 1
                prod(i) = 1.0#
                j = 0
                For Each s As Compound In tms.Phases(0).Compounds.Values
                    With FlowSheet.Reactions(Me.Reactions(i))
                        If .ReactionPhase = PhaseName.Vapor AndAlso .Components.ContainsKey(s.Name) Then
                            Select Case .ReactionBasis
                                Case ReactionBasis.Activity, ReactionBasis.Fugacity
                                    basis(j) = cpv(j)
                                Case ReactionBasis.MassFrac
                                    basis(j) = pp.AUX_CONVERT_MOL_TO_MASS(Vz)(j)
                                Case ReactionBasis.MolarFrac
                                    basis(j) = Vz(j)
                                Case ReactionBasis.PartialPress
                                    basis(j) = Vz(j) * fugv(j) * P
                                Case Else
                                    Throw New Exception("Selected Reaction Basis is not supported.")
                            End Select
                            prod(i) *= basis(j) ^ .Components(s.Name).StoichCoeff
                        ElseIf .ReactionPhase = PhaseName.Liquid AndAlso .Components.ContainsKey(s.Name) Then
                            Select Case .ReactionBasis
                                Case ReactionBasis.Activity, ReactionBasis.Fugacity
                                    basis(j) = cpl(j)
                                Case ReactionBasis.MassFrac
                                    basis(j) = pp.AUX_CONVERT_MOL_TO_MASS(Vz)(j)
                                Case ReactionBasis.MolarFrac
                                    basis(j) = Vz(j)
                                Case ReactionBasis.PartialPress
                                    basis(j) = Vz(j) * fugl(j) * P
                                Case Else
                                    Throw New Exception("Selected Reaction Basis is not supported.")
                            End Select
                            prod(i) *= basis(j) ^ .Components(s.Name).StoichCoeff
                        End If
                    End With
                    j += 1
                Next
            Next

            Dim pen_val As Double = If(NoPenVal, 0.0, ReturnPenaltyValue())

            For i = 0 To Me.Reactions.Count - 1
                With FlowSheet.Reactions(Me.Reactions(i))
                    f(i) = prod(i) - .EvaluateK(T, pp) + pen_val
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
                If N(s) < 0.0 Then N(s) = 0.0
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

            pp.CurrentMaterialStream = tms
            _IObj?.SetCurrent
            tms.Calculate(True, True)
            pp.CurrentMaterialStream = tms

            Return tms.Phases(0).Properties.gibbs_free_energy.GetValueOrDefault * tms.Phases(0).Properties.molecularWeight.GetValueOrDefault * tms.Phases(0).Properties.molarflow.GetValueOrDefault / 1000

        End Function

        Private Function FunctionGradient2N(ByVal x() As Double) As Double(,)

            Dim epsilon As Double = DerivativePerturbation

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
            Next

            Dim abssum0 As Double
            Try
                abssum0 = AbsSum(FunctionValue2N(tmpx0))
            Catch ex As Exception
                abssum0 = New Random().Next * 1.0E+20
            End Try
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
                    pen_val += -delta1 * 1000000.0# * (i + 1) ^ 2
                ElseIf delta2 > 1 Then
                    pen_val += -delta2 * 1000000.0# * (i + 1) ^ 2
                Else
                    pen_val += 0.0#
                End If
            Next

            If Double.IsNaN(pen_val) Then pen_val = 0.0#

            Return pen_val

        End Function

#End Region

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Reactor_Equilibrium()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Reactor_Equilibrium)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

            Me.ComponentIDs = New List(Of String)

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

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent

            IObj?.Paragraphs.Add("The calculation of chemical equilibrium at specified temperature and pressure is in many ways similar to the calculation of phase equilibrium. In both cases the equilibrium state corresponds to a global minimum of the Gibbs energy subject to a set of material balance constraints.")

            IObj?.Paragraphs.Add("For the phase equilibrium calculation these constraints represent component material balances, and they are usually eliminated explicitly by calculating the molar component amounts in one of the equilibrium phases from that in the other. A similar procedure can be used to convert the chemical equilibrium calculation to an unconstrained optimization problem, through the use of the 'reaction extents' formulation which ensures automatic satisfaction of all material balance constraints.")

            IObj?.Paragraphs.Add("<h3>Chemical reaction equilibrium</h3>")

            IObj?.Paragraphs.Add("In phase equilibrium calculations for a given feed at specified temperature and pressure a material balance must be satisfied for each component in the mixture, the total amount in the combined product phases being identical to that in the feed. When chemical reactions occur, additional degrees of freedom are available, resulting in a set of material balance constraints, which is smaller than the number of components in the mixture.")

            IObj?.Paragraphs.Add("The mixture composition at chemical equilibrium at constant T and p satisfies the condition of minimum Gibbs energy,")

            IObj?.Paragraphs.Add("<m>\min G = \min \sum\limits_{i=1}^{C}{n_i\mu _i} </m>")

            IObj?.Paragraphs.Add("subject to a set of M < C material balance constraints. In addition we must require that")

            IObj?.Paragraphs.Add("<m>n_i \geq 0, i=1,2,...,C</m>")

            IObj?.Paragraphs.Add("The material balance constraints can be formulated in different ways, and the most important formulations are outlined below. To illustrate the concepts, we shall consider a specific example, the combustion of a mixture of 1 mole propane (C3H8) and 5 moles oxygen (02), at 2200 K, 4.0 MPa. Under these conditions the reaction mixture is assumed to contain the following species (components) at equilibrium: C02 (1), CO (2), H20 (3), 02 (4), H2 (5), O (6), H (7) and OH (8).")

            IObj?.Paragraphs.Add("Independent chemical reactions and reaction extents ")

            IObj?.Paragraphs.Add("One approach eliminates the material balance constraints by formulating a complete set of independent chemical reactions between the mixture components. In our example, the following reactions could be chosen:")

            IObj?.Paragraphs.Add("2CO2 <--> 2CO + O2")
            IObj?.Paragraphs.Add("2H2O <--> 2H2 + O2")
            IObj?.Paragraphs.Add("H2 <--> 2H")
            IObj?.Paragraphs.Add("O2 <--> 2O")
            IObj?.Paragraphs.Add("H2 + O2 <--> 2OH")

            IObj?.Paragraphs.Add("Each reaction is characterized by a stoichiometric vector, <mi>\nu</mi>, where the i'th element of the vector represents the stoichiometric coefficient of component i in the reaction, with a negative sign for components on the left hand side and positive on the right hand side. Thus,")

            IObj?.Paragraphs.Add("<m>\nu_1=(-2,2,0,1,0,0,0,0)^T</m>")

            IObj?.Paragraphs.Add("The chosen set of chemical reactions must have linearly independent stoichiometric vectors, i.e., none of the reactions may be linear combinations of other reactions. If e.g. the reaction")

            IObj?.Paragraphs.Add("C02 + H2 <-->	CO + H2O")

            IObj?.Paragraphs.Add("was chosen a candidate for a potential 6th reaction, its stoichiometric vector")

            IObj?.Paragraphs.Add("<m>\nu_6=(-1,1,1,0,-1,0,0,0)^T</m>")

            IObj?.Paragraphs.Add("shows that the choice is illegal, since <mi>\nu_6=1/2(\nu_1-\nu_2)</mi>.")

            IObj?.Paragraphs.Add("The R (here, R = 5) stoichiometric vectors are combined into an C x R matrix E, given by")

            IObj?.Paragraphs.Add("<m>\mathbf{E}=(\nu_1,\nu_2,...,\nu_R)</m>")

            IObj?.Paragraphs.Add("A test for linear independence of the stoichiometric vectors is that the matrix E must be of rank R. Given an initial composition vector <mi>\nu_0</mi> consistent with the overall feed composition, the vector of moles n can be written in the general form")

            IObj?.Paragraphs.Add("<m>\mathbf{n}=\mathbf{n_0}+\sum\limits_{k=1}^{R}{\nu_k\zeta_k} </m>")

            IObj?.Paragraphs.Add("or")

            IObj?.Paragraphs.Add("<m>\mathbf{n}=\mathbf{n_0}+\mathbf{E}\zeta</m>")

            IObj?.Paragraphs.Add("where <mi>\zeta_k</mi> is called the extent of the k'th reaction.")

            IObj?.Paragraphs.Add("The composition vector no can be chosen as the feed composition, if all components present in the feed are also found in the equilibrium mixture. For the present example, the equilibrium concentration of propane is likely to be extremely small (below 10-30) and propane is therefore not included in the vector of possible product components. Some consistent choices of no are")

            IObj?.Paragraphs.Add("<m>\mathbf{n_0}=(3,0,4,0,0,0,0,0)^T</m>")

            IObj?.Paragraphs.Add("(3 moles C02, 4 moles H20) or")

            IObj?.Paragraphs.Add("<m>\mathbf{n_0}=(0,3,0,3.5,4,0,0,0)^T</m>")

            IObj?.Paragraphs.Add("(3 moles CO, 3.5 moles 02, 4 moles H2). An alternative possibility is of course to introduce C3H8 as an additional component in the reaction mixture, together with a reaction involving propane, e.g.")

            IObj?.Paragraphs.Add("C3H8 +502 <--> 3C02 + 4H20")

            IObj?.Paragraphs.Add("in which case the feed composition is chosen as the no-vector.")

            IObj?.Paragraphs.Add("The substitution of the mole vector enables us to formulate the equilibrium calculation in terms of the R independent variable <mi>\zeta _k</mi>, i.e.,")

            IObj?.Paragraphs.Add("<m>\min G=\min G(\mathbf{n}(\zeta _k))</m>")

            IObj?.Paragraphs.Add("At equilibrium the derivatives of G with respect to the reaction extents must equal zero")

            IObj?.Paragraphs.Add("<m>\frac{\partial G}{\partial \zeta_k}=\sum\limits_{i=1}^{C}{\frac{\partial G}{\partial n_i}\frac{\partial n_i}{\partial \zeta_k}}=\sum\limits_{i=1}^{C}{\mu_iE_{i,k}}=0,\space k=1,2,...,R</m>")

            IObj?.Paragraphs.Add("Any suitable procedure can be used for G, and the approach based on reaction extents can be used without essential differences for ideal as well as for non-ideal mixtures.")

            IObj?.Paragraphs.Add("The reaction extent approach is best suited for problems where the number of independent reactions is small, or where the mixture is highly nonideal.")

            IObj?.Paragraphs.Add("<h2>DWSIM Procedure</h2>")

            IObj?.Paragraphs.Add("DWSIM uses an inner loop to converge the reaction extents using information about the equilibrium constant for each reaction,")

            IObj?.Paragraphs.Add("<m>\ln K = \prod{basis^\nu}</m>")

            IObj?.Paragraphs.Add("where K is the equilibrium constant, 'basis' is the reaction basis for each reaction compound (i.e. activity, fugacity, partial pressure, etc.), and <mi>\nu</mi> is the stoichiometric coefficient.")

            IObj?.Paragraphs.Add("<h2>Calculated Parameters</h2>")

            _IObj = IObj

            Dim i, j As Integer

            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)
            If Me.ReactionExtents Is Nothing Then Me.ReactionExtents = New Dictionary(Of String, Double)
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
            ims.PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag
            ims.SetPropertyPackage(PropertyPackage)

            For Each comp In ims.Phases(0).Compounds.Values
                If comp.ConstantProperties.IG_Enthalpy_of_Formation_25C = 0.0 And comp.ConstantProperties.OriginalDB <> "ChemSep" Then
                    If FlowSheet IsNot Nothing Then
                        FlowSheet.ShowMessage(String.Format("Enthalpy of Formation data for compound '{0}' is missing or equal to 0, may impact equilibrium/composition calculations.", comp.Name), IFlowsheet.MessageType.Warning)
                    End If
                End If
                If comp.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C = 0.0 And comp.ConstantProperties.OriginalDB <> "ChemSep" Then
                    If FlowSheet IsNot Nothing Then
                        FlowSheet.ShowMessage(String.Format("Gibbs Energy of Formation data for compound '{0}' is missing or equal to 0, may impact equilibrium/composition calculations.", comp.Name), IFlowsheet.MessageType.Warning)
                    End If
                End If
            Next

            'Reactants Enthalpy (kJ/kg * kg/s = kW) (ISOTHERMIC)
            Dim Hr0 As Double
            Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

            Dim tmp As IFlashCalculationResult

            'Copy results to upstream MS
            Dim xl, xv, H, S, wtotalx, wtotaly As Double
            pp.CurrentMaterialStream = ims

            T0 = ims.Phases(0).Properties.temperature.GetValueOrDefault
            ims.Phases(0).Properties.pressure -= DeltaP.GetValueOrDefault
            P = ims.Phases(0).Properties.pressure.GetValueOrDefault
            P0 = 101325

            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    T = T0 'initial value only, final value will be calculated by an iterative procedure
                Case OperationMode.Isothermic
                    T = T0
                Case OperationMode.OutletTemperature
                    T = OutletTemperature
            End Select

            'check active reactions (equilibrium only) in the reaction set
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                If FlowSheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Equilibrium And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    Me.ReactionExtents.Add(rxnsb.ReactionID, 0)
                End If
            Next

            pp.CurrentMaterialStream = ims
            ppr.CurrentMaterialStream = ims

            'initial estimates for reaction extents

            tms = ims.Clone()
            tms.SetFlowsheet(ims.FlowSheet)

            Me.ComponentConversions.Clear()
            Me.ComponentIDs = New List(Of String)

            'r: number of reactions
            'c: number of components
            'i,j: iterators

            i = 0
            For Each rxid As String In Me.Reactions
                rx = FlowSheet.Reactions(rxid)
                If Not rx.Components.ContainsKey(rx.BaseReactant) Then
                    Throw New Exception("No base reactant defined for reaction '" + rx.Name + "'.")
                End If
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

            If Not AlternateBoundsInitializer Then

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

            Else

                Dim nvars As New List(Of Double)
                Dim pvars As New List(Of Double)

                i = 0
                For Each rxid As String In Me.Reactions
                    nvars.Clear()
                    pvars.Clear()
                    rx = FlowSheet.Reactions(rxid)
                    For Each comp As ReactionStoichBase In rx.Components.Values
                        If comp.StoichCoeff < 0 Then pvars.Add(-N0(comp.CompName) / comp.StoichCoeff)
                        If comp.StoichCoeff > 0 Then nvars.Add(-N0(comp.CompName) / comp.StoichCoeff)
                    Next
                    lbound(i) = nvars.Max
                    ubound(i) = pvars.Min
                    i += 1
                Next

            End If

            Dim m As Integer = 0

            Dim REx(r) As Double

            If UsePreviousReactionExtents And PreviousReactionExtents.Count > 0 Then
                REx = PreviousReactionExtents.Values.ToArray()
            Else
                For i = 0 To r
                    REx(i) = lbound(i) + ReactionExtentsInitializer * (ubound(i) - lbound(i))
                Next
            End If

            IObj?.Paragraphs.Add(String.Format("Initial Estimates for Reaction Extents: {0}", REx.ToMathArrayString))

            Dim g0, g1 As Double

            Dim REx0(REx.Length - 1) As Double

            IObj?.SetCurrent

            g0 = FunctionValue2G(REx0)

            IObj?.SetCurrent

            IObj?.Paragraphs.Add(String.Format("Initial Gibbs Energy: {0}", g0))

            Me.InitialGibbsEnergy = g0

            Dim CalcFinished As Boolean = False

            Dim TLast As Double = T0 'remember T for iteration loops
            Dim cnt As Integer = 0

            Do

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                _IObj = IObj2

                Inspector.Host.CheckAndAdd(IObj2, "", "Calculate", "Equilibrium Reactor Convergence Temperature Loop Iteration #" & cnt, "", True)

                'solve using newton's method

                Dim fx(r), dfdx(r, r), dx(r), x(r) As Double

                Dim niter As Integer

                x = REx
                niter = 0
                NoPenVal = False
                Do

                    IObj2?.SetCurrent

                    Dim IObj3 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    _IObj = IObj3

                    Inspector.Host.CheckAndAdd(IObj3, "", "Calculate", "Equilibrium Reactor Reaction Extents Convergence Loop Iteration #" & niter, "", True)

                    IObj3?.SetCurrent

                    IObj3?.Paragraphs.Add(String.Format("Tentative Reaction Extents: {0}", x.ToMathArrayString))

                    fx = Me.FunctionValue2N(x)

                    IObj3?.SetCurrent

                    IObj3?.Paragraphs.Add(String.Format("Error Function Values: {0}", fx.ToMathArrayString))

                    If AbsSum(fx) < InternalLoopTolerance Then Exit Do

                    IObj3?.SetCurrent

                    dfdx = Me.FunctionGradient2N(x)

                    IObj3?.Paragraphs.Add(String.Format("Reaction Extents Jacobian Matrix: {0}", dfdx.ToMathArrayString))

                    IObj3?.SetCurrent

                    Dim success As Boolean
                    success = SysLin.rsolve.rmatrixsolve(dfdx, fx, r + 1, dx)

                    If success Then
                        If niter = 0 Then
                            For i = 0 To r
                                x(i) *= 0.999
                            Next
                        ElseIf niter = 1 Then
                            For i = 0 To r
                                x(i) -= dx(i) * 0.01
                            Next
                        ElseIf niter = 2 Then
                            For i = 0 To r
                                x(i) -= dx(i) * 0.3
                            Next
                        ElseIf niter >= 3 Then
                            For i = 0 To r
                                x(i) -= dx(i)
                            Next
                        End If
                    Else
                        For i = 0 To r
                            x(i) *= 0.999
                        Next
                    End If

                    IObj3?.Paragraphs.Add(String.Format("Reaction Extents Update: {0}", dx.ToMathArrayString))

                    IObj3?.Paragraphs.Add(String.Format("Updated Reaction Extents: {0}", x.ToMathArrayString))

                    niter += 1

                    IObj3?.Close()

                Loop Until niter >= InternalLoopMaximumIterations

                If niter >= InternalLoopMaximumIterations Then Throw New Exception(FlowSheet.GetTranslatedString("Nmeromximodeiteraesa3"))

                IObj2?.SetCurrent

                _IObj = IObj2

                If ReturnPenaltyValue() > 0 Then

                    'recalculate extents to fix mass balance

                    Dim ref(r), dni(r) As Double
                    Dim vars As New List(Of OptSimplexBoundVariable)

                    i = 0
                    Do
                        vars.Add(New OptSimplexBoundVariable(REx(i), -N0.Values.Sum, N0.Values.Sum))
                        i += 1
                    Loop Until i = r + 1

                    Dim splex As New Simplex()

                    splex.MaxFunEvaluations = 100000
                    splex.Tolerance = 1.0E-20

                    NoPenVal = True

                    ref = splex.ComputeMin(Function(xi)

                                               Return FunctionValue2N(xi).AbsSqrSumY

                                           End Function, vars.ToArray)

                    REx = ref

                    If ReturnPenaltyValue() > 0 Then

                        Throw New Exception("Invalid solution: mass balance residue > 0.")

                    End If

                End If

                'reevaluate function

                tms.Phases(0).Properties.temperature = T

                g1 = FunctionValue2G(REx)

                IObj2?.SetCurrent

                Me.FinalGibbsEnergy = g1

                i = 0
                For Each r As String In Me.Reactions

                    'process reaction i

                    rx = FlowSheet.Reactions(r)

                    If rx.Tmax = 0 Then rx.Tmax = 2000

                    If T >= rx.Tmin And T <= rx.Tmax Then
                        ReactionExtents(r) = REx(i)
                    Else
                        ReactionExtents(r) = 0.0
                    End If

                    i += 1

                Next

                PreviousReactionExtents = New Dictionary(Of String, Double)(ReactionExtents)

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
                    If rx.Components(rx.BaseReactant).StoichCoeff > 0 Then
                        DHr += -rx.ReactionHeat * Me.ReactionExtents(Me.Reactions(i)) * rx.Components(rx.BaseReactant).StoichCoeff / 1000
                    Else
                        DHr += rx.ReactionHeat * Me.ReactionExtents(Me.Reactions(i)) * rx.Components(rx.BaseReactant).StoichCoeff / 1000
                    End If

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
                        subst.MolarFlow = 0.0
                    Else
                        sum1 += subst.MolarFlow.GetValueOrDefault
                    End If
                Next
                For Each subst As Compound In tms.Phases(0).Compounds.Values
                    subst.MoleFraction = subst.MolarFlow.GetValueOrDefault / sum1
                Next

                ims = tms.Clone
                ims.SetFlowsheet(tms.FlowSheet)

                IObj2?.SetCurrent

                Select Case Me.ReactorOperationMode

                    Case OperationMode.Adiabatic

                        Me.DeltaQ = 0.0#

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = Hr0 + DHr
                        Hp = Hp / ims.Phases(0).Properties.massflow.GetValueOrDefault

                        ims.Phases(0).Properties.enthalpy = Hp
                        ims.SpecType = StreamSpec.Pressure_and_Enthalpy

                        ims.Calculate(True, True)

                        TLast = T
                        T = ims.Phases(0).Properties.temperature.GetValueOrDefault
                        Me.DeltaT = T - T0

                        If Math.Abs(T - TLast) < ExternalLoopTolerance Then CalcFinished = True

                        T = 0.9 * TLast + 0.1 * T

                        ims.Phases(0).Properties.temperature = T

                    Case OperationMode.Isothermic

                        ims.SpecType = StreamSpec.Temperature_and_Pressure
                        ims.Calculate(True, True)

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                        'Heat (kW)
                        Me.DeltaQ = Hp - Hr0 - DHr

                        Me.DeltaT = 0
                        CalcFinished = True

                    Case OperationMode.OutletTemperature

                        Dim Tout As Double = Me.OutletTemperature

                        Me.DeltaT = Tout - T

                        ims.Phases(0).Properties.temperature = Tout

                        ims.SpecType = StreamSpec.Temperature_and_Pressure

                        ims.Calculate(True, True)

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                        'Heat (kW)
                        Me.DeltaQ = Hp - Hr0 - DHr
                        CalcFinished = True

                End Select

                cnt += 1

                If cnt >= ExternalLoopMaximumIterations Then Throw New Exception(FlowSheet.GetTranslatedString("Nmeromximodeiteraesa3"))

                IObj2?.Close()

            Loop Until CalcFinished

            IObj?.Paragraphs.Add(String.Format("Final Gibbs Energy: {0}", g1))

            Me.ReactionExtents.Clear()

            For Each rxid As String In Me.Reactions
                rx = FlowSheet.Reactions(rxid)
                ReactionExtents.Add(rx.ID, (N(rx.BaseReactant) - N0(rx.BaseReactant)) / rx.Components(rx.BaseReactant).StoichCoeff)
            Next

            Dim W As Double = ims.Phases(0).Properties.massflow.GetValueOrDefault

            pp.CurrentMaterialStream = ims

            IObj?.SetCurrent

            'do a flash calc (calculate final temperature/enthalpy)
            tmp = pp.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, ims.Phases(0).Properties.pressure.GetValueOrDefault, ims.Phases(0).Properties.temperature.GetValueOrDefault, 0)

            'Return New Object() {xl, xv, T, P, H, S, 1, 1, Vx, Vy}
            Dim wv, Vx(ims.Phases(0).Compounds.Count - 1), Vy(ims.Phases(0).Compounds.Count - 1), Vwx(ims.Phases(0).Compounds.Count - 1), Vwy(ims.Phases(0).Compounds.Count - 1) As Double
            xl = tmp.GetLiquidPhase1MoleFraction
            xv = tmp.GetVaporPhaseMoleFraction
            wv = tmp.GetVaporPhaseMassFraction
            T = ims.Phases(0).Properties.temperature.GetValueOrDefault
            P = ims.Phases(0).Properties.pressure.GetValueOrDefault
            H = tmp.CalculatedEnthalpy
            S = tmp.CalculatedEntropy
            Vx = tmp.GetLiquidPhase1MoleFractions
            Vy = tmp.GetVaporPhaseMoleFractions

            Dim ms As MaterialStream
            Dim cp As IConnectionPoint
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
                    .ClearAllProps()
                    .SpecType = StreamSpec.Temperature_and_Pressure
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    .Phases(0).Properties.enthalpy = H / wv
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
                    .Phases(0).Properties.massflow = W * wv
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .ClearAllProps()
                    .SpecType = StreamSpec.Temperature_and_Pressure
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    If wv < 1.0# Then .Phases(0).Properties.enthalpy = H / (1 - wv) Else .Phases(0).Properties.enthalpy = 0.0#
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
                    .Phases(0).Properties.massflow = W * (1 - wv)
                End With
            End If

            'energy stream - update energy flow value (kW)
            With GetInletEnergyStream(1)
                .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                .GraphicObject.Calculated = True
            End With

            IObj?.Close()

        End Sub

        Public Overrides Sub DeCalculate()

            Dim j As Integer

            Dim ms As MaterialStream
            Dim cp As IConnectionPoint

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

            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As Double = 0

                If prop.Contains("_") Then

                    Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                    Select Case propidx

                        Case 0
                            'PROP_HT_0	Pressure Drop
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)

                    End Select

                Else

                    Select Case prop
                        Case "Calculation Mode"
                            Select Case ReactorOperationMode
                                Case OperationMode.Adiabatic
                                    Return "Adiabatic"
                                Case OperationMode.Isothermic
                                    Return "Isothermic"
                                Case OperationMode.OutletTemperature
                                    Return "Defined Temperature"
                            End Select
                        Case "Initial Gibbs Energy"
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.InitialGibbsEnergy)
                        Case "Final Gibbs Energy"
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.FinalGibbsEnergy)
                        Case Else
                            If prop.Contains("Conversion") Then
                                Dim comp = prop.Split(": ")(0)
                                If ComponentConversions.ContainsKey(comp) Then
                                    value = ComponentConversions(comp) * 100
                                Else
                                    value = 0.0
                                End If
                            End If
                            If prop.Contains("Extent") Then
                                Dim rx = prop.Split(": ")(0)
                                Dim rx2 = FlowSheet.Reactions.Values.Where(Function(x) x.Name = rx).FirstOrDefault
                                If rx2 IsNot Nothing AndAlso ReactionExtents.ContainsKey(rx2.ID) Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, ReactionExtents(rx2.ID))
                                Else
                                    value = 0.0
                                End If
                            End If
                    End Select

                End If

                Return value

            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
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
                    proplist.Add("Calculation Mode")
                    proplist.Add("Initial Gibbs Energy")
                    proplist.Add("Final Gibbs Energy")
                    For Each item In ComponentConversions
                        proplist.Add(item.Key + ": Conversion")
                    Next
                    For Each item In ReactionExtents
                        Dim rx = FlowSheet.Reactions(item.Key)
                        proplist.Add(rx.Name + ": Extent")
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

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
            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then
                Return u0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As String = ""

                If prop.Contains("_") Then

                    Try

                        Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                        Select Case propidx

                            Case 0
                                'PROP_HT_0	Pressure Drop
                                value = su.deltaP

                        End Select

                        Return value

                    Catch ex As Exception

                        Return ""

                    End Try

                Else

                    Select Case prop
                        Case "Calculation Mode"
                            Return ""
                        Case "Initial Gibbs Energy"
                            value = su.heatflow
                        Case "Final Gibbs Energy"
                            value = su.heatflow
                        Case Else
                            If prop.Contains("Conversion") Then value = "%"
                            If prop.Contains("Extent") Then value = su.molarflow
                    End Select

                    Return value

                End If

            End If

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_ReactorConvEqGibbs With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ReactorConvEqGibbs With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.re_equi_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("EQUIL_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("EQUIL_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Dim elm As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ReactionExtents").LastOrDefault

            If Not elm Is Nothing Then
                ReactionExtents = New Dictionary(Of String, Double)
                For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "ReactionExtents").LastOrDefault.Elements
                    ReactionExtents.Add(xel2.@ID, Double.Parse(xel2.@Value, ci))
                Next
            End If

            elm = (From xel2 As XElement In data Select xel2 Where xel2.Name = "PreviousReactionExtents").LastOrDefault

            If Not elm Is Nothing Then
                PreviousReactionExtents = New Dictionary(Of String, Double)
                For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "PreviousReactionExtents").LastOrDefault.Elements
                    PreviousReactionExtents.Add(xel2.@ID, Double.Parse(xel2.@Value, ci))
                Next
            End If

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Remove(.Where(Function(e) e.Name = "ReactionExtents").FirstOrDefault)
                .Add(New XElement("ReactionExtents"))
                For Each kvp In ReactionExtents
                    .Item(.Count - 1).Add(New XElement("ReactionExtent", New XAttribute("ID", kvp.Key), New XAttribute("Value", kvp.Value.ToString(ci))))
                Next
                .Remove(.Where(Function(e) e.Name = "PreviousReactionExtents").FirstOrDefault)
                .Add(New XElement("PreviousReactionExtents"))
                For Each kvp In ReactionExtents
                    .Item(.Count - 1).Add(New XElement("ReactionExtent", New XAttribute("ID", kvp.Key), New XAttribute("Value", kvp.Value.ToString(ci))))
                Next
            End With

            Return elements

        End Function


        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            str.AppendLine("Reactor: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Calculation Parameters")
            str.AppendLine()
            str.AppendLine("    Calculation mode: " & ReactorOperationMode.ToString)
            str.AppendLine("    Pressure drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.DeltaP.GetValueOrDefault).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    str.AppendLine("    Outlet Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                Case OperationMode.Isothermic
                    str.AppendLine("    Heat added/removed: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault).ToString(numberformat, ci) & " " & su.heatflow)
                Case OperationMode.OutletTemperature
                    str.AppendLine("    Outlet Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Heat added/removed: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault).ToString(numberformat, ci) & " " & su.heatflow)
            End Select
            str.AppendLine("    Initial Gibbs Free Energy Value: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.InitialGibbsEnergy).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine("    Final Gibbs Free Energy Value: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.FinalGibbsEnergy).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine()
            str.AppendLine("Reaction Extents")
            str.AppendLine()
            If Not Me.ReactionExtents Is Nothing Then
                For Each dbl As KeyValuePair(Of String, Double) In Me.ReactionExtents
                    If dbl.Value > 0.0# Then str.AppendLine("    " & Me.GetFlowsheet.Reactions(dbl.Key).Name & ": " & dbl.Value.ToString(numberformat, ci))
                Next
            End If
            str.AppendLine()
            str.AppendLine("Compound Conversions")
            str.AppendLine()
            For Each dbl As KeyValuePair(Of String, Double) In Me.ComponentConversions
                If dbl.Value > 0 Then str.AppendLine("    " & dbl.Key & ": " & (dbl.Value * 100).ToString(numberformat, ci) & "%")
            Next
            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Equilibrium Reactor '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))


            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn, New String() {"Calculation Mode", ReactorOperationMode.ToString}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn, New String() {"Pressure Drop", SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.DeltaP.GetValueOrDefault).ToString(nf), su.deltaP}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Temperature",
                            Me.OutletTemperature.ConvertFromSI(su.temperature).ToString(nf), su.temperature}))
                Case OperationMode.Isothermic
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Heat Added/Removed",
                            Me.DeltaQ.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow}))
                Case OperationMode.OutletTemperature
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Temperature",
                            Me.OutletTemperature.ConvertFromSI(su.temperature).ToString(nf), su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Heat Added/Removed",
                            Me.DeltaQ.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Initial Gibbs Free Energy",
                    InitialGibbsEnergy.ConvertFromSI(su.enthalpy).ToString(nf),
                    su.enthalpy}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Final Gibbs Free Energy",
                    FinalGibbsEnergy.ConvertFromSI(su.enthalpy).ToString(nf),
                    su.enthalpy}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Reaction Extents"}))
            If Not Me.Conversions Is Nothing Then
                For Each dbl As KeyValuePair(Of String, Double) In Me.ReactionExtents
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {Me.GetFlowsheet.Reactions(dbl.Key).Name,
                            (dbl.Value * 100).ConvertFromSI(su.molarflow).ToString(nf), su.molarflow}))
                Next
            End If

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Compound Conversions"}))
            For Each dbl As KeyValuePair(Of String, Double) In Me.ComponentConversions
                If dbl.Value >= 0 Then list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {dbl.Key,
                            (dbl.Value * 100).ToString(nf), "%"}))
            Next

            Return list

        End Function
        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Calculation Mode") Then
                Return "Select the calculation mode of this reactor."
            ElseIf p.Equals("Pressure Drop") Then
                Return "Enter the desired pressure drop for this reactor."
            ElseIf p.Equals("Outlet Temperature") Then
                Return "If you chose 'Outlet Temperature' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace


