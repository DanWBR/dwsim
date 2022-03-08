'    Gibbs Reactor Calculation Routines 
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
Imports DWSIM.MathOps.MathEx.Common
Imports DotNumerics.Optimization
Imports DWSIM.MathOps.MathEx
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Thermodynamics
Imports scaler = DotNumerics.Scaling.Scaler
Imports DWSIM.MathOps

Namespace Reactors

    <System.Serializable()> Public Class Reactor_Gibbs

        Inherits Reactor

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

        Public Property UseIPOPTSolver As Boolean = True

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_ReactorConvEqGibbs

        Public Enum SolvingMethod

            ReactionExtents = 0
            DirectMinimization = 1

        End Enum

        Private _IObj As InspectorItem

        Private _solvemethod As SolvingMethod = SolvingMethod.DirectMinimization
        Protected m_reactionextents As Dictionary(Of String, Double)
        Private _rex_iest As New ArrayList
        Private _el_mat As Double(,) = New Double(,) {}
        Private _components As New List(Of String)
        Private _components_re As New List(Of String)
        Private _initialestimates As New List(Of Double)
        Private _elements As String() = {}
        Private _totalelements As Double() = {}
        Private _ige, _fge, _elbal As Double
        Private igcp() As Double
        Private xv_0, xl1_0, xl2_0, xs_0, fv_0, fl1_0, fl2_0, fs_0 As Double()

        Dim tmpx As Double(), tmpdx As Double()

        Dim tms As MaterialStream
        Dim N0 As New Dictionary(Of String, Double)
        Dim DN As New Dictionary(Of String, Double)
        Dim N As New Dictionary(Of String, Double)
        Dim T, T0, P, P0, Ninerts, Winerts, E(,), W0 As Double
        Dim r, c, els, comps, cnt As Integer
        Dim ims As MaterialStream

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Reactor_Gibbs()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Reactor_Gibbs)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            _components = New List(Of String)
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Compounds").LastOrDefault.Elements
                If Not _components.Contains(xel2.@ID) Then _components.Add(xel2.@ID)
            Next

            _initialestimates = New List(Of Double)
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "InitialEstimates").LastOrDefault.Elements
                _initialestimates.Add(Double.Parse(xel2.@Value, ci))
            Next

            Dim elmns As New ArrayList
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Elements").Elements
                elmns.Add(xel2.@ID)
            Next
            _elements = elmns.ToArray(Type.GetType("System.String"))

            Dim telmns As New ArrayList
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "TotalElements").Elements
                telmns.Add(Double.Parse(xel2.@ID, ci))
            Next
            _totalelements = telmns.ToArray(Type.GetType("System.Double"))

            Dim elmattext As String = (From xel As XElement In data Select xel Where xel.Name = "ElementMatrix").SingleOrDefault.Value

            Dim rows() As String = elmattext.Split(":")
            Dim n As Integer = rows.Length - 1
            Dim m As Integer = 0
            If n > 0 Then
                m = rows(0).Split(",").Length - 1
            End If
            If n > 0 And m > 0 Then
                Dim elm(n, m) As Double
                For i As Integer = 0 To n
                    For j As Integer = 0 To m
                        elm(i, j) = Double.Parse(rows(i).Split(",")(j))
                    Next
                Next
                _el_mat = elm
            End If
            Return True
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("Compounds"))
                For Each s As String In _components
                    .Item(.Count - 1).Add(New XElement("Compound", New XAttribute("ID", s)))
                Next
                .Add(New XElement("InitialEstimates"))
                For Each d As Double In _initialestimates
                    .Item(.Count - 1).Add(New XElement("Item", New XAttribute("Value", d.ToString(ci))))
                Next
                .Add(New XElement("Elements"))
                For Each s As String In _elements
                    .Item(.Count - 1).Add(New XElement("Element", New XAttribute("ID", s)))
                Next
                .Add(New XElement("TotalElements"))
                For Each d As Double In _totalelements
                    .Item(.Count - 1).Add(New XElement("Element", New XAttribute("ID", d.ToString(ci))))
                Next
                Dim elmattext As String = ""
                For i As Integer = 0 To _el_mat.GetUpperBound(0)
                    For j As Integer = 0 To _el_mat.GetUpperBound(1)
                        elmattext += _el_mat(i, j).ToString(ci) + ","
                    Next
                    elmattext = elmattext.TrimEnd(",") + ":"
                Next
                elmattext = elmattext.TrimEnd(":")
                .Add(New XElement("ElementMatrix", elmattext))
            End With

            Return elements

        End Function

#Region "Properties"

        Public Property InitializeFromPreviousSolution As Boolean = True

        Public Property PreviousSolution As New List(Of Double)

        Public Property EnableDamping As Boolean = True

        Public Property DampingLowerLimit As Double = 0.001

        Public Property DampingUpperLimit As Double = 2.0

        Public Property DerivativePerturbation As Double = 0.0001

        Public MaximumInternalIterations As Integer = 1000

        Public InternalTolerance As Double = 1.0E-20

        Public Property LagrangeCoeffsEstimationTemperature As Double = 1000.0

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

        Public Property ElementBalance As Double = 0.0

        Public ReadOnly Property ReactionExtents() As Dictionary(Of String, Double)
            Get
                Return Me.m_reactionextents
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

        Public Property ComponentIDs() As List(Of String)
            Set(value As List(Of String))
                _components = value
            End Set
            Get
                Return _components
            End Get
        End Property

        Public Property ComponentIDs_RE() As List(Of String)
            Set(value As List(Of String))
                _components_re = value
            End Set
            Get
                Return _components_re
            End Get
        End Property

        Public Property InitialEstimates() As List(Of Double)
            Set(value As List(Of Double))
                _initialestimates = value
            End Set
            Get
                If _initialestimates Is Nothing Then _initialestimates = New List(Of Double)
                Return _initialestimates
            End Get
        End Property

        Public Property ElementMatrix() As Double(,)
            Get
                Return _el_mat
            End Get
            Set(ByVal value As Double(,))
                _el_mat = value
            End Set
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

        Private Function FunctionValue2G2(ByVal x() As Double, T As Double) As Double

            Dim i As Integer

            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage

            i = 0
            For Each s As String In N.Keys
                DN(s) = x(i) - N0(s)
                i += 1
            Next

            i = 0
            For Each s As String In DN.Keys
                N(s) = x(i)
                'If x(i) < 0 Then N(s) = 0
                i += 1
            Next

            Dim fw(comps), fm(comps), sumfm, sum1, sumn, sumw As Double

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

            Dim multiphase As Boolean = False

            tms.Calculate(True, False)

            pp.CurrentMaterialStream = tms

            Dim vfrac, l1frac, l2frac, sfrac As Double

            vfrac = tms.Phases(2).Properties.molarfraction.GetValueOrDefault()
            l1frac = tms.Phases(3).Properties.molarfraction.GetValueOrDefault()
            l2frac = tms.Phases(4).Properties.molarfraction.GetValueOrDefault()
            sfrac = tms.Phases(7).Properties.molarfraction.GetValueOrDefault()

            Dim P = tms.GetPressure()
            Dim P0 = 101325

            Dim gf = 0.0
            Dim t1, t2, t3 As Double

            If vfrac > 0 Then
                Dim xv = tms.GetPhaseComposition(2)
                Dim fugc = pp.DW_CalcFugCoeff(xv, T, P, PropertyPackages.State.Vapor)
                i = 0
                For Each s As Compound In tms.Phases(0).Compounds.Values
                    t1 = pp.AUX_DELGF_T(298.15, T, s.Name) * s.ConstantProperties.Molar_Weight
                    If s.MoleFraction.GetValueOrDefault > 0 Then
                        t2 = Log(s.MoleFraction.GetValueOrDefault())
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    gf += s.MoleFraction.GetValueOrDefault() * (t1 + t2 + t3) * 8.314 * T
                    i += 1
                Next
            End If
            If l1frac > 0 Then
                Dim xv = tms.GetPhaseComposition(3)
                Dim fugc = pp.DW_CalcFugCoeff(xv, T, P, PropertyPackages.State.Liquid)
                i = 0
                For Each s As Compound In tms.Phases(0).Compounds.Values
                    t1 = pp.AUX_DELGF_T(298.15, T, s.Name) * s.ConstantProperties.Molar_Weight
                    If s.MoleFraction.GetValueOrDefault > 0 Then
                        t2 = Log(s.MoleFraction.GetValueOrDefault())
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    gf += s.MoleFraction.GetValueOrDefault() * (t1 + t2 + t3) * 8.314 * T
                    i += 1
                Next
            End If
            If l2frac > 0 Then
                Dim xv = tms.GetPhaseComposition(4)
                Dim fugc = pp.DW_CalcFugCoeff(xv, T, P, PropertyPackages.State.Liquid)
                i = 0
                For Each s As Compound In tms.Phases(0).Compounds.Values
                    t1 = pp.AUX_DELGF_T(298.15, T, s.Name) * s.ConstantProperties.Molar_Weight
                    If s.MoleFraction.GetValueOrDefault > 0 Then
                        t2 = Log(s.MoleFraction.GetValueOrDefault())
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    gf += s.MoleFraction.GetValueOrDefault() * (t1 + t2 + t3) * 8.314 * T
                    i += 1
                Next
            End If
            If sfrac > 0 Then
                Dim fugc = pp.DW_CalcSolidFugCoeff(T, P)
                i = 0
                For Each s As Compound In tms.Phases(0).Compounds.Values
                    t1 = pp.AUX_DELGF_T(298.15, T, s.Name) * s.ConstantProperties.Molar_Weight
                    If s.MoleFraction.GetValueOrDefault > 0 Then
                        t2 = Log(s.MoleFraction.GetValueOrDefault())
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    gf += s.MoleFraction.GetValueOrDefault() * (t1 + t2 + t3) * 8.314 * T
                    i += 1
                Next
            End If

            FlowSheet.CheckStatus()

            Return gf * sumfm / 1000.0 / 1000.0 / 1000.0

        End Function

        Private Function FunctionValue2G(ByVal x() As Double) As Double

            Dim i As Integer

            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage

            i = 0
            For Each s As String In N.Keys
                DN(s) = x(i) - N0(s)
                i += 1
            Next

            i = 0
            For Each s As String In DN.Keys
                N(s) = x(i)
                If x(i) < 0 Then N(s) = 0
                i += 1
            Next

            Dim fw(comps), fm(comps), sumfm, sum1, sumn, sumw As Double

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
            tms.SpecType = StreamSpec.Temperature_and_Pressure
            _IObj?.SetCurrent
            tms.Calculate(True, True)
            pp.CurrentMaterialStream = tms

            Dim G = tms.Phases(0).Properties.gibbs_free_energy.GetValueOrDefault * tms.Phases(0).Properties.molecularWeight.GetValueOrDefault * tms.Phases(0).Properties.molarflow.GetValueOrDefault / 1000
            Dim Gf = pp.AUX_DELGFM_T(pp.AUX_CONVERT_MOL_TO_MASS(tms.GetOverallComposition()), tms.GetTemperature()) * tms.GetOverallMolecularWeight() * 8.314 * tms.GetTemperature() * tms.Phases(0).Properties.molarflow.GetValueOrDefault / 1000

            Return Gf + G

        End Function

#End Region

#Region "Auxiliary Subs"

        Public Sub CheckCompoundIDs()

            Dim ids = ComponentIDs.ToList

            For Each id In ids
                If Not FlowSheet.SelectedCompounds.ContainsKey(id) Then
                    ComponentIDs.Remove(id)
                End If
            Next

        End Sub

        Public Sub CreateElementMatrix()

            Dim ims As MaterialStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)

            CheckCompoundIDs()

            N0.Clear()

            For Each cname As String In Me.ComponentIDs
                N0.Add(cname, ims.Phases(0).Compounds(cname).MolarFlow)
            Next

            Dim elements As New List(Of String)

            For Each sn As String In Me.ComponentIDs
                For Each el As String In Me.FlowSheet.SelectedCompounds(sn).Elements.Keys
                    If Not elements.Contains(el) Then elements.Add(el)
                Next
            Next

            Me.Elements = elements.ToArray()

            els = Me.Elements.Length - 1
            c = Me.ComponentIDs.Count - 1

            ReDim Me.ElementMatrix(els, c)
            ReDim Me.TotalElements(els)

            Dim sum_e As Double

            For i = 0 To els
                sum_e = 0
                For j = 0 To c
                    If Me.FlowSheet.SelectedCompounds(Me.ComponentIDs(j)).Elements.ContainsKey(Me.Elements(i)) Then
                        Me.ElementMatrix(i, j) = Me.FlowSheet.SelectedCompounds(Me.ComponentIDs(j)).Elements(Me.Elements(i))
                    Else
                        Me.ElementMatrix(i, j) = 0
                    End If
                    sum_e += N0(Me.ComponentIDs(j)) * Me.ElementMatrix(i, j)
                Next
                Me.TotalElements(i) = sum_e
            Next

        End Sub

#End Region

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

            Me._components = New List(Of String)
            ReDim ElementMatrix(0, 0)

        End Sub

        Public Overrides Sub Validate()

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac10"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac11"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac11"))
            End If

        End Sub

        Public Overrides Sub RunDynamicModel()

            Calculate()

        End Sub

        Private LagrangeFactor As Double = 1000.0

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Calculate_GibbsMin()

        End Sub

        Public Sub Calculate_GibbsMin()

            Me.Validate()

            Dim IObj As InspectorItem = Host.GetNewInspectorItem()

            Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent

            IObj?.Paragraphs.Add("The calculation of chemical equilibrium at specified temperature and pressure is in many ways similar to the calculation of phase equilibrium. In both cases the equilibrium state corresponds to a global minimum of the Gibbs energy subject to a set of material balance constraints.")

            IObj?.Paragraphs.Add("<h3>Chemical reaction equilibrium</h3>")

            IObj?.Paragraphs.Add("In phase equilibrium calculations for a given feed at specified temperature and pressure a material balance must be satisfied for each component in the mixture, the total amount in the combined product phases being identical to that in the feed. When chemical reactions occur, additional degrees of freedom are available, resulting in a set of material balance constraints, which is smaller than the number of components in the mixture.")

            IObj?.Paragraphs.Add("The mixture composition at chemical equilibrium at constant T and p satisfies the condition of minimum Gibbs energy,")

            IObj?.Paragraphs.Add("<m>\min G = \min \sum\limits_{i=1}^{C}{n_i\mu _i} </m>")

            IObj?.Paragraphs.Add("subject to a set of M < C material balance constraints. In addition we must require that")

            IObj?.Paragraphs.Add("<m>n_i \geq 0, i=1,2,...,C</m>")

            IObj?.Paragraphs.Add("The alternative formulation of the constraints Is based on the requirement of conservation 
                                of chemical elements. A key concept in this approach is the formula matrix for the reaction 
                                components. In this matrix, <mi>A_{ji}</mi> Is the formula content of element <mi>j</mi> in component <mi>i</mi>.")

            IObj?.Paragraphs.Add("The element conservation constraints can be written")

            IObj?.Paragraphs.Add("<m>An = b</m>")

            IObj?.Paragraphs.Add("where <mi>b_k</mi> is the total amount of element <mi>k</mi> in the reaction mixture.")

            IObj?.Paragraphs.Add("<h2>Solving Method</h2>")

            IObj?.Paragraphs.Add("DWSIM solves the multiphase Gibbs minimization problem,")

            IObj?.Paragraphs.Add("<m>\min G = \min \sum\limits_{i=1}^{C}{n_i\mu _i}</m>")

            IObj?.Paragraphs.Add("using the reactive compound overall molar fractions as the minimization variables,
                                while obeying the element and mass conservation constraints.")

            IObj?.Paragraphs.Add("IPOPT (or an external solver) is used to minimize <mi>G</mi>. For each solver iteration, a multiphase flash calculation is performed
                                using the current tentative molar fractions, and the <mi>G</mi> for each phase is calculated using the
                                compound fugacities as provided by the currently selected Property Package. Total <mi>G</mi> is calculated
                                as the sum of the phase <mi>G</mi>s multiplied by their amounts.")

            IObj?.Paragraphs.Add("Chemical potentials are calculated from fugacities using the following relationship:")

            IObj?.Paragraphs.Add("<m>\mu _{ik} = \mu _0+RT\ln {f_{ik}}+RT\ln {x_{ik}}</m>")

            Dim i, j As Integer

            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)
            If Me.ReactionExtents Is Nothing Then Me.m_reactionextents = New Dictionary(Of String, Double)
            If Me.ComponentConversions Is Nothing Then Me.m_componentconversions = New Dictionary(Of String, Double)

            Me.Reactions.Clear()
            Me.ReactionExtents.Clear()
            Me.ReactionsSequence.Clear()
            Me.Conversions.Clear()
            Me.ComponentConversions.Clear()
            Me.DeltaQ = 0
            Me.DeltaT = 0

            ims = GetInletMaterialStream(0).Clone
            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage
            Dim ppr As New PropertyPackages.RaoultPropertyPackage()

            ims.SetFlowsheet(Me.FlowSheet)
            ims.PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag
            ims.SetPropertyPackage(PropertyPackage)

            Dim compremoved As Boolean = False

            For Each comp In ims.Phases(0).Compounds.Values
                If comp.ConstantProperties.IG_Enthalpy_of_Formation_25C = 0.0 And comp.ConstantProperties.OriginalDB <> "ChemSep" Then
                    If Me.ComponentIDs.Contains(comp.Name) Then
                        FlowSheet.ShowMessage(String.Format("Enthalpy of Formation data for compound '{0}' is missing or equal to 0. It will be removed from the reactive compounds list.", comp.Name), IFlowsheet.MessageType.Warning)
                        Me.ComponentIDs.Remove(comp.Name)
                        compremoved = True
                    End If
                End If
                If comp.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C = 0.0 And comp.ConstantProperties.OriginalDB <> "ChemSep" Then
                    If Me.ComponentIDs.Contains(comp.Name) Then
                        FlowSheet.ShowMessage(String.Format("Gibbs Energy of Formation data for compound '{0}' is missing or equal to 0. It will be removed from the reactive compounds list.", comp.Name), IFlowsheet.MessageType.Warning)
                        Me.ComponentIDs.Remove(comp.Name)
                        compremoved = True
                    End If
                End If
            Next

            If compremoved Then Me.CreateElementMatrix()

            'Reactants Enthalpy (kJ/kg * kg/s = kW) (ISOTHERMIC)
            Dim Hr0, Hr0i As Double
            Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

            Dim tmp As IFlashCalculationResult
            Dim xl, xv, xs, H, S As Double

            pp.CurrentMaterialStream = ims

            'read temperature and pressure from inlet stream.

            T0 = ims.Phases(0).Properties.temperature.GetValueOrDefault

            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic, OperationMode.Isothermic
                    T = T0
                Case OperationMode.OutletTemperature
                    T = OutletTemperature
            End Select

            Hr0i = pp.RET_Hid(298.15, T, pp.RET_VMOL(PropertyPackages.Phase.Mixture)) * ims.Phases(0).Properties.massflow.GetValueOrDefault

            ims.Phases(0).Properties.temperature = T

            ims.Phases(0).Properties.pressure -= DeltaP.GetValueOrDefault
            P = ims.Phases(0).Properties.pressure.GetValueOrDefault
            P0 = 101325
            W0 = ims.GetMassFlow()

            Dim e, c As Integer
            e = Me.Elements.Length - 1
            c = Me.ComponentIDs.Count - 1
            els = e
            comps = c

            If e < 0 Then Throw New Exception("The Element Matrix is not defined.")
            If c < 0 Then Throw New Exception("The list of reacting compounds is not defined.")

            tms = ims.Clone()
            tms.SetFlowsheet(ims.FlowSheet)

            Dim ids = tms.Phases(0).Compounds.Keys.ToList()

            Dim te(els) As Double

            Me.TotalElements = te

            Dim fm0(c), N0tot, W0tot, wm0 As Double

            N0.Clear()
            DN.Clear()
            N.Clear()

            'store initial values for molar flows

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

            'calculate the total amount of each element in the mixture and store values in a vector

            Dim sum_e As Double
            For i = 0 To els
                sum_e = 0
                For j = 0 To c
                    sum_e += N0(Me.ComponentIDs(j)) * Me.ElementMatrix(i, j)
                Next
                If sum_e < 1.0E-20 Then
                    Throw New Exception("The Element Matrix is invalid. Check reacting components with zero flow at the reactor inlet.")
                End If
                Me.TotalElements(i) = sum_e
            Next

            Me.ComponentConversions.Clear()
            For Each s1 As String In Me.ComponentIDs
                Me.ComponentConversions.Add(s1, 0)
            Next

            Dim CProps = DirectCast(PropertyPackage, PropertyPackages.PropertyPackage).DW_GetConstantProperties()

            Dim keys = N.Keys.ToArray()

            Dim ival(N.Count - 1), lbo(N.Count - 1), ubo(N.Count - 1), NFv(N.Count - 1) As Double
            For i = 0 To N.Count - 1
                lbo(i) = 0.0000000001
                ubo(i) = W0tot / CProps(i).Molar_Weight * 1000
                If InitializeFromPreviousSolution Then
                    Try
                        ival(i) = InitialEstimates(i)
                    Catch ex As Exception
                        InitializeFromPreviousSolution = False
                        Throw New Exception("invalid initial estimates.")
                    End Try
                Else
                    ival(i) = N0(Me.ComponentIDs(i))
                End If
                If ival(i) < 0.000000001 Then ival(i) = 0.000000001
            Next

            Dim ipo As New Optimization.IPOPTSolver
            ipo.MaxIterations = MaximumInternalIterations
            ipo.Tolerance = InternalTolerance
            ipo.ReturnLowestObjFuncValue = False

            Dim esolv As IExternalNonLinearMinimizationSolver = Nothing
            If FlowSheet.ExternalSolvers.ContainsKey(ExternalSolverID) Then
                esolv = FlowSheet.ExternalSolvers(ExternalSolverID)
            End If

            Dim g0 = FunctionValue2G(N.Values.ToArray)

            Me.InitialGibbsEnergy = g0

            IObj?.Paragraphs.Add(String.Format("Initial Gibbs Energy (kW): {0}", g0))

            IObj?.Paragraphs.Add(String.Format("Initial Overall Composition: {0}", ival.NormalizeY().ToMathArrayString()))

            Dim CalcFinished As Boolean = False

            cnt = 0

            IObj?.Paragraphs.Add("<h2>Iteration History</h2>")

            IObj?.Paragraphs.Add("<table style='width:100%'><tr>
                                <th>Iteration</th>
                                <th>Objective Function Value</th>
                                <th>Element Balance Residue</th>
                                <th>Mass Balance Residue</th>
                              </tr>")

            Dim gfunc = Function(Tx)

                            T = Tx

                            If cnt > 0 Then
                                For i = 0 To N.Count - 1
                                    ival(i) = N(Me.ComponentIDs(i))
                                    If ival(i) < 0.0000000001 Then ival(i) = 0.0000000001
                                Next
                            End If

                            Dim ebal As Double = 0.0
                            Dim errval As Double = 0.0
                            Dim wbal As Double = 0.0

                            Dim icount As Integer = 0

                            If esolv IsNot Nothing Then

                                NFv = esolv.Solve(Function(xn)
                                                      Dim gval = FunctionValue2G2(xn, T)
                                                      Dim ebal_i As Double
                                                      ebal = 0.0
                                                      For i = 0 To els
                                                          ebal_i = 0
                                                          For j = 0 To c
                                                              ebal_i += xn(j) * Me.ElementMatrix(i, j)
                                                          Next
                                                          ebal += ((TotalElements(i) - ebal_i) / TotalElements(i)) ^ 2
                                                      Next
                                                      icount += 1
                                                      wbal = ((tms.GetMassFlow() - W0tot) / W0tot) ^ 2
                                                      errval = Exp(gval) + wbal * 100 + ebal * 100
                                                      IObj?.SetCurrent()
                                                      IObj?.Paragraphs.Add(String.Format("<tr><td>{0}</td><td>{1}</td><td>{2}</td><td>{3}</td></tr>",
                                                                            icount, errval, ebal, wbal))
                                                      Return errval
                                                  End Function, Nothing, Nothing,
                                                          ival, lbo, ubo, MaximumInternalIterations, InternalTolerance)

                            Else

                                If UseIPOPTSolver Then

                                    NFv = ipo.Solve(Function(xn)
                                                        Dim gval = FunctionValue2G2(xn, T)
                                                        Dim ebal_i As Double
                                                        ebal = 0.0
                                                        For i = 0 To els
                                                            ebal_i = 0
                                                            For j = 0 To c
                                                                ebal_i += xn(j) * Me.ElementMatrix(i, j)
                                                            Next
                                                            ebal += ((TotalElements(i) - ebal_i) / TotalElements(i)) ^ 2
                                                        Next
                                                        icount += 1
                                                        wbal = ((tms.GetMassFlow() - W0tot) / W0tot) ^ 2
                                                        errval = Exp(gval) + wbal * 100 + ebal * 100
                                                        IObj?.SetCurrent()
                                                        IObj?.Paragraphs.Add(String.Format("<tr><td>{0}</td><td>{1}</td><td>{2}</td><td>{3}</td></tr>",
                                                                            icount, errval, ebal, wbal))
                                                        Return errval
                                                    End Function, Nothing, ival, lbo, ubo)

                                Else

                                    Dim slv As New BFGSBMinimizer
                                    slv.MaxIterations = MaximumInternalIterations
                                    slv.Tolerance = InternalTolerance

                                    NFv = slv.Solve(Function(xn)
                                                        Dim gval = FunctionValue2G2(xn, T)
                                                        Dim ebal_i As Double
                                                        ebal = 0.0
                                                        For i = 0 To els
                                                            ebal_i = 0
                                                            For j = 0 To c
                                                                ebal_i += xn(j) * Me.ElementMatrix(i, j)
                                                            Next
                                                            ebal += ((TotalElements(i) - ebal_i) / TotalElements(i)) ^ 2
                                                        Next
                                                        icount += 1
                                                        wbal = ((tms.GetMassFlow() - W0tot) / W0tot) ^ 2
                                                        errval = Exp(gval) + wbal * 100 + ebal * 100
                                                        IObj?.SetCurrent()
                                                        IObj?.Paragraphs.Add(String.Format("<tr><td>{0}</td><td>{1}</td><td>{2}</td><td>{3}</td></tr>",
                                                                            icount, errval, ebal, wbal))
                                                        Return errval
                                                    End Function, Nothing, ival, lbo, ubo)


                                End If


                            End If

                            IObj?.Paragraphs.Add("</table>")

                            InitialEstimates = NFv.ToList()

                            For i = 0 To N.Count - 1
                                N(keys(i)) = NFv(i)
                                DN(keys(i)) = N(keys(i)) - N0(keys(i))
                                i += 1
                            Next

                            ElementBalance = ebal

                            'calculate component conversions.

                            For Each sb As Compound In ims.Phases(0).Compounds.Values
                                If Me.ComponentConversions.ContainsKey(sb.Name) Then
                                    Me.ComponentConversions(sb.Name) = -DN(sb.Name) / N0(sb.Name)
                                End If
                            Next

                            'reaction heat

                            Dim DHr As Double = 0

                            For Each sb As Compound In ims.Phases(0).Compounds.Values
                                If N0.ContainsKey(sb.Name) Then
                                    DHr += sb.ConstantProperties.IG_Enthalpy_of_Formation_25C * sb.ConstantProperties.Molar_Weight * DN(sb.Name) / 1000
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

                            Me.PropertyPackage.CurrentMaterialStream = ims

                            Dim Qerror = 0.0

                            Select Case Me.ReactorOperationMode

                                Case OperationMode.Adiabatic

                                    Me.DeltaQ = 0.0#

                                    ims.SetTemperature(T)
                                    ims.SpecType = StreamSpec.Temperature_and_Pressure
                                    ims.Calculate(True, True)

                                    'Products Enthalpy (kJ/kg * kg/s = kW)
                                    Dim Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                                    Qerror = Hp - Hr0 + DHr

                                    Me.DeltaT = T - T0

                                    CalcFinished = True

                                Case OperationMode.Isothermic

                                    ims.SpecType = StreamSpec.Temperature_and_Pressure
                                    ims.Calculate(True, True)

                                    'Products Enthalpy (kJ/kg * kg/s = kW)
                                    Dim Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                                    'Heat (kW)
                                    Me.DeltaQ = Hp - Hr0 + DHr

                                    Me.DeltaT = 0

                                    CalcFinished = True

                                Case OperationMode.OutletTemperature

                                    Dim Tout As Double = Me.OutletTemperature

                                    Me.DeltaT = Tout - T

                                    ims.Phases(0).Properties.temperature = Tout

                                    ims.SpecType = StreamSpec.Temperature_and_Pressure

                                    ims.Calculate(True, True)

                                    'Products Enthalpy (kJ/kg * kg/s = kW)
                                    Dim Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                                    'Heat (kW)
                                    Me.DeltaQ = Hp - Hr0 + DHr

                                    CalcFinished = True

                            End Select

                            cnt += 1

                            Return Qerror ^ 2

                        End Function

            If ReactorOperationMode = OperationMode.Adiabatic Then
                Dim brent As New BrentOpt.BrentMinimize
                Dim tryagain As Boolean = True
                Try
                    brent.brentoptimize2(200, T * 3 - 200, 0.01, Function(Tx)
                                                                     Return gfunc.Invoke(Tx)
                                                                 End Function)
                    tryagain = False
                Catch ex As Exception
                End Try
                If tryagain Then
                    brent.brentoptimize2(200, T * 2 - 200, 0.01, Function(Tx)
                                                                     Return gfunc.Invoke(Tx)
                                                                 End Function)
                End If
            Else
                gfunc.Invoke(T)
            End If

            'this call to FunctionValue2G returns the final gibbs energy in kJ/s.

            tms.SetTemperature(T)

            Dim g1 = FunctionValue2G(N.Values.ToArray)

            Me.FinalGibbsEnergy = g1

            IObj?.Paragraphs.Add(String.Format("Final Gibbs Energy (kW): {0}", g1))

            IObj?.Paragraphs.Add(String.Format("Final Overall Composition: {0}", ims.GetOverallComposition().ToMathArrayString()))

            Dim W As Double = ims.Phases(0).Properties.massflow.GetValueOrDefault

            pp.CurrentMaterialStream = ims

            'do a flash calc (calculate final temperature/enthalpy)
            tmp = pp.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, ims.Phases(0).Properties.pressure.GetValueOrDefault, ims.Phases(0).Properties.temperature.GetValueOrDefault, 0)

            'Return New Object() {xl, xv, T, P, H, S, 1, 1, Vx, Vy}
            Dim wl, wv, ws, Ki(ims.Phases(0).Compounds.Count - 1), Vx(ims.Phases(0).Compounds.Count - 1), Vy(ims.Phases(0).Compounds.Count - 1), Vs(ims.Phases(0).Compounds.Count - 1), Vwx(ims.Phases(0).Compounds.Count - 1), Vwy(ims.Phases(0).Compounds.Count - 1), Vws(ims.Phases(0).Compounds.Count - 1) As Double
            xl = tmp.GetLiquidPhase1MoleFraction
            xv = tmp.GetVaporPhaseMoleFraction
            xs = tmp.GetSolidPhaseMoleFraction
            T = ims.Phases(0).Properties.temperature.GetValueOrDefault
            P = ims.Phases(0).Properties.pressure.GetValueOrDefault
            H = tmp.CalculatedEnthalpy
            S = tmp.CalculatedEntropy
            Vx = tmp.GetLiquidPhase1MoleFractions
            Vy = tmp.GetVaporPhaseMoleFractions
            Vs = tmp.GetSolidPhaseMoleFractions
            Vwx = tmp.GetLiquidPhase1MassFractions
            Vwy = tmp.GetVaporPhaseMassFractions
            Vws = tmp.GetSolidPhaseMassFractions
            wl = tmp.GetLiquidPhase1MassFraction
            wv = tmp.GetVaporPhaseMassFraction
            ws = tmp.GetSolidPhaseMassFraction
            Ki = tmp.Kvalues.ToArray

            OutletTemperature = T

            Dim ms As MaterialStream
            Dim cp As IConnectionPoint

            Dim ids2 = ims.PropertyPackage.RET_VNAMES().ToList

            Dim Hv As Double

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .ClearAllProps()
                    .SpecType = StreamSpec.Temperature_and_Pressure
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        If xv = 0.0# Then
                            comp.MoleFraction = 0.0#
                            comp.MassFraction = 0.0#
                        Else
                            comp.MoleFraction = Vy(ids2.IndexOf(comp.Name))
                            comp.MassFraction = Vwy(ids2.IndexOf(comp.Name))
                        End If
                    Next
                    .PropertyPackage.CurrentMaterialStream = ms
                    Hv = .PropertyPackage.DW_CalcEnthalpy(ms.GetOverallComposition(), T, P, PropertyPackages.State.Vapor)
                    .Phases(0).Properties.enthalpy = Hv
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
                    For Each comp In .Phases(0).Compounds.Values
                        If (1 - xv) = 0.0# Then
                            comp.MoleFraction = 0.0#
                            comp.MassFraction = 0.0#
                        Else
                            comp.MoleFraction = (Vx(ids2.IndexOf(comp.Name)) * xl + Vs(ids2.IndexOf(comp.Name)) * xs) / (1 - xv)
                            comp.MassFraction = (Vwx(ids2.IndexOf(comp.Name)) * wl + Vws(ids2.IndexOf(comp.Name)) * ws) / (1 - wv)
                        End If
                    Next
                    .Phases(0).Properties.enthalpy = (H - Hv * wv) / (1 - wv)
                    .Phases(0).Properties.massflow = W * (1 - wv)
                End With
            End If

            If GetInletEnergyStream(1) IsNot Nothing Then
                'energy stream - update energy flow value (kW)
                With GetInletEnergyStream(1)
                    .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                    .GraphicObject.Calculated = True
                End With
            End If

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
                            'PROP_GR_0	Pressure Drop
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                        Case 1
                            'PROP_GR_1	Outlet Temperature
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature)
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
                        Case "Element Balance Residue"
                            value = ElementBalance
                        Case Else
                            If prop.Contains("Conversion") Then
                                Dim comp = prop.Split(": ")(0)
                                If ComponentConversions.ContainsKey(comp) Then
                                    value = ComponentConversions(comp) * 100
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
                    For i = 0 To 1
                        proplist.Add("PROP_GR_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 1
                        proplist.Add("PROP_GR_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 1
                        proplist.Add("PROP_GR_" + CStr(i))
                    Next
                    proplist.Add("Calculation Mode")
                    proplist.Add("Initial Gibbs Energy")
                    proplist.Add("Final Gibbs Energy")
                    proplist.Add("Element Balance Residue")
                    For Each item In ComponentConversions
                        proplist.Add(item.Key + ": Conversion")
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
                    'PROP_GR_0	Pressure Drop
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 1
                    'PROP_GR_1	Outlet Temperature
                    Me.OutletTemperature = SystemsOfUnits.Converter.ConvertToSI(su.temperature, propval)

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
                                'PROP_GR_0	Pressure Drop
                                value = su.deltaP
                            Case 1
                                'PROP_GR_1	Outlet Temperature
                                value = su.temperature
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
                        Case "Element Balance Residue"
                            value = ""
                        Case Else
                            If prop.Contains("Conversion") Then value = "%"
                    End Select

                End If

                Return value

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
            Return My.Resources.re_gibbs_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("GIBBS_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("GIBBS_Name")
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
            str.AppendLine("    Element Balance Residue: " & Me.ElementBalance.ToString("R"))
            str.AppendLine()
            str.AppendLine("Compound Conversions")
            str.AppendLine()
            For Each dbl As KeyValuePair(Of String, Double) In Me.ComponentConversions
                If dbl.Value > 0.0# Then str.AppendLine("    " & dbl.Key & ": " & (dbl.Value * 100).ToString(numberformat, ci) & "%")
            Next
            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Gibbs Reactor '" & Me.GraphicObject.Tag + "'"}))
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
                    InitialGibbsEnergy.ConvertFromSI(su.heatflow).ToString(nf),
                    su.enthalpy}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Final Gibbs Free Energy",
                    FinalGibbsEnergy.ConvertFromSI(su.heatflow).ToString(nf),
                    su.enthalpy}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Element Balance Residue",
                    ElementBalance.ToString("G"),
                    ""}))

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