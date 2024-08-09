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

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = True

        Public Property UseIPOPTSolver As Boolean = True

        Public Property AlternateSolvingMethod As Boolean = False

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

        Dim Tab As Double?, Hr0, Qin, W0tot, N0tot As Double

        Dim DelGF() As Double

        Public Enum ReactivePhaseType
            CalculateEquilibrium = 0
            Vapor = 1
            Liquid = 2
            Solid = 3
        End Enum

        Public Property ReactivePhaseBehavior As ReactivePhaseType = ReactivePhaseType.CalculateEquilibrium

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

        Public Property MaximumInternalIterations As Integer = 1000

        Public Property InternalTolerance As Double = 1.0E-20

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

            If ReactivePhaseBehavior = ReactivePhaseType.CalculateEquilibrium Then
                tms.Calculate(True, False)
            End If

            pp.CurrentMaterialStream = tms

            Dim vfrac, l1frac, l2frac, sfrac As Double

            vfrac = tms.Phases(2).Properties.molarfraction.GetValueOrDefault()
            l1frac = tms.Phases(3).Properties.molarfraction.GetValueOrDefault()
            l2frac = tms.Phases(4).Properties.molarfraction.GetValueOrDefault()
            sfrac = tms.Phases(7).Properties.molarfraction.GetValueOrDefault()

            Select Case ReactivePhaseBehavior
                Case ReactivePhaseType.Vapor
                    vfrac = 1.0
                    l1frac = 0.0
                    l2frac = 0.0
                    sfrac = 0.0
                Case ReactivePhaseType.Liquid
                    vfrac = 0.0
                    l1frac = 1.0
                    l2frac = 0.0
                    sfrac = 0.0
                Case ReactivePhaseType.Solid
                    vfrac = 0.0
                    l1frac = 0.0
                    l2frac = 0.0
                    sfrac = 1.0
            End Select

            Dim P = tms.GetPressure()
            Dim P0 = 101325

            Dim gf = 0.0
            Dim t1, t2, t3 As Double

            If vfrac > 0.0 Then
                Dim xv = tms.GetPhaseComposition(2)
                If ReactivePhaseBehavior = ReactivePhaseType.Vapor Then
                    xv = tms.GetOverallComposition()
                End If
                Dim fugc = pp.DW_CalcFugCoeff(xv, T, P, PropertyPackages.State.Vapor)
                For i = 0 To xv.Length - 1
                    t1 = DelGF(i)
                    If xv(i) > 0 Then
                        t2 = Log(xv(i))
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    gf += vfrac * xv(i) * (t1 + t2 + t3) * 8.314 * T
                Next
            End If
            If l1frac > 0 Then
                Dim xv = tms.GetPhaseComposition(3)
                If ReactivePhaseBehavior = ReactivePhaseType.Liquid Then
                    xv = tms.GetOverallComposition()
                End If
                Dim fugc = pp.DW_CalcFugCoeff(xv, T, P, PropertyPackages.State.Liquid)
                For i = 0 To xv.Length - 1
                    t1 = DelGF(i)
                    If xv(i) > 0 Then
                        t2 = Log(xv(i))
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    gf += l1frac * xv(i) * (t1 + t2 + t3) * 8.314 * T
                Next
            End If
            If l2frac > 0.0 Then
                Dim xv = tms.GetPhaseComposition(4)
                Dim fugc = pp.DW_CalcFugCoeff(xv, T, P, PropertyPackages.State.Liquid)
                For i = 0 To xv.Length - 1
                    t1 = DelGF(i)
                    If xv(i) > 0 Then
                        t2 = Log(xv(i))
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    gf += l2frac * xv(i) * (t1 + t2 + t3) * 8.314 * T
                Next
            End If
            If sfrac > 0.0 Then
                Dim fugc = pp.DW_CalcSolidFugCoeff(T, P)
                Dim xv = tms.GetPhaseComposition(7)
                If ReactivePhaseBehavior = ReactivePhaseType.Vapor Then
                    xv = tms.GetOverallComposition()
                End If
                For i = 0 To xv.Length - 1
                    t1 = DelGF(i)
                    If xv(i) > 0 Then
                        t2 = Log(xv(i))
                    Else
                        t2 = Log(1.0E-20)
                    End If
                    t3 = Log(fugc(i) * P / P0)
                    If Double.IsNaN(t3) Or Double.IsInfinity(t3) Then t3 = 0.0
                    '???
                    t3 = 0.0
                    '???
                    gf += sfrac * xv(i) * (t1 + t2 + t3) * 8.314 * T
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

        Private Function FunctionValue2N(ByVal x() As Double) As Double()

            Dim i, j, n, c, e, pos As Integer

            n = x.Length - 5
            c = Me.ComponentIDs.Count - 1
            e = Me.Elements.Length - 1

            Dim lagm(), nv, nl1, nl2, ns, nt As Double

            Dim f(x.Length - 1) As Double

            Dim ids = tms.Phases(0).Compounds.Keys.ToList

            Dim sum As Double

            lagm = x.Take(n + 1).ToArray
            nv = x(n + 1)
            nl1 = x(n + 2)
            nl2 = x(n + 3)
            ns = x(n + 4)
            nt = nv + nl1 + nl2 + ns

            For i = 0 To c
                sum = 0.0#
                For j = 0 To n
                    sum += ElementMatrix(j, i) * lagm(j)
                Next
                pos = ids.IndexOf(ComponentIDs(i))
                xv_0(pos) = Exp(sum - (igcp(i) + Log(fv_0(pos) * P / 101325)))
                xl1_0(pos) = Exp(sum - (igcp(i) + Log(fl1_0(pos) * P / 101325)))
                xl2_0(pos) = Exp(sum - (igcp(i) + Log(fl2_0(pos) * P / 101325)))
                xs_0(pos) = Exp(sum - (igcp(i) + Log(fs_0(pos) * P / 101325)))
            Next

            For i = 0 To n
                sum = 0
                For j = 0 To c
                    pos = ids.IndexOf(ComponentIDs(j))
                    sum += ElementMatrix(i, j) * xv_0(pos) * nv
                    sum += ElementMatrix(i, j) * xl1_0(pos) * nl1
                    sum += ElementMatrix(i, j) * xl2_0(pos) * nl2
                    sum += ElementMatrix(i, j) * xs_0(pos) * ns
                Next
                f(i) = sum - TotalElements(i)
            Next
            f(n + 1) = nv * (xv_0.SumY - 1)
            f(n + 2) = nl1 * (xl1_0.SumY - 1)
            f(n + 3) = nl2 * (xl2_0.SumY - 1)
            f(n + 4) = ns * (xs_0.SumY - 1)

            'penalty value for negative mole flows

            Dim penval As Double = 0.0#

            If nv < 0.0# Then penval += nv ^ 2
            If nl1 < 0.0# Then penval += nl1 ^ 2
            If nl2 < 0.0# Then penval += nl2 ^ 2
            If ns < 0.0# Then penval += ns ^ 2

            'If penval > 0.0# Then
            '    For i = 0 To x.Length - 1
            '        f(i) += penval * (i + 1) ^ 2
            '    Next
            'End If

            Return f

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

        Private Function ReturnPenaltyValue() As Double

            'calculate penalty functions for constraint variables

            Dim i As Integer
            Dim n As Integer = tms.Phases(0).Compounds.Count - 1

            Dim con_lc(n), con_uc(n), con_val(n) As Double
            Dim pen_val As Double = 0
            Dim delta1, delta2 As Double

            i = 0
            For Each comp In tms.Phases(0).Compounds.Values
                con_lc(i) = 0
                con_uc(i) = 1
                con_val(i) = comp.MoleFraction.GetValueOrDefault
                i += 1
            Next

            pen_val = 0
            For i = 0 To n
                delta1 = con_val(i) - con_lc(i)
                delta2 = con_val(i) - con_uc(i)
                If delta1 < 0 Then
                    pen_val += -delta1 * 1000000
                ElseIf delta2 > 1 Then
                    pen_val += -delta2 * 1000000
                Else
                    pen_val += 0
                End If
            Next

            If Double.IsNaN(pen_val) Then pen_val = 0

            Return pen_val

        End Function

        Private Function FunctionValue2FC(ByVal x() As Double) As Double

            Dim sumel(els), totalsum As Double

            totalsum = 0
            For i = 0 To els
                sumel(i) = 0
                For j = 0 To comps
                    sumel(i) += Me.ElementMatrix(i, j) * x(j)
                Next
                sumel(i) -= Me.TotalElements(i)
                totalsum += sumel(i)
            Next

            Return totalsum

        End Function

        Private Function FixFugCoeff(fc() As Double, T As Double, st As PropertyPackages.State) As Double()

            Dim newfc(fc.Length - 1) As Double, i As Integer

            newfc = fc.Clone

            Dim Tf As Double() = Me.PropertyPackage.CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(c) c.ConstantProperties.TemperatureOfFusion).ToArray

            Select Case st
                Case PropertyPackages.State.Vapor, PropertyPackages.State.Liquid
                    For i = 0 To fc.Length - 1
                        If Tf(i) > T Then
                            'newfc(i) = 1.0E+30
                        End If
                    Next
                Case PropertyPackages.State.Solid
                    For i = 0 To fc.Length - 1
                        If Tf(i) < T Then
                            newfc(i) = 1.0E+30
                        End If
                    Next
            End Select

            Return newfc

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

        Public Overrides Sub DisplayDynamicsEditForm()

            If fd Is Nothing Then
                fd = New DynamicsPropertyEditor With {.SimObject = Me}
                fd.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
                fd.Tag = "ObjectEditor"
                fd.UpdateCallBack = Sub(table)
                                        AddButtonsToDynEditor(table)
                                    End Sub
                Me.FlowSheet.DisplayForm(fd)
            Else
                If fd.IsDisposed Then
                    fd = New DynamicsPropertyEditor With {.SimObject = Me}
                    fd.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
                    fd.Tag = "ObjectEditor"
                    fd.UpdateCallBack = Sub(table)
                                            AddButtonsToDynEditor(table)
                                        End Sub
                    Me.FlowSheet.DisplayForm(fd)
                Else
                    fd.Activate()
                End If
            End If

        End Sub

        Private Sub AddButtonsToDynEditor(table As TableLayoutPanel)

            Dim button1 As New Button With {.Text = FlowSheet.GetTranslatedString("ViewAccumulationStream"),
                .Dock = DockStyle.Bottom, .AutoSize = True, .AutoSizeMode = AutoSizeMode.GrowAndShrink}
            AddHandler button1.Click, Sub(s, e)
                                          AccumulationStream.SetFlowsheet(FlowSheet)
                                          Dim fms As New MaterialStreamEditor With {
                                          .MatStream = AccumulationStream,
                                          .IsAccumulationStream = True,
                                          .Text = Me.GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("AccumulationStream")}
                                          FlowSheet.DisplayForm(fms)
                                      End Sub

            Dim button2 As New Button With {.Text = FlowSheet.GetTranslatedString("FillWithStream"),
                .Dock = DockStyle.Bottom, .AutoSize = True, .AutoSizeMode = AutoSizeMode.GrowAndShrink}
            AddHandler button2.Click, Sub(s, e)
                                          AccumulationStream.SetFlowsheet(FlowSheet)
                                          Dim fms As New EditingForm_SeparatorFiller With {.Separator = Me}
                                          fms.ShowDialog()
                                      End Sub

            table.Controls.Add(button1)
            table.Controls.Add(button2)
            table.Controls.Add(New Panel())

        End Sub

        Public Overrides Sub CreateDynamicProperties()

            AddDynamicProperty("Operating Pressure (Dynamics)", "Current Operating Pressure", 0, UnitOfMeasure.pressure, 1.0.GetType())
            AddDynamicProperty("Liquid Level", "Current Liquid Level", 0, UnitOfMeasure.distance, 1.0.GetType())
            AddDynamicProperty("Volume", "Reactor Volume", 1, UnitOfMeasure.volume, 1.0.GetType())
            AddDynamicProperty("Height", "Available Height for Liquid", 2, UnitOfMeasure.distance, 1.0.GetType())
            AddDynamicProperty("Minimum Pressure", "Minimum Dynamic Pressure for this Unit Operation.", 101325, UnitOfMeasure.pressure, 1.0.GetType())
            AddDynamicProperty("Initialize using Inlet Stream", "Initializes the Reactor's available space with information from the inlet stream, if the vessel content is null.", False, UnitOfMeasure.none, True.GetType())
            AddDynamicProperty("Reset Contents", "Empties the Reactor's space on the next run.", False, UnitOfMeasure.none, True.GetType())
            RemoveDynamicProperty("Reset Content")

        End Sub

        Private prevM, currentM As Double

        Public Overrides Sub RunDynamicModel()

            Dim integratorID = FlowSheet.DynamicsManager.ScheduleList(FlowSheet.DynamicsManager.CurrentSchedule).CurrentIntegrator
            Dim integrator = FlowSheet.DynamicsManager.IntegratorList(integratorID)

            Dim timestep = integrator.IntegrationStep.TotalSeconds

            If integrator.RealTime Then timestep = Convert.ToDouble(integrator.RealTimeStepMs) / 1000.0

            Dim ims1 As MaterialStream = GetInletMaterialStream(0)

            Dim oms1 As MaterialStream = GetOutletMaterialStream(0)
            Dim oms2 As MaterialStream = GetOutletMaterialStream(1)

            Dim es = GetInletEnergyStream(1)

            Dim Height As Double = GetDynamicProperty("Height")
            Dim Pressure As Double
            Dim Pmin = GetDynamicProperty("Minimum Pressure")
            Dim InitializeFromInlet As Boolean = GetDynamicProperty("Initialize using Inlet Stream")

            Dim Reset As Boolean = GetDynamicProperty("Reset Contents")

            Dim Volume As Double = GetDynamicProperty("Volume")

            If Reset Then
                AccumulationStream = Nothing
                SetDynamicProperty("Reset Contents", 0)
            End If

            If AccumulationStream Is Nothing Then

                If InitializeFromInlet Then

                    AccumulationStream = ims1.CloneXML

                Else

                    AccumulationStream = ims1.Subtract(oms1, timestep)
                    If oms2 IsNot Nothing Then AccumulationStream = AccumulationStream.Subtract(oms2, timestep)

                End If

                Dim density = AccumulationStream.Phases(0).Properties.density.GetValueOrDefault

                AccumulationStream.SetMassFlow(density * Volume)
                AccumulationStream.SpecType = StreamSpec.Temperature_and_Pressure
                AccumulationStream.PropertyPackage = PropertyPackage
                AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream
                AccumulationStream.Calculate()

            Else

                AccumulationStream.SetFlowsheet(FlowSheet)
                AccumulationStream = AccumulationStream.Add(ims1, timestep)
                AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream
                AccumulationStream.Calculate()
                AccumulationStream = AccumulationStream.Subtract(oms1, timestep)
                If oms2 IsNot Nothing Then AccumulationStream = AccumulationStream.Subtract(oms2, timestep)
                If AccumulationStream.GetMassFlow <= 0.0 Then AccumulationStream.SetMassFlow(0.0)

            End If

            AccumulationStream.SetFlowsheet(FlowSheet)

            ' Calculate Temperature

            Dim Qval, Ha, Wa As Double

            Ha = AccumulationStream.GetMassEnthalpy
            Wa = AccumulationStream.GetMassFlow

            If es IsNot Nothing Then Qval = es.EnergyFlow.GetValueOrDefault

            'If Qval <> 0.0 Then

            '    If Wa > 0 Then

            '        AccumulationStream.SetMassEnthalpy(Ha + Qval * timestep / Wa)

            '        AccumulationStream.SpecType = StreamSpec.Pressure_and_Enthalpy

            '        AccumulationStream.PropertyPackage = PropertyPackage
            '        AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream

            '        If integrator.ShouldCalculateEquilibrium Then

            '            AccumulationStream.Calculate(True, True)

            '        End If

            '    End If

            'End If

            'calculate pressure

            Dim M = AccumulationStream.GetMolarFlow()

            Dim Temperature = AccumulationStream.GetTemperature()

            Pressure = AccumulationStream.GetPressure()

            'm3/mol

            prevM = currentM

            currentM = Volume / M

            PropertyPackage.CurrentMaterialStream = AccumulationStream

            Dim LiquidVolume, RelativeLevel As Double

            If AccumulationStream.GetPressure > Pmin Then

                If prevM = 0.0 Or integrator.ShouldCalculateEquilibrium Then

                    Dim result As IFlashCalculationResult

                    result = PropertyPackage.CalculateEquilibrium2(FlashCalculationType.VolumeTemperature, currentM, Temperature, Pressure)

                    Pressure = result.CalculatedPressure

                    LiquidVolume = AccumulationStream.Phases(3).Properties.volumetric_flow.GetValueOrDefault

                    RelativeLevel = LiquidVolume / Volume

                    SetDynamicProperty("Liquid Level", RelativeLevel * Height)

                Else

                    Pressure = currentM / prevM * Pressure

                End If

            Else

                Pressure = Pmin

                LiquidVolume = 0.0

                RelativeLevel = LiquidVolume / Volume

                SetDynamicProperty("Liquid Level", RelativeLevel * Height)

            End If

            AccumulationStream.SetPressure(Pressure)
            AccumulationStream.SpecType = StreamSpec.Temperature_and_Pressure

            AccumulationStream.PropertyPackage = PropertyPackage
            AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream

            If integrator.ShouldCalculateEquilibrium And Pressure > 0.0 Then

                AccumulationStream.Calculate(True, True)

            End If

            SetDynamicProperty("Operating Pressure", Pressure)

            Calculate(True)

            OutletTemperature = AccumulationStream.GetTemperature()

            DeltaT = OutletTemperature - ims1.GetTemperature()

            DeltaP = AccumulationStream.GetPressure() - ims1.GetPressure()

            DeltaQ = (AccumulationStream.GetMassEnthalpy() - ims1.GetMassEnthalpy()) * ims1.GetMassFlow()

            ' comp. conversions

            For Each sb As Compound In ims1.Phases(0).Compounds.Values
                If ComponentConversions.ContainsKey(sb.Name) > 0 Then
                    Dim n0 = ims1.Phases(0).Compounds(sb.Name).MolarFlow.GetValueOrDefault()
                    Dim nf = AccumulationStream.Phases(0).Compounds(sb.Name).MolarFlow.GetValueOrDefault()
                    ComponentConversions(sb.Name) = Math.Abs(n0 - nf) / nf
                End If
            Next

        End Sub


        Private LagrangeFactor As Double = 1000.0

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If AlternateSolvingMethod Then
                Calculate_Lagrange(args)
            Else
                Calculate_GibbsMin(args)
            End If

        End Sub

        Public Sub Calculate_GibbsMin(Optional ByVal args As Object = Nothing)

            Me.Validate()

            Dim dynamics As Boolean = False

            If args IsNot Nothing Then dynamics = args

            Qin = 0.0

            'energy stream
            If GetInletEnergyStream(1) IsNot Nothing Then
                Qin = GetInletEnergyStream(1).EnergyFlow.GetValueOrDefault()
            End If

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

            If dynamics Then
                ims = AccumulationStream.Clone()
            Else
                ims = GetInletMaterialStream(0).Clone()
            End If

            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage
            Dim ppr As New PropertyPackages.RaoultPropertyPackage()

            ims.SetFlowsheet(Me.FlowSheet)
            ims.SetPropertyPackage(PropertyPackage)

            Dim compremoved As Boolean = False

            For Each comp In ims.Phases(0).Compounds.Values
                If comp.ConstantProperties.IG_Enthalpy_of_Formation_25C = 0.0 And comp.ConstantProperties.OriginalDB <> "ChemSep" Then
                    If Me.ComponentIDs.Contains(comp.Name) Then
                        FlowSheet.ShowMessage(String.Format("Enthalpy of Formation data for compound '{0}' is missing or equal to 0.", comp.Name), IFlowsheet.MessageType.Warning)
                        'Me.ComponentIDs.Remove(comp.Name)
                        'compremoved = True
                    End If
                End If
                If comp.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C = 0.0 And comp.ConstantProperties.OriginalDB <> "ChemSep" Then
                    If Me.ComponentIDs.Contains(comp.Name) Then
                        FlowSheet.ShowMessage(String.Format("Gibbs Energy of Formation data for compound '{0}' is missing or equal to 0.", comp.Name), IFlowsheet.MessageType.Warning)
                        'Me.ComponentIDs.Remove(comp.Name)
                        'compremoved = True
                    End If
                End If
            Next

            If compremoved Then Me.CreateElementMatrix()

            'Reactants Enthalpy (kJ/kg * kg/s = kW) (ISOTHERMIC)

            Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

            Dim tmp As IFlashCalculationResult
            Dim xl, xv, xs, H, S As Double

            pp.CurrentMaterialStream = ims

            'read temperature and pressure from inlet stream.

            T0 = ims.Phases(0).Properties.temperature.GetValueOrDefault

            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    If Tab.HasValue Then
                        T = Tab.Value
                    Else
                        T = OutletTemperature
                    End If
                Case OperationMode.Isothermic
                    T = T0
                Case OperationMode.OutletTemperature
                    T = OutletTemperature
            End Select

            'Hr0i = pp.RET_Hid(298.15, T, pp.RET_VMOL(PropertyPackages.Phase.Mixture)) * ims.Phases(0).Properties.massflow.GetValueOrDefault

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
                ubo(i) = W0tot / CProps(i).Molar_Weight * 1000 * 10.0
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
            ipo.ReturnLowestObjFuncValue = True

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

                            pp.CurrentMaterialStream = ims

                            Dim delgfl As New List(Of Double)
                            For Each cmp As Compound In ims.Phases(0).Compounds.Values
                                delgfl.Add(pp.AUX_DELGF_T(298.15, T, cmp.Name) * cmp.ConstantProperties.Molar_Weight)
                            Next

                            DelGF = delgfl.ToArray()

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
                                    DHr += sb.ConstantProperties.IG_Enthalpy_of_Formation_25C * sb.ConstantProperties.Molar_Weight * DN(sb.Name) / 1000.0
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
                                    ims.SpecType = StreamSpec.Pressure_and_Enthalpy

                                    'Products Enthalpy (kJ/kg * kg/s = kW)
                                    Dim Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                                    ims.SetMassEnthalpy((Hr0 + Qin - DHr) / W0)

                                    ims.Calculate(True, True)

                                    Qerror = T - ims.GetTemperature()

                                    T = T / 2 + ims.GetTemperature() / 2

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
                Dim adberror As Double
                Do
                    adberror = gfunc.Invoke(T)
                Loop Until adberror <= 0.1
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

            If dynamics Then
                AccumulationStream.Assign(ims)
                AccumulationStream.AssignProps(ims)
            End If

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
                    .DefinedFlow = FlowSpec.Mass
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
                    .DefinedFlow = FlowSpec.Mass
                End With
            End If

            If ReactorOperationMode <> OperationMode.Adiabatic Then
                'energy stream - update energy flow value (kW)
                With GetInletEnergyStream(1)
                    .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                    .GraphicObject.Calculated = True
                End With
            End If

        End Sub

        Public Sub Calculate_Lagrange(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent

            IObj?.Paragraphs.Add("The calculation of chemical equilibrium at specified temperature and pressure is in many ways similar to the calculation of phase equilibrium. In both cases the equilibrium state corresponds to a global minimum of the Gibbs energy subject to a set of material balance constraints.")

            IObj?.Paragraphs.Add("For the phase equilibrium calculation these constraints represent component material balances, and they are usually eliminated explicitly by calculating the molar component amounts in one of the equilibrium phases from that in the other. A similar procedure can be used to convert the chemical equilibrium calculation to an unconstrained optimization problem, through the use of the 'reaction extents' formulation which ensures automatic satisfaction of all material balance constraints. Alternatively, a formulation can be set up by means of classical methods for constrained optimization where the constraints are handled by means of Lagrange multipliers.")

            IObj?.Paragraphs.Add("<h3>Chemical reaction equilibrium</h3>")

            IObj?.Paragraphs.Add("In phase equilibrium calculations for a given feed at specified temperature and pressure a material balance must be satisfied for each component in the mixture, the total amount in the combined product phases being identical to that in the feed. When chemical reactions occur, additional degrees of freedom are available, resulting in a set of material balance constraints, which is smaller than the number of components in the mixture.")

            IObj?.Paragraphs.Add("The mixture composition at chemical equilibrium at constant T and p satisfies the condition of minimum Gibbs energy,")

            IObj?.Paragraphs.Add("<m>\min G = \min \sum\limits_{i=1}^{C}{n_i\mu _i} </m>")

            IObj?.Paragraphs.Add("subject to a set of M < C material balance constraints. In addition we must require that")

            IObj?.Paragraphs.Add("<m>n_i \geq 0, i=1,2,...,C</m>")

            IObj?.Paragraphs.Add("<h3>Formula matrix and element balances</h3>")

            IObj?.Paragraphs.Add("The alternative formulation of the constraints is based on the requirement of conservation of chemical elements. A key concept in this approach is the formula matrix for the reaction components. In this matrix, Aji is the formula content of element j in component i.")

            IObj?.Paragraphs.Add("The element conservation constraints can be written")

            IObj?.Paragraphs.Add("<m>\mathbf{A}\mathbf{n}=\mathbf{b}</m>")

            IObj?.Paragraphs.Add("where <mi>b_k</mi> is the total amount of element k in the reaction mixture. The matrix A has M = C — R rows, where R is the number of independent reactions. It is readily shown that A, b, n0 and E are related as follows:")

            IObj?.Paragraphs.Add("<m>\mathbf{A}\mathbf{n}= \mathbf{A}\mathbf{n_0}+\mathbf{A}\mathbf{E}\zeta </m>")

            IObj?.Paragraphs.Add("This equation must be satisfied for all values of <mi>\zeta</mi>, and therefore")

            IObj?.Paragraphs.Add("<m>\mathbf{A}\mathbf{E}=0, \mathbf{A}\mathbf{n_0}=\mathbf{b}</m>")

            IObj?.Paragraphs.Add("The M rows of A must be linearly independent. If this is not the case, it is necessary to redefine the chosen 'elements'.")

            IObj?.Paragraphs.Add("<h3>Solution by constrained optimization</h3>")

            IObj?.Paragraphs.Add("The constraints defined above can be incorporated into the Gibbs energy minimization by means of Lagrange multipliers, <mi>\lambda</mi>. We find it preferable to work with the reduced Gibbs energy and form the augmented objective function,")

            IObj?.Paragraphs.Add("<m>\mathscr{L}(\mathbf{n,\lambda})=\sum\limits_{i}^{C}{\frac{n_iu_i}{RT}}-\sum\limits_{j=1}^{M}{\lambda _j}\left(\sum\limits_{i}^{C}{A_{j,i}n_i-b_j} \right) </m>")

            IObj?.Paragraphs.Add("by adding to the original objective function the constraint terms, multiplied by the Lagrange multipliers.")

            IObj?.Paragraphs.Add("At the minimum it is required that the derivatives of the Lagrange function are equal to zero")

            IObj?.Paragraphs.Add("<m>\frac{\partial \mathscr{L}}{\partial n_i}=\frac{\mu _i}{RT}-\sum\limits_{j=1}^{M}{A_{j,i}\lambda _j}=0</m>")

            IObj?.Paragraphs.Add("<m>\frac{\partial \mathscr{L}}{\partial \lambda_j}=-\sum\limits_{i=1}^{C}{A_{j,i}n_i}+b_j=0  </m>")

            IObj?.Paragraphs.Add("which yields a total of C + M equations to determine the C + M variables.")

            IObj?.Paragraphs.Add("The numerical solution of the partial derivatives is simplified when the reaction mixture forms an ideal solution. In this case, we can write")

            IObj?.Paragraphs.Add("<m>\frac{\mu _i}{RT} =\frac{\mu _i^*}{RT} +\ln x_i+\ln \varphi _i+\ln (P/P_0)=\frac{\mu _i^{pure}}{RT}+\ln x_i</m>")

            IObj?.Paragraphs.Add("where <mi>\mu _i^{pure}</mi> is the chemical potential of pure component i at the system temperature and pressure. The set of the first derivative equations then yields")

            IObj?.Paragraphs.Add("<m>\ln x_i=\sum\limits_{j=1}^{M}{A_{j,i}\lambda _j}-\frac{\mu _i^{pure}}{RT}  </m>")

            IObj?.Paragraphs.Add("Substituting <mi>n_i=n_tx_i</mi>, where nt is the total number of moles, we obtain the M + 1 equations")

            IObj?.Paragraphs.Add("<m>n_t\sum\limits_{i=1}^{C}{A_{j,i}x_i}-b_j=0, j=1,2,...,M</m>")

            IObj?.Paragraphs.Add("<m>\sum\limits_{i=1}^{C}{x_i}-1=0</m>")

            IObj?.Paragraphs.Add("The number of unknowns is now reduced to M +1: The M Lagrange multipliers and the total number of moles present, nt.")

            IObj?.Paragraphs.Add("The approach is readily generalised to multiphase systems. Let the chemical potential of pure component i in phase k at the system temperature and pressure be <mi>\mu _{i,k}^{pure}</mi>. Then,")

            IObj?.Paragraphs.Add("<m>\ln x_{i,k}=\sum\limits_{j=1}^{M}{A_{j,i}\lambda _j}-\frac{\mu_{i,k}^{pure}}{RT}</m>")

            IObj?.Paragraphs.Add("and we arrive at the following set of M + F equations")

            IObj?.Paragraphs.Add("<m>\sum\limits_{k=1}^{F}{n_{t,k}}\sum\limits_{i=1}^{C}{A_{ji}x_{i,k}}-b_j=0 </m>")

            IObj?.Paragraphs.Add("<m>\sum\limits_{i=1}^{C}{x_{i,k}}-1=0 </m>")

            IObj?.Paragraphs.Add("For moderately non-ideal mixtures, a successive substitution procedure is attractive. We just replace <mi>\mu _{i,k}^{pure}</mi> by <mi>\mu _i^*(T)+RT\ln (\varphi_{i,k}P/P_0)</mi> where <mi>\mu _i^*(T)</mi> is the ideal gas chemical potential at temperature T and pressure P0, and <mi>\varphi_{i,k}</mi>  is the fugacity coefficient of component i in phase k. A composition estimate provides initial values of the fugacity coeffcients, and the set of equations above are solved, yielding new phase compositions. These are in turn used to update the fugacity coefficients in an outer loop, and the process is repeated until convergence. Conceptually, we can consider the use of the  above equations as an approach where the equilibrium relations are satisfied automatically whereas the material  balances are solved iteratively.")

            IObj?.Paragraphs.Add("Initial estimates for the single phase as well as for the multiphase equilibrium calculation can be generated by means of linear programming. We construct an approximate solution based on the assumption that the chem ical potentials are composition independent. Evidently, this approximation is poor except for components with a mole fraction near 1, but it enables us to use a simple safe solution procedure. For the single phase equilibrium  calculation the Gibbs energy minimization becomes")

            IObj?.Paragraphs.Add("<m>\min \sum\limits_{i=1}^{C}{\frac{n_i\mu _i^{pure}}{RT} } </m>")

            IObj?.Paragraphs.Add("subject to")

            IObj?.Paragraphs.Add("<m>\sum\limits_{i=1}^{C}{A_{ji}n_i-b_j}=0,\space n_i \geq0,\space j=1,2,...,M </m>")

            IObj?.Paragraphs.Add("which is the standard form of a linear programming problem. The solution is (except in degenerate cases) a set of M non-zero values of the component mole numbers that enable a unique determination of the corresponding Lagrange multipliers from")

            IObj?.Paragraphs.Add("<m>\frac{\mu _i^{pure}}{RT}=\sum\limits_{j=1}^{M}{A_{ji}\lambda _j},\space n_i \geq 0  </m>")

            IObj?.Paragraphs.Add("The initial estimate from linear programming will yield a reasonable approximation for the Lagrange multipliers and the total number of moles.")

            IObj?.Paragraphs.Add("Problems can arise in cases where the solution to the LP-subproblem corresponds to a number of equilibrium components which is smaller than M. A simple example is given by the mixture (H2O, H2, 02). If the overall mixture composition corresponds to a b-vector with a ratio of H to O of exactly 2:1, the LP-solution at low temperature will be formation of water only, and we are unable to determine individual Lagrange multipliers for H and O.")

            IObj?.Paragraphs.Add("<h2>DWSIM Procedure</h2>")

            IObj?.Paragraphs.Add("DWSIM calculates the Gibbs Reactor using three nested loops. The external loop converges temperature for an adiabatic calculation, taking into account the composition changes during the convergence of inner loops.")
            IObj?.Paragraphs.Add("The intermediate loop converges the mass balance for all phases using an initial distribution obtained from the initial estimates for the overall composition.")
            IObj?.Paragraphs.Add("The internal loop converges fugacity coefficients calculated with the current estimate for phase compositions.")

            IObj?.Paragraphs.Add("<h2>Calculated Parameters</h2>")

            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)
            If Me.ReactionExtents Is Nothing Then Me.m_reactionextents = New Dictionary(Of String, Double)
            If Me.ComponentConversions Is Nothing Then Me.m_componentconversions = New Dictionary(Of String, Double)

            'first we validate the connections.

            Me.Validate()

            Dim i, j As Integer

            Me.Reactions.Clear()
            Me.ReactionExtents.Clear()
            Me.ReactionsSequence.Clear()
            Me.Conversions.Clear()
            Me.ComponentConversions.Clear()
            Me.DeltaQ = 0
            Me.DeltaT = 0

            Dim rx As Reaction
            ims = GetInletMaterialStream(0).Clone
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
            Dim Hr0, Hr0i As Double
            Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

            Dim tmp As IFlashCalculationResult
            Dim xl, xv, xs, H, S As Double

            pp.CurrentMaterialStream = ims

            'read temperature and pressure from inlet stream.

            T0 = ims.Phases(0).Properties.temperature.GetValueOrDefault

            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    T = T0 'initial value only, final value will be calculated by an iterative procedure
                Case OperationMode.Isothermic
                    T = T0
                Case OperationMode.OutletTemperature
                    T = OutletTemperature
            End Select

            IObj?.SetCurrent
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
                Me.TotalElements(i) = sum_e
            Next

            IObj?.Paragraphs.Add(String.Format("Element Matrix: {0}", ElementMatrix.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Total Elements: {0}", TotalElements.ToMathArrayString))

            Me.ComponentConversions.Clear()
            For Each s1 As String In Me.ComponentIDs
                Me.ComponentConversions.Add(s1, 0)
            Next

            i = 0

            'Dim lp As Integer

            Dim re(c + 1) As Double

            'calculate ideal gas gibbs energy values

            Dim igge(c), igge_lp(c + 1) As Double

            pp.CurrentMaterialStream = ims

            For i = 0 To c
                IObj?.SetCurrent
                igge(i) = pp.AUX_DELGF_T(298.15, T, Me.ComponentIDs(i), False) * FlowSheet.SelectedCompounds(Me.ComponentIDs(i)).Molar_Weight + Log(P / P0) / (8.314 * T)
            Next

            igcp = igge.Clone

            IObj?.Paragraphs.Add(String.Format("Ideal Gas Gibbs Energy values: {0}", igge.ToMathArrayString))

            Dim lagrm(e) As Double
            Dim nv, nl1, nl2, ns, nt As Double
            Dim resc(c) As Double

            If InitializeFromPreviousSolution And PreviousSolution.Count > 0 Then

                lagrm = PreviousSolution.Take(e + 1).ToArray()
                resc = PreviousSolution.GetRange(e + 1, c + 1).ToArray()

            Else

                'estimate initial values by solving linear problem using lp_solve

                'DWSIM.Thermodynamics.Calculator.CheckParallelPInvoke()

                Dim lp As IntPtr

                lpsolve55.Init(".")
                lp = lpsolve55.make_lp(0, c + 1)
                lpsolve55.default_basis(lp)

                For i = 0 To e
                    For j = 1 To c + 1
                        re(j) = Me.ElementMatrix(i, j - 1)
                    Next
                    lpsolve55.add_constraint(lp, re, lpsolve55.lpsolve_constr_types.EQ, Me.TotalElements(i))
                Next

                'calculate ideal gas gibbs energy values

                pp.CurrentMaterialStream = ims

                For i = 1 To c + 1
                    igge_lp(i) = pp.AUX_DELGF_T(298.15, T, Me.ComponentIDs(i - 1)) * FlowSheet.SelectedCompounds(Me.ComponentIDs(i - 1)).Molar_Weight + Log(P / P0)
                    lpsolve55.set_lowbo(lp, i, 0)
                Next

                lpsolve55.set_obj_fn(lp, igge_lp)
                lpsolve55.set_minim(lp)
                lpsolve55.solve(lp)

                lpsolve55.print_lp(lp)

                lpsolve55.print_solution(lp, c + 1)

                'the linear problem solution consists of only 'e' molar flows higher than zero.

                lpsolve55.get_variables(lp, resc)

                lpsolve55.delete_lp(lp)

                IObj?.Paragraphs.Add(String.Format("Initial Mole Amounts {0}", resc.ToArray.ToMathArrayString))

                'estimate lagrange multipliers

                Dim mymat As New Mapack.Matrix(e + 1, e + 1)
                Dim mypot As New Mapack.Matrix(e + 1, 1)
                Dim mylags As New Mapack.Matrix(e + 1, 1)

                Dim k As Integer = 0

                For i = 0 To e
                    k = 0
                    For j = 0 To c
                        If resc(j) > 0.0# Then
                            mymat(i, k) = Me.ElementMatrix(i, j)
                            mypot(k, 0) = igge(j)
                            k += 1
                        End If
                    Next
                Next

                Try
                    mylags = mymat.Solve(mypot.Multiply(-1))
                    For i = 0 To e
                        lagrm(i) = mylags(i, 0)
                    Next
                Catch ex As Exception
                    For i = 0 To e
                        lagrm(i) = 0.0
                    Next
                End Try

            End If

            Dim g0, g1, result(c + e + 1) As Double

            For i = 0 To c + e + 1
                If i <= c Then
                    result(i) = N0(Me.ComponentIDs(i))
                Else
                    result(i) = lagrm(i - c - 1)
                End If
            Next

            'this call to FunctionValue2G returns the gibbs energy in kJ/s for the inlet stream - initial gibbs energy.

            _IObj = IObj

            IObj?.SetCurrent

            g0 = FunctionValue2G(result)

            IObj?.SetCurrent

            IObj?.Paragraphs.Add(String.Format("Initial Gibbs Energy: {0}", g0))

            Me.InitialGibbsEnergy = g0

            Dim CalcFinished As Boolean = False

            Dim TLast As Double = T0 'remember T for iteration loops

            cnt = 0

            Do

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                _IObj = IObj2

                Inspector.Host.CheckAndAdd(IObj2, "", "Calculate", "Gibbs Reactor Convergence Temperature Loop Iteration #" & cnt, "", True)

                pp.CurrentMaterialStream = tms

                For i = 0 To c
                    IObj2?.SetCurrent
                    igcp(i) = pp.AUX_DELGF_T(298.15, T, Me.ComponentIDs(i), False) * FlowSheet.SelectedCompounds(Me.ComponentIDs(i)).Molar_Weight + Log(P / P0) / (8.314 * T)
                Next

                'estimate initial distribution between phases and fugacity coefficients

                IObj2?.Paragraphs.Add("Estimating initial distribution between phases and fugacity coefficients...")

                Dim xm0(tms.Phases(0).Compounds.Count - 1) As Double, ids As New List(Of String)

                ids = tms.Phases(0).Compounds.Keys.ToList

                If cnt = 0 Then

                    i = 0
                    For Each id In ComponentIDs
                        xm0(ids.IndexOf(id)) = resc(i) / resc.Sum
                        i += 1
                    Next

                Else

                    For Each id In ComponentIDs
                        xm0(ids.IndexOf(id)) = N(id) / N.Values.Sum
                    Next

                    xm0 = xm0.NormalizeY

                End If

                IObj2?.Paragraphs.Add(String.Format("Initial Estimate for Mixture Molar Composition: {0}", xm0.ToMathArrayString))

                IObj2?.SetCurrent
                Dim flashresults = pp.FlashBase.CalculateEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.T, P, T, pp, xm0, Nothing, 0)

                With flashresults
                    xv_0 = .GetVaporPhaseMoleFractions
                    xl1_0 = .GetLiquidPhase1MoleFractions
                    xl2_0 = .GetLiquidPhase2MoleFractions
                    xs_0 = .GetSolidPhaseMoleFractions
                    IObj2?.SetCurrent
                    fv_0 = pp.DW_CalcFugCoeff(xv_0, T, P, PropertyPackages.State.Vapor).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray
                    IObj2?.SetCurrent
                    fl1_0 = pp.DW_CalcFugCoeff(xl1_0, T, P, PropertyPackages.State.Liquid).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray
                    IObj2?.SetCurrent
                    fl2_0 = pp.DW_CalcFugCoeff(xl2_0, T, P, PropertyPackages.State.Liquid).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray
                    IObj2?.SetCurrent
                    fs_0 = pp.DW_CalcSolidFugCoeff(T, P).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray
                    nv = .GetVaporPhaseMoleFraction
                    nl1 = .GetLiquidPhase1MoleFraction
                    nl2 = .GetLiquidPhase2MoleFraction
                    ns = .GetSolidPhaseMoleFraction
                End With

                If nv > 0.0# Then nv *= W0tot / pp.AUX_MMM(xv_0) * 1000 Else nv = 0.0001 * N0tot
                If nl1 > 0.0# Then nl1 *= W0tot / pp.AUX_MMM(xl1_0) * 1000 Else nl1 = 0.0001 * N0tot
                If nl2 > 0.0# Then nl2 *= W0tot / pp.AUX_MMM(xl2_0) * 1000 Else nl2 = 0.0001 * N0tot
                If ns > 0.0# Then ns *= W0tot / pp.AUX_MMM(xs_0) * 1000 Else ns = 0.0001 * N0tot

                nt = nv + nl1 + nl2 + ns

                fv_0 = FixFugCoeff(fv_0, T, PropertyPackages.State.Vapor)
                fl1_0 = FixFugCoeff(fl1_0, T, PropertyPackages.State.Liquid)
                fl2_0 = FixFugCoeff(fl2_0, T, PropertyPackages.State.Liquid)
                fs_0 = FixFugCoeff(fs_0, T, PropertyPackages.State.Solid)

                IObj2?.Paragraphs.Add(String.Format("Initial Vapor Phase Amount: {0}", nv))
                IObj2?.Paragraphs.Add(String.Format("Initial Liquid Phase 1 Amount: {0}", nl1))
                IObj2?.Paragraphs.Add(String.Format("Initial Liquid Phase 2 Amount: {0}", nl2))
                IObj2?.Paragraphs.Add(String.Format("Initial Solid Phase Amount: {0}", ns))

                IObj2?.Paragraphs.Add(String.Format("Initial Vapor Phase Composition: {0}", xv_0.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Initial Liquid Phase 1 Composition: {0}", xl1_0.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Initial Liquid Phase 2 Composition: {0}", xl2_0.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Initial Solid Phase Composition: {0}", xs_0.ToMathArrayString))

                'outer loop for converging fugacity coefficients

                Dim sumerr As Double = 0.0#

                Dim fx(e + 1 + 4), dfdx(e + 1 + 4, e + 1 + 4), dx(e + 1 + 4), x(e + 1 + 4), px(e + 1 + 4), df, fval As Double

                Dim ni_ext As Integer

                ni_ext = 0

                If lagrm.Sum = 0.0 Then

                    'initial values for the lagrange multipliers

                    For i = 0 To c
                        IObj2?.SetCurrent
                        igcp(i) = pp.AUX_DELGF_T(298.15, LagrangeCoeffsEstimationTemperature, Me.ComponentIDs(i), False) * FlowSheet.SelectedCompounds(Me.ComponentIDs(i)).Molar_Weight + Log(P / P0) / (8.314 * 1000)
                    Next

                    Dim variables As New List(Of OptBoundVariable)
                    For i = 0 To e
                        variables.Add(New OptBoundVariable(0.0#, -1000.0#, 1000.0#))
                    Next
                    variables.Add(New OptBoundVariable(nv, True))
                    variables.Add(New OptBoundVariable(nl1, True))
                    variables.Add(New OptBoundVariable(nl2, True))
                    variables.Add(New OptBoundVariable(ns, True))

                    Dim solver As New Simplex, smplres As Double()
                    solver.MaxFunEvaluations = 50000
                    solver.Tolerance = 0.0000000001
                    smplres = solver.ComputeMin(Function(x1)
                                                    Return FunctionValue2N(x1).AbsSqrSumY
                                                End Function, variables.ToArray)

                    lagrm = smplres.Take(e + 1).ToArray

                    For i = 0 To c
                        IObj2?.SetCurrent
                        igcp(i) = pp.AUX_DELGF_T(298.15, T, Me.ComponentIDs(i), False) * FlowSheet.SelectedCompounds(Me.ComponentIDs(i)).Molar_Weight + Log(P / P0) / (8.314 * T)
                    Next

                End If

                IObj2?.Paragraphs.Add(String.Format("Lagrange Multipliers: {0}", lagrm.ToMathArrayString))

                'convergence of the material balance + gibbs minimization using Newton's method
                'external loop: fugacity coefficient calculation/update
                'internal loop: material balance convergence

                Dim finalx As Double()

                Dim nsolv As New MathOps.MathEx.Optimization.NewtonSolver
                nsolv.MaxIterations = MaximumInternalIterations
                nsolv.Tolerance = InternalTolerance

                Dim s2 As New Simplex
                s2.MaxFunEvaluations = 50000
                s2.Tolerance = 0.00001

                Do

                    IObj2?.SetCurrent

                    Dim IObj3 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    _IObj = IObj3

                    Inspector.Host.CheckAndAdd(IObj3, "", "Calculate", "Gibbs Reactor External Loop Iteration #" & ni_ext, "Converge Fugacity Coefficients", True)

                    x = lagrm.Concat({nv, nl1, nl2, ns}).ToArray

                    Dim fail As Boolean = False

                    Try
                        finalx = nsolv.Solve(Function(x1)
                                                 Return FunctionValue2N(x1)
                                             End Function, x)
                    Catch ex As Exception
                        fail = True
                    End Try

                    If fail Then
                        'try simplex.
                        finalx = s2.ComputeMin(Function(x1)
                                                   Return FunctionValue2N(x1).AbsSqrSumY
                                               End Function, x)
                    End If

                    lagrm = finalx.Take(e + 1).ToArray

                    nv = finalx(e + 1)
                    nl1 = finalx(e + 2)
                    nl2 = finalx(e + 3)
                    ns = finalx(e + 4)

                    IObj3?.Paragraphs.Add(String.Format("Updated Lagrange Multipliers: {0}", lagrm.ToMathArrayString))

                    IObj3?.SetCurrent
                    fv_0 = pp.DW_CalcFugCoeff(xv_0, T, P, PropertyPackages.State.Vapor).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray
                    IObj3?.SetCurrent
                    fl1_0 = pp.DW_CalcFugCoeff(xl1_0, T, P, PropertyPackages.State.Liquid).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray
                    IObj3?.SetCurrent
                    fl2_0 = pp.DW_CalcFugCoeff(xl2_0, T, P, PropertyPackages.State.Liquid).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray
                    IObj3?.SetCurrent
                    fs_0 = pp.DW_CalcSolidFugCoeff(T, P).Select(Function(d) If(Double.IsNaN(d), 1.0#, d)).ToArray

                    fv_0 = FixFugCoeff(fv_0, T, PropertyPackages.State.Vapor)
                    fl1_0 = FixFugCoeff(fl1_0, T, PropertyPackages.State.Liquid)
                    fl2_0 = FixFugCoeff(fl2_0, T, PropertyPackages.State.Liquid)
                    fs_0 = FixFugCoeff(fs_0, T, PropertyPackages.State.Solid)

                    IObj3?.Paragraphs.Add(String.Format("Vapor Phase Amount: {0}", nv))
                    IObj3?.Paragraphs.Add(String.Format("Liquid Phase 1 Amount: {0}", nl1))
                    IObj3?.Paragraphs.Add(String.Format("Liquid Phase 2 Amount: {0}", nl2))
                    IObj3?.Paragraphs.Add(String.Format("Solid Phase Amount: {0}", ns))

                    IObj3?.Paragraphs.Add(String.Format("Vapor Phase Composition: {0}", xv_0.ToMathArrayString))
                    IObj3?.Paragraphs.Add(String.Format("Liquid Phase 1 Composition: {0}", xl1_0.ToMathArrayString))
                    IObj3?.Paragraphs.Add(String.Format("Liquid Phase 2 Composition: {0}", xl2_0.ToMathArrayString))
                    IObj3?.Paragraphs.Add(String.Format("Solid Phase Composition: {0}", xs_0.ToMathArrayString))

                    sumerr = finalx.SubtractY(x).AbsSqrSumY

                    IObj3?.Paragraphs.Add(String.Format("Error Value: {0}", sumerr))

                    IObj3?.Close()

                    ni_ext += 1

                Loop Until sumerr < 0.00001 Or ni_ext > 1000

                If ni_ext > 1000 Then

                    Throw New Exception(FlowSheet.GetTranslatedString("Nmeromximodeiteraesa3"))

                End If

                Dim errfunc = FunctionValue2N(finalx).AbsSqrSumY

                If errfunc > 0.00001 Then

                    errfunc = FunctionValue2N(finalx).AbsSqrSumY

                    If errfunc > 0.00001 Then

                        Throw New Exception(FlowSheet.GetTranslatedString("ConvergenceError"))

                    End If

                End If

                For Each id In ComponentIDs
                    i = ids.IndexOf(id)
                    N(ids(i)) = nv * xv_0(i) + nl1 * xl1_0(i) + nl2 * xl2_0(i) + ns * xs_0(i)
                    DN(ids(i)) = N(ids(i)) - N0(ids(i))
                Next

                _IObj = IObj2

                IObj2?.SetCurrent

                'reevaluate function

                'this call to FunctionValue2G returns the final gibbs energy in kJ/s.

                g1 = FunctionValue2G(N.Values.ToArray)

                IObj2?.SetCurrent

                IObj2?.Paragraphs.Add(String.Format("Final Gibbs Energy: {0}", g1))

                If (g1 > g0) Then FlowSheet.ShowMessage(Me.GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("GibbsLocalEquilibrium"), IFlowsheet.MessageType.Warning)

                Me.FinalGibbsEnergy = g1

                'this call to FunctionValue2FC returns the element material balance - should be very very close to zero.

                _elbal = Me.FunctionValue2FC(N.Values.ToArray)

                IObj2?.Paragraphs.Add(String.Format("Element Balance: {0}", _elbal))

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

                IObj2?.SetCurrent

                Me.PropertyPackage.CurrentMaterialStream = ims

                Select Case Me.ReactorOperationMode

                    Case OperationMode.Adiabatic

                        Me.DeltaQ = 0.0#

                        'Products Enthalpy (kJ/kg * kg/s = kW)

                        Dim Hp = Hr0 - DHr

                        Hp = Hp / ims.Phases(0).Properties.massflow.GetValueOrDefault

                        ims.Phases(0).Properties.enthalpy = Hp

                        pp.CurrentMaterialStream = ims

                        ims.SpecType = StreamSpec.Pressure_and_Enthalpy
                        ims.Calculate(True, True)

                        TLast = T
                        T = ims.Phases(0).Properties.temperature

                        Me.DeltaT = T - T0

                        If Abs(T - TLast) < 0.5 Then CalcFinished = True

                        T = TLast * 0.7 + T * 0.3

                        ims.Phases(0).Properties.temperature = T
                        tms.Phases(0).Properties.temperature = T

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

                If cnt > 50 Then
                    Throw New Exception(FlowSheet.GetTranslatedString("Nmeromximodeiteraesa3"))
                End If

                IObj2?.Close()

            Loop Until CalcFinished

            PreviousSolution.Clear()

            For i = 0 To lagrm.Count - 1
                PreviousSolution.Add(lagrm(i))
            Next
            For i = 0 To N.Count - 1
                PreviousSolution.Add(N.Values.ToArray(i))
            Next

            IObj?.SetCurrent

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
                    For Each comp In .Phases(0).Compounds.Values
                        If xv = 0.0# Then
                            comp.MoleFraction = 0.0#
                            comp.MassFraction = 0.0#
                        Else
                            comp.MoleFraction = Vy(ids2.IndexOf(comp.Name))
                            comp.MassFraction = Vwy(ids2.IndexOf(comp.Name))
                        End If
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
                    For Each comp In .Phases(0).Compounds.Values
                        If (1 - xv) = 0.0# Then
                            comp.MoleFraction = 0.0#
                            comp.MassFraction = 0.0#
                        Else
                            comp.MoleFraction = (Vx(ids2.IndexOf(comp.Name)) * xl + Vs(ids2.IndexOf(comp.Name)) * xs) / (1 - xv)
                            comp.MassFraction = (Vwx(ids2.IndexOf(comp.Name)) * wl + Vws(ids2.IndexOf(comp.Name)) * ws) / (1 - wv)
                        End If
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
            Return My.Resources.reactor_gibbs
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