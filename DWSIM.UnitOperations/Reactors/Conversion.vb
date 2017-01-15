'    Conversion Reactor Calculation Routines 
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Namespace Reactors

    <System.Serializable()> Public Class Reactor_Conversion

        Inherits Reactor

        <NonSerialized> <Xml.Serialization.XmlIgnore> Dim f As EditingForm_ReactorConvEqGibbs

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedeenerg17"))
            End If

            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)

            Dim ConversionLimiter As New Dictionary(Of String, Double)

            Me.Reactions.Clear()
            Me.ReactionsSequence.Clear()
            Me.Conversions.Clear()
            Me.DeltaQ = 0
            Me.DeltaT = 0

            'check active reactions (conversion only) in the reaction set
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                If FlowSheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Conversion And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                End If
            Next

            'order reactions
            Dim i As Integer
            i = 0
            Dim maxrank As Integer = 0
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                If rxnsb.Rank > maxrank And Me.Reactions.Contains(rxnsb.ReactionID) Then maxrank = rxnsb.Rank
            Next

            'ordering of parallel reactions

            i = 0
            Dim arr As New ArrayList
            Do
                arr = New ArrayList
                For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                    If rxnsb.Rank = i And Me.Reactions.Contains(rxnsb.ReactionID) Then arr.Add(rxnsb.ReactionID)
                Next
                If arr.Count > 0 Then Me.ReactionsSequence.Add(i, arr)
                i = i + 1
            Loop Until i = maxrank + 1

            Dim ims As MaterialStream = DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Clone
            Dim pp = Me.PropertyPackage

            ims.SetFlowsheet(Me.FlowSheet)
            ims.PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag

            pp.CurrentMaterialStream = ims

            Dim DN As New Dictionary(Of String, Double)
            Dim N0 As New Dictionary(Of String, Double)
            Dim N As New Dictionary(Of String, Double)

            Dim X, scBC, nBC, DHr, Hid_r, Hid_p, Hr, Hp, Tin, Pin, Pout, W As Double
            Dim BC As String = ""
            Dim tmp As IFlashCalculationResult

            Tin = ims.Phases(0).Properties.temperature.GetValueOrDefault
            Pin = ims.Phases(0).Properties.pressure.GetValueOrDefault
            W = ims.Phases(0).Properties.massflow.GetValueOrDefault
            Pout = ims.Phases(0).Properties.pressure.GetValueOrDefault - Me.DeltaP.GetValueOrDefault
            ims.Phases(0).Properties.pressure = Pout

            'Reactants Enthalpy (kJ/kg * kg/s = kW)

            Hr = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

            Dim rxn As Reaction

            'loop through reactions

            For Each ar As ArrayList In Me.ReactionsSequence.Values

                ConversionLimiter.Clear()

                'calculate conversion limiters

                i = 0
                Do

                    'process reaction i

                    rxn = FlowSheet.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    'initial mole flows
                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        Select Case rxn.ReactionPhase
                            Case PhaseName.Liquid
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Phases(1).Compounds(sb.CompName).MoleFraction.GetValueOrDefault * ims.Phases(1).Properties.molarflow.GetValueOrDefault)
                                    N.Add(sb.CompName, N0(sb.CompName))
                                Else
                                    N0(sb.CompName) = ims.Phases(1).Compounds(sb.CompName).MoleFraction.GetValueOrDefault * ims.Phases(1).Properties.molarflow.GetValueOrDefault
                                    'N(sb.CompName) = N0(sb.CompName)
                                End If
                            Case PhaseName.Vapor
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Phases(2).Compounds(sb.CompName).MoleFraction.GetValueOrDefault * ims.Phases(2).Properties.molarflow.GetValueOrDefault)
                                    N.Add(sb.CompName, N0(sb.CompName))
                                Else
                                    N0(sb.CompName) = ims.Phases(2).Compounds(sb.CompName).MoleFraction.GetValueOrDefault * ims.Phases(2).Properties.molarflow.GetValueOrDefault
                                    'N(sb.CompName) = N0(sb.CompName)
                                End If
                            Case PhaseName.Mixture
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Phases(0).Compounds(sb.CompName).MoleFraction.GetValueOrDefault * ims.Phases(0).Properties.molarflow.GetValueOrDefault)
                                    N.Add(sb.CompName, N0(sb.CompName))
                                Else
                                    N0(sb.CompName) = ims.Phases(0).Compounds(sb.CompName).MoleFraction.GetValueOrDefault * ims.Phases(0).Properties.molarflow.GetValueOrDefault
                                    'N(sb.CompName) = N0(sb.CompName)
                                End If
                        End Select

                    Next

                    nBC = N0(rxn.BaseReactant)

                    If FlowSheet.MobileCompatibilityMode Then

                        Dim parser As YAMP.Parser = New YAMP.Parser()
                        Dim vars As New Dictionary(Of String, YAMP.Value)
                        vars.Add("T", New YAMP.ScalarValue(ims.Phases(0).Properties.temperature.GetValueOrDefault))
                        X = DirectCast(parser.Evaluate(rxn.Expression, vars), YAMP.ScalarValue).Value / 100

                    Else

                        rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                        rxn.ExpContext.Imports.AddType(GetType(System.Math))
                        rxn.ExpContext.Variables.Clear()
                        rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                        rxn.ExpContext.Variables.Add("T", ims.Phases(0).Properties.temperature.GetValueOrDefault)

                        rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.Expression)
                        X = rxn.Expr.Evaluate / 100

                    End If

                    If X < 0 Or X > 1 Then Throw New ArgumentOutOfRangeException("Conversion Expression", "The conversion expression for reaction " & rxn.Name & " results in a value that is out of the valid range (0 to 100%).")

                    'verify if the calculated conversion is reachable

                    For Each sb As ReactionStoichBase In rxn.Components.Values
                        If sb.StoichCoeff < 1 Then
                            If Not ConversionLimiter.ContainsKey(sb.CompName) Then ConversionLimiter.Add(sb.CompName, 0.0#)
                            Dim amount As Double = Abs(X * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC)
                            ConversionLimiter(sb.CompName) += amount / N0(sb.CompName)
                        End If
                    Next

                    i += 1

                Loop Until i = ar.Count

                i = 0
                DHr = 0
                Hid_r = 0
                Hid_p = 0
                Do

                    'process reaction i
                    rxn = FlowSheet.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    nBC = N0(rxn.BaseReactant)

                    If FlowSheet.MobileCompatibilityMode Then

                        Dim parser As YAMP.Parser = New YAMP.Parser()
                        Dim vars As New Dictionary(Of String, YAMP.Value)
                        vars.Add("T", New YAMP.ScalarValue(ims.Phases(0).Properties.temperature.GetValueOrDefault))
                        X = DirectCast(parser.Evaluate(rxn.Expression, vars), YAMP.ScalarValue).Value / 100

                    Else

                        rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                        rxn.ExpContext.Imports.AddType(GetType(System.Math))
                        rxn.ExpContext.Variables.Clear()
                        rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                        rxn.ExpContext.Variables.Add("T", ims.Phases(0).Properties.temperature.GetValueOrDefault)

                        rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.Expression)
                        X = rxn.Expr.Evaluate / 100

                    End If

                    If X < 0.0# Or X > 1.0# Then

                        Throw New ArgumentOutOfRangeException("Conversion Expression", "The conversion expression for reaction " & rxn.Name & " results in a value that is out of the valid range (0 to 100%).")

                    Else

                        If ConversionLimiter.Values.Max > 1.0# Then X /= ConversionLimiter.Values.Max

                        If Not Me.Conversions.ContainsKey(rxn.ID) Then
                            Me.Conversions.Add(rxn.ID, X)
                        Else
                            Me.Conversions(rxn.ID) = X
                        End If

                        'delta mole flows

                        For Each sb As ReactionStoichBase In rxn.Components.Values
                            If Not DN.ContainsKey(sb.CompName) Then
                                DN.Add(sb.CompName, -X * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC)
                            Else
                                DN(sb.CompName) = -X * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC
                            End If
                        Next

                        'final mole flows

                        For Each sb As ReactionStoichBase In rxn.Components.Values
                            N(sb.CompName) += DN(sb.CompName)
                        Next

                        'Ideal Gas Reactants Enthalpy (kJ/kg * kg/s = kW)
                        Hid_r += 0 'ppr.RET_Hid(298.15, ims.Phases(0).Properties.temperature.GetValueOrDefault, PropertyPackages.Phase.Mixture) * ims.Phases(0).Properties.massflow.GetValueOrDefault

                        'update mole flows/fractions
                        Dim Nall(ims.Phases(0).Compounds.Count - 1), Nsum As Double
                        For Each sb As ReactionStoichBase In rxn.Components.Values

                            Nsum = 0
                            For Each s2 As Compound In ims.Phases(0).Compounds.Values
                                If rxn.Components.ContainsKey(s2.Name) Then
                                    Nsum += N(s2.Name)
                                Else
                                    Nsum += ims.Phases(0).Compounds(s2.Name).MoleFraction.GetValueOrDefault * ims.Phases(0).Properties.molarflow.GetValueOrDefault
                                End If
                            Next

                            For Each s3 As Compound In ims.Phases(0).Compounds.Values
                                If rxn.Components.ContainsKey(s3.Name) Then
                                    s3.MoleFraction = N(s3.Name) / Nsum
                                Else
                                    s3.MoleFraction = ims.Phases(0).Compounds(s3.Name).MoleFraction.GetValueOrDefault * ims.Phases(0).Properties.molarflow.GetValueOrDefault / Nsum
                                End If
                            Next

                            ims.Phases(0).Properties.molarflow = Nsum

                            Dim mmm As Double = 0

                            For Each s3 As Compound In ims.Phases(0).Compounds.Values
                                mmm += s3.MoleFraction.GetValueOrDefault * s3.ConstantProperties.Molar_Weight
                            Next

                            For Each s3 As Compound In ims.Phases(0).Compounds.Values
                                s3.MassFraction = s3.MoleFraction.GetValueOrDefault * s3.ConstantProperties.Molar_Weight / mmm
                            Next

                        Next

                        'Ideal Gas Products Enthalpy (kJ/kg * kg/s = kW)
                        Hid_p += 0 'ppr.RET_Hid(298.15, ims.Phases(0).Properties.temperature.GetValueOrDefault, PropertyPackages.Phase.Mixture) * ims.Phases(0).Properties.massflow.GetValueOrDefault

                    End If

                    'Heat released (or absorbed) (kJ/s = kW) (Ideal Gas)
                    DHr += rxn.ReactionHeat * Abs(DN(rxn.BaseReactant)) / 1000

                    i += 1

                Loop Until i = ar.Count

                'do a flash calc (calculate final temperature/enthalpy)

                Select Case Me.ReactorOperationMode

                    Case OperationMode.Adiabatic

                        Me.DeltaQ = 0.0#

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = Me.DeltaQ.GetValueOrDefault + Hr + Hid_p - Hid_r - DHr
                        Hp = Hp / W

                        ims.Phases(0).Properties.enthalpy = Hp
                        ims.SpecType = StreamSpec.Pressure_and_Enthalpy

                        ims.Calculate(True, True)

                        Dim Tout As Double = ims.Phases(0).Properties.temperature.GetValueOrDefault
                        Me.DeltaT = Tout - Tin

                    Case OperationMode.Isothermic

                        ims.SpecType = StreamSpec.Temperature_and_Pressure
                        ims.Calculate(True, True)

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                        'Heat (kW)
                        Me.DeltaQ = Me.DeltaQ.GetValueOrDefault + DHr + Hid_r + Hp - Hr - Hid_p

                        Me.DeltaT = 0

                    Case OperationMode.OutletTemperature

                        Dim Tout As Double = Me.OutletTemperature

                        Me.DeltaT = Tout - Tin

                        ims.Phases(0).Properties.temperature = Tout
                        ims.SpecType = StreamSpec.Temperature_and_Pressure

                        ims.Calculate(True, True)

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                        'Heat (kW)
                        Me.DeltaQ = Me.DeltaQ.GetValueOrDefault + DHr + Hid_r + Hp - Hr - Hid_p

                End Select

            Next

            'Copy results to upstream MS
            Dim xl, xv, xs, T, P, H, S, wtotalx, wtotaly, wtotalS As Double
            Dim nc As Integer = ims.Phases(0).Compounds.Count - 1
            pp.CurrentMaterialStream = ims
            tmp = pp.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, ims.Phases(0).Properties.pressure.GetValueOrDefault, ims.Phases(0).Properties.temperature.GetValueOrDefault, 0)

            Dim Vx(nc), Vy(nc), Vs(nc), Vwx(nc), Vwy(nc), Vws(nc) As Double
            xl = tmp.GetLiquidPhase1MoleFraction
            xv = tmp.GetVaporPhaseMoleFraction
            xs = tmp.GetSolidPhaseMoleFraction
            T = tmp.CalculatedTemperature
            P = tmp.CalculatedPressure
            H = tmp.CalculatedEnthalpy
            S = tmp.CalculatedEntropy
            Vx = tmp.GetLiquidPhase1MoleFractions
            Vy = tmp.GetVaporPhaseMoleFractions
            Vs = tmp.GetSolidPhaseMoleFractions

            Dim j As Integer = 0

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
                    wtotalS += Vs(i) * comp.ConstantProperties.Molar_Weight
                    i += 1
                Next
                i = 0
                For Each comp In ms.Phases(0).Compounds.Values
                    If wtotalx > 0 Then Vwx(i) = Vx(i) * comp.ConstantProperties.Molar_Weight / wtotalx
                    If wtotaly > 0 Then Vwy(i) = Vy(i) * comp.ConstantProperties.Molar_Weight / wtotaly
                    If wtotalS > 0 Then Vws(i) = Vs(i) * comp.ConstantProperties.Molar_Weight / wtotalS
                    i += 1
                Next
            End If

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .SpecType = StreamSpec.Temperature_and_Pressure
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = Vy(j)
                        comp.MassFraction = Vwy(j)
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = W * (wtotaly * xv / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Phases(0).Properties.massfraction = (wtotaly * xv / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Phases(0).Properties.molarfraction = 1
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .SpecType = StreamSpec.Temperature_and_Pressure
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = (Vx(j) * xl + Vs(j) * xs) / (xl + xs)
                        comp.MassFraction = (Vwx(j) * wtotalx + Vws(j) * wtotalS) / (wtotalx + wtotalS)
                        j += 1
                    Next
                    j = 0
                    .Phases(0).Properties.massflow = W * ((wtotalx * xl + wtotalS * xs) / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Phases(0).Properties.massfraction = ((wtotalx * xl + wtotalS * xs) / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Phases(0).Properties.molarfraction = 1
                End With
            End If

            'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
            With GetInletEnergyStream(1)
                .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                .GraphicObject.Calculated = True
            End With

        End Sub

        Public Overrides Sub DeCalculate()

            'If Not Me.GraphicObject.InputConnectors(0).IsAttached Then Throw New Exception(Flowsheet.GetTranslatedString("Nohcorrentedematriac10"))
            'If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then Throw New Exception(Flowsheet.GetTranslatedString("Nohcorrentedematriac11"))
            'If Not Me.GraphicObject.OutputConnectors(1).IsAttached Then Throw New Exception(Flowsheet.GetTranslatedString("Nohcorrentedematriac11"))
            'Dim ems As DWSIM.SimulationObjects.Streams.MaterialStream = form.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            'Dim W As Double = ems.Phases(0).Properties.massflow.GetValueOrDefault
            Dim j As Integer = 0

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
                    'PROP_HT_0    Pressure Drop
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
                        proplist.Add("PROP_CR_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 0
                        proplist.Add("PROP_CR_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 0
                        proplist.Add("PROP_CR_" + CStr(i))
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

            If f Is Nothing Then
                f = New EditingForm_ReactorConvEqGibbs With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ReactorConvEqGibbs With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
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
            Return My.Resources.re_conv_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CONV_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CONV_Name")
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
    End Class

End Namespace
