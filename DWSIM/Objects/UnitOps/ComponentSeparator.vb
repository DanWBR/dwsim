'    Component Separator Calculation Routines 
'    Copyright 2010 Daniel Wagner O. de Medeiros
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

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports DWSIM.DWSIM.SimulationObjects.Streams
Imports DWSIM.DWSIM.SimulationObjects.UnitOps.Auxiliary
Imports System.Linq

Namespace DWSIM.SimulationObjects.UnitOps.Auxiliary

    Public Enum SeparationSpec
        MassFlow
        MolarFlow
        PercentInletMassFlow
        PercentInletMolarFlow
    End Enum

    <System.Serializable()> Public Class ComponentSeparationSpec

        Private _compID As String
        Private _sepspec As SeparationSpec = SeparationSpec.PercentInletMassFlow
        Private _specvalue As Double = 0
        Private _specunit As String = ""

        Public Property ComponentID() As String
            Get
                Return _compID
            End Get
            Set(ByVal value As String)
                _compID = value
            End Set
        End Property

        Public Property SepSpec() As SeparationSpec
            Get
                Return _sepspec
            End Get
            Set(ByVal value As SeparationSpec)
                _sepspec = value
            End Set
        End Property

        Public Property SpecValue() As Double
            Get
                Return _specvalue
            End Get
            Set(ByVal value As Double)
                _specvalue = value
            End Set
        End Property

        Public Property SpecUnit() As String
            Get
                Return _specunit
            End Get
            Set(ByVal value As String)
                _specunit = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Sub New(ByVal id As String, ByVal spectype As Auxiliary.SeparationSpec, ByVal specvalue As Double, ByVal specunit As String)
            Me.ComponentID = id
            Me.SepSpec = spectype
            Me.SpecValue = specvalue
            Me.SpecUnit = specunit
        End Sub

    End Class

End Namespace

Namespace DWSIM.SimulationObjects.UnitOps

    <System.Serializable()> Public Class ComponentSeparator

        Inherits SimulationObjects_UnitOpBaseClass

        Protected m_ei As Double
        Protected _compsepspeccollection As Dictionary(Of String, ComponentSeparationSpec)
        Protected _streamindex As Byte = 0

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Me.ComponentSepSpecs = New Dictionary(Of String, ComponentSeparationSpec)

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "SeparationSpecs").SingleOrDefault.Elements.ToList
                Dim spec As New ComponentSeparationSpec With {.ComponentID = xel.@CompID, .SepSpec = [Enum].Parse(Type.GetType("DWSIM.DWSIM.SimulationObjects.UnitOps.Auxiliary.SeparationSpec"), xel.@SepSpec), .SpecUnit = xel.@SpecUnit, .SpecValue = xel.@SpecValue}
                _compsepspeccollection.Add(xel.@ID, spec)
            Next

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("SeparationSpecs"))
                For Each kvp As KeyValuePair(Of String, ComponentSeparationSpec) In _compsepspeccollection
                    .Item(.Count - 1).Add(New XElement("SeparationSpec", New XAttribute("ID", kvp.Key),
                                                       New XAttribute("CompID", kvp.Value.ComponentID),
                                                       New XAttribute("SepSpec", kvp.Value.SepSpec),
                                                       New XAttribute("SpecUnit", kvp.Value.SpecUnit),
                                                       New XAttribute("SpecValue", kvp.Value.SpecValue.ToString(ci))))
                Next
            End With

            Return elements

        End Function

        Public Property SpecifiedStreamIndex() As Byte
            Get
                Return _streamindex
            End Get
            Set(ByVal value As Byte)
                _streamindex = value
            End Set
        End Property

        Public Property ComponentSepSpecs() As Dictionary(Of String, ComponentSeparationSpec)
            Get
                Return _compsepspeccollection
            End Get
            Set(ByVal value As Dictionary(Of String, ComponentSeparationSpec))
                _compsepspeccollection = value
            End Set
        End Property

        Public Property EnergyImb() As Double
            Get
                Return m_ei
            End Get
            Set(ByVal value As Double)
                m_ei = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal nome As String, ByVal descricao As String)

            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
            Me.ShowQuickTable = True
            Me.ComponentSepSpecs = New Dictionary(Of String, ComponentSeparationSpec)

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As FormFlowsheet = Me.FlowSheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            Dim su As SistemasDeUnidades.Unidades = form.Options.SelectedUnitSystem
            Dim cv As New SistemasDeUnidades.Conversor

            Dim instr, outstr1, outstr2, specstr, otherstr As Streams.MaterialStream
            instr = FlowSheet.Collections.ObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            outstr1 = FlowSheet.Collections.ObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
            outstr2 = FlowSheet.Collections.ObjectCollection(Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)

            'get component ids

            Dim namesS, namesC, toremove As New ArrayList

            For Each sb As Substancia In instr.Fases(0).Componentes.Values
                namesS.Add(sb.Nome)
            Next
            For Each cs As ComponentSeparationSpec In Me.ComponentSepSpecs.Values
                namesC.Add(cs.ComponentID)
            Next

            If namesC.Count > namesS.Count Then

            ElseIf namesC.Count < namesS.Count Then

            End If

            If Me.SpecifiedStreamIndex = 0 Then
                specstr = outstr1
                otherstr = outstr2
            Else
                specstr = outstr2
                otherstr = outstr1
            End If

            'separate components according to specifications

            For Each cs As ComponentSeparationSpec In Me.ComponentSepSpecs.Values
                If specstr.Fases(0).Componentes.ContainsKey(cs.ComponentID) Then
                    With specstr.Fases(0).Componentes(cs.ComponentID)
                        Select Case cs.SepSpec
                            Case SeparationSpec.MassFlow
                                .MassFlow = Conversor.ConverterParaSI(su.spmp_massflow, cs.SpecValue)
                                .MolarFlow = .MassFlow / .ConstantProperties.Molar_Weight * 1000
                            Case SeparationSpec.MolarFlow
                                .MolarFlow = Conversor.ConverterParaSI(su.spmp_molarflow, cs.SpecValue)
                                .MassFlow = .MolarFlow * .ConstantProperties.Molar_Weight / 1000
                            Case SeparationSpec.PercentInletMassFlow
                                Dim mf As Double = instr.Fases(0).Componentes(cs.ComponentID).MassFlow.GetValueOrDefault
                                If mf <> 0.0# Then
                                    .MassFlow = cs.SpecValue / 100 * mf
                                Else
                                    .MassFlow = 0.0#
                                End If
                                .MolarFlow = .MassFlow / .ConstantProperties.Molar_Weight * 1000
                            Case SeparationSpec.PercentInletMolarFlow
                                Dim mf As Double = instr.Fases(0).Componentes(cs.ComponentID).MolarFlow.GetValueOrDefault
                                If mf <> 0.0# Then
                                    .MassFlow = cs.SpecValue / 100 * mf
                                Else
                                    .MassFlow = 0.0#
                                End If
                                .MassFlow = .MolarFlow * .ConstantProperties.Molar_Weight / 1000
                        End Select
                        CheckSpec(.MolarFlow, False, "component molar flow: " & .ConstantProperties.Name.ToString.ToLower)
                        CheckSpec(.MassFlow, False, "component mass flow: " & .ConstantProperties.Name.ToString.ToLower)
                    End With
                    With otherstr.Fases(0).Componentes(cs.ComponentID)
                        .MassFlow = instr.Fases(0).Componentes(cs.ComponentID).MassFlow.GetValueOrDefault - specstr.Fases(0).Componentes(cs.ComponentID).MassFlow.GetValueOrDefault
                        If .MassFlow < 0.0# Then .MassFlow = 0.0#
                        .MolarFlow = instr.Fases(0).Componentes(cs.ComponentID).MolarFlow.GetValueOrDefault - specstr.Fases(0).Componentes(cs.ComponentID).MolarFlow.GetValueOrDefault
                        If .MolarFlow < 0.0# Then .MolarFlow = 0.0#
                    End With
                Else
                    toremove.Add(cs.ComponentID)
                End If
            Next

            For Each cs As String In toremove
                If Me.ComponentSepSpecs.ContainsKey(cs) Then Me.ComponentSepSpecs.Remove(cs)
            Next

            Dim summ, sumw As Double

            summ = 0
            sumw = 0
            For Each sb As Substancia In specstr.Fases(0).Componentes.Values
                summ += sb.MolarFlow.GetValueOrDefault
                sumw += sb.MassFlow.GetValueOrDefault
            Next
            specstr.Fases(0).SPMProperties.molarflow = summ
            specstr.Fases(0).SPMProperties.massflow = sumw
            For Each sb As Substancia In specstr.Fases(0).Componentes.Values
                If summ <> 0.0# Then sb.FracaoMolar = sb.MolarFlow.GetValueOrDefault / summ Else sb.FracaoMolar = 0.0#
                If sumw <> 0.0# Then sb.FracaoMassica = sb.MassFlow.GetValueOrDefault / sumw Else sb.FracaoMassica = 0.0#
            Next
            summ = 0
            sumw = 0
            For Each sb As Substancia In otherstr.Fases(0).Componentes.Values
                summ += sb.MolarFlow.GetValueOrDefault
                sumw += sb.MassFlow.GetValueOrDefault
            Next
            otherstr.Fases(0).SPMProperties.molarflow = summ
            otherstr.Fases(0).SPMProperties.massflow = sumw
            For Each sb As Substancia In otherstr.Fases(0).Componentes.Values
                If summ <> 0.0# Then sb.FracaoMolar = sb.MolarFlow.GetValueOrDefault / summ Else sb.FracaoMolar = 0.0#
                If sumw <> 0.0# Then sb.FracaoMassica = sb.MassFlow.GetValueOrDefault / sumw Else sb.FracaoMassica = 0.0#
            Next

            'pass conditions

            outstr1.Fases(0).SPMProperties.temperature = instr.Fases(0).SPMProperties.temperature.GetValueOrDefault
            outstr1.Fases(0).SPMProperties.pressure = instr.Fases(0).SPMProperties.pressure.GetValueOrDefault
            outstr2.Fases(0).SPMProperties.temperature = instr.Fases(0).SPMProperties.temperature.GetValueOrDefault
            outstr2.Fases(0).SPMProperties.pressure = instr.Fases(0).SPMProperties.pressure.GetValueOrDefault

            Dim Hi, Ho1, Ho2, Wi, Wo1, Wo2 As Double

            Hi = instr.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
            Wi = instr.Fases(0).SPMProperties.massflow.GetValueOrDefault

            Wo1 = outstr1.Fases(0).SPMProperties.massflow.GetValueOrDefault
            Wo2 = outstr2.Fases(0).SPMProperties.massflow.GetValueOrDefault

            CheckSpec(Hi, False, "inlet enthalpy")
            CheckSpec(Wi, True, "inlet mass flow")
            CheckSpec(Wo1, True, "outlet mass flow")
            CheckSpec(Wo1, True, "outlet mass flow")

            'do a flash calculation on streams to calculate energy imbalance

            If Wo1 <> 0.0# Then
                outstr1.PropertyPackage.CurrentMaterialStream = outstr1
                outstr1.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                Ho1 = outstr1.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
            End If

            If Wo2 <> 0.0# Then
                outstr2.PropertyPackage.CurrentMaterialStream = outstr2
                outstr2.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                Ho2 = outstr2.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
            End If

            'calculate imbalance

            EnergyImb = Hi * Wi - Ho1 * Wo1 - Ho2 * Wo2

            CheckSpec(EnergyImb, False, "energy balance")

            'update energy stream power value

            With form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                .Energia = EnergyImb
                .GraphicObject.Calculated = True
            End With

            'call the flowsheet calculator

            With objargs
                .Calculado = True
                .Nome = Me.Nome
                .Tag = Me.GraphicObject.Tag
                .Tipo = Me.GraphicObject.TipoObjeto
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            'If Not Me.GraphicObject.InputConnectors(0).IsAttached Then Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac10"))
            'If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac11"))
            'If Not Me.GraphicObject.OutputConnectors(1).IsAttached Then Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac11"))

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet

            Dim j As Integer = 0

            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.enthalpy = Nothing
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        comp.FracaoMassica = 0
                        j += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.enthalpy = Nothing
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        comp.FracaoMassica = 0
                        j += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            'Corrente de energia - atualizar valor da potência (kJ/s)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                    .Energia = Nothing
                    .GraphicObject.Calculated = False
                End With
            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Nome = Me.Nome
                .Tipo = TipoObjeto.Vessel
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overloads Overrides Sub UpdatePropertyNodes(ByVal su As SistemasDeUnidades.Unidades, ByVal nf As String)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor
            If Me.NodeTableItems Is Nothing Then
                Me.NodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.FillNodeItems()
            End If

            For Each nti As Outros.NodeItem In Me.NodeTableItems.Values
                nti.Value = GetPropertyValue(nti.Text, FlowSheet.Options.SelectedUnitSystem)
                nti.Unit = GetPropertyUnit(nti.Text, FlowSheet.Options.SelectedUnitSystem)
            Next

            If Me.QTNodeTableItems Is Nothing Then
                Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.QTFillNodeItems()
            End If

            With Me.QTNodeTableItems

                Me.ShowQuickTable = True

                Dim valor As String
                valor = Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.EnergyImb), nf)

                .Item(0).Value = valor
                .Item(0).Unit = su.spmp_heatflow

            End With

        End Sub

        Public Overrides Sub QTFillNodeItems()

            With Me.QTNodeTableItems

                .Clear()

                .Add(0, New DWSIM.Outros.NodeItem(DWSIM.App.GetLocalString("CSepEnergyImbalance"), "", "", 0, 0, ""))

            End With

        End Sub

        Public Overrides Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

            MyBase.PropertyValueChanged(s, e)

            If FlowSheet.Options.CalculatorActivated Then

                'Call function to calculate flowsheet
                Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                With objargs
                    .Tag = Me.GraphicObject.Tag
                    .Calculado = False
                    .Nome = Me.GraphicObject.Name
                    .Tipo = Me.GraphicObject.TipoObjeto
                    .Emissor = "PropertyGrid"
                End With

                If Me.IsSpecAttached = True And Me.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.CLCS_SpecCollection(Me.AttachedSpecId).Calculate()
                FlowSheet.CalculationQueue.Enqueue(objargs)

            End If

        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent, saida1, saida2, en As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent = ""
                End If
                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida1 = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida1 = ""
                End If
                If Me.GraphicObject.OutputConnectors(1).IsAttached = True Then
                    saida2 = Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
                Else
                    saida2 = ""
                End If
                If Me.GraphicObject.EnergyConnector.IsAttached = True Then
                    en = Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
                Else
                    en = ""
                End If

                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("OutletStream1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("OutletStream2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), en, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("CSepSpecStream"), Me, "SpecifiedStreamIndex", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = 0
                End With

                .Item.Add(DWSIM.App.GetLocalString("CSepSeparationSpecs"), Me, "ComponentSepSpecs", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.ComponentSeparator.UICSepSpecEditor
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("CSepEnergyImbalance"), su.spmp_heatflow), Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.EnergyImb), FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)

                .ExpandAllGridItems()

            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As Double = 0
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0

                    value = Conversor.ConverterDoSI(su.spmp_heatflow, Me.EnergyImb)

            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RW
                Case PropertyType.WR
                Case PropertyType.ALL
                    For i = 0 To 0
                        proplist.Add("PROP_CP_" + CStr(i))
                    Next
                Case PropertyType.RO
                    For i = 0 To 0
                        proplist.Add("PROP_CP_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

            End Select

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As String = ""
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0

                    value = su.spmp_heatflow

            End Select

            Return value
        End Function
    End Class

End Namespace