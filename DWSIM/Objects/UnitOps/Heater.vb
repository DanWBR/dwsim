'    Heater Calculation Routines 
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

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.SimulationObjects.UnitOps

    <System.Serializable()> Public Class Heater

        Inherits SimulationObjects_UnitOpBaseClass

        Public Enum CalculationMode
            HeatAdded = 0
            OutletTemperature = 1
            EnergyStream = 2
            OutletVaporFraction = 3
        End Enum

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)
        Protected m_Tout As Nullable(Of Double) = 298.15#
        Protected m_VFout As Nullable(Of Double)
        Protected m_cmode As CalculationMode = CalculationMode.HeatAdded

        Protected m_eta As Nullable(Of Double) = 100

        Protected m_FixOnHeat As Boolean = True

        Public Property FixOnHeat() As Boolean
            Get
                Return m_FixOnHeat
            End Get
            Set(ByVal value As Boolean)
                m_FixOnHeat = value
            End Set
        End Property

        Public Sub New(ByVal nome As String, ByVal descricao As String)
            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
        End Sub

        Public Property Eficiencia() As Nullable(Of Double)
            Get
                Return m_eta
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_eta = value
            End Set
        End Property

        Public Property OutletVaporFraction() As Nullable(Of Double)
            Get
                Return m_VFout
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_VFout = value
            End Set
        End Property

        Public Property OutletTemperature() As Nullable(Of Double)
            Get
                Return m_Tout
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_Tout = value
            End Set
        End Property

        Public Property CalcMode() As CalculationMode
            Get
                Return m_cmode
            End Get
            Set(ByVal value As CalculationMode)
                m_cmode = value
            End Set
        End Property


        Public Property DeltaP() As Nullable(Of Double)
            Get
                Return m_dp
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dp = value
            End Set
        End Property

        Public Property DeltaT() As Nullable(Of Double)
            Get
                Return m_dt
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dt = value
            End Set
        End Property

        Public Property DeltaQ() As Nullable(Of Double)
            Get
                Return m_DQ
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_DQ = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            Dim FlashSpec As Integer = Streams.MaterialStream.Flashspec.Temperature_and_Pressure

            If Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.Heater
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedeenergia2"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.Heater
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.Heater
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            End If

            form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Validate()

            Dim Ti, Pi, Hi, Wi, ei, ein, T2, P2, H2, V2 As Double

            Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            Ti = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.temperature.GetValueOrDefault
            Pi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.pressure.GetValueOrDefault
            Hi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.enthalpy.GetValueOrDefault
            Wi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.massflow.GetValueOrDefault
            ei = Hi * Wi
            ein = ei

            Dim es As DWSIM.SimulationObjects.Streams.EnergyStream = form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)

            P2 = Pi - Me.DeltaP.GetValueOrDefault

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
            If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, W = {3} kg/s", Ti, Pi, Hi, Wi))

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            Select Case Me.CalcMode

                Case CalculationMode.HeatAdded

                    FlashSpec = Streams.MaterialStream.Flashspec.Pressure_and_Enthalpy
                    H2 = Me.DeltaQ.GetValueOrDefault * (Me.Eficiencia.GetValueOrDefault / 100) / Wi + Hi

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, 0)
                    T2 = tmp(2)
                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    'Corrente de energia - atualizar valor da potência (kJ/s)
                    With es
                        .Energia = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                Case CalculationMode.OutletTemperature

                    FlashSpec = Streams.MaterialStream.Flashspec.Temperature_and_Pressure
                    T2 = Me.OutletTemperature.GetValueOrDefault

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate outlet enthalpy... P = {0} Pa, T = {1} K", P2, T2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P, T2, P2, 0)
                    H2 = tmp(4)
                    CheckSpec(H2, False, "outlet enthalpy")
                    Me.DeltaT = T2 - Ti
                    Me.DeltaQ = (H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    'Corrente de energia - atualizar valor da potência (kJ/s)
                    With es
                        .Energia = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                Case CalculationMode.EnergyStream

                    FlashSpec = Streams.MaterialStream.Flashspec.Pressure_and_Enthalpy
                    Me.DeltaQ = es.Energia.GetValueOrDefault
                    H2 = Me.DeltaQ.GetValueOrDefault * (Me.Eficiencia.GetValueOrDefault / 100) / Wi + Hi

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")
                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                Case CalculationMode.OutletVaporFraction

                    FlashSpec = Streams.MaterialStream.Flashspec.Pressure_and_VaporFraction
                    V2 = m_VFout.GetValueOrDefault

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PVF flash to calculate outlet temperature... P = {0} Pa, VF = {1}", P2, V2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.VAP, P2, m_VFout.GetValueOrDefault, Ti)
                    H2 = tmp(4)
                    CheckSpec(H2, False, "outlet enthalpy")
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")
                    Me.DeltaT = T2 - Ti
                    Me.DeltaQ = (H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    'Corrente de energia - atualizar valor da potência (kJ/s)
                    With es
                        .Energia = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

            End Select

            If Not DebugMode Then

                'Atribuir valores à corrente de matéria conectada à jusante
                With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .SpecType = FlashSpec
                    .Fases(0).SPMProperties.temperature = T2
                    .Fases(0).SPMProperties.pressure = P2
                    .Fases(0).SPMProperties.enthalpy = H2
                    .Fases(2).SPMProperties.molarfraction = V2
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMolar
                        comp.FracaoMassica = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMassica
                    Next
                    .Fases(0).SPMProperties.massflow = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.massflow.GetValueOrDefault
                End With

                'Call function to calculate flowsheet
                With objargs
                    .Calculado = True
                    .Nome = Me.Nome
                    .Tag = Me.GraphicObject.Tag
                    .Tipo = TipoObjeto.Heater
                End With

                form.CalculationQueue.Enqueue(objargs)

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de matéria conectada a jusante
                With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.enthalpy = Nothing
                    .Fases(0).SPMProperties.molarfraction = 1
                    .Fases(0).SPMProperties.massfraction = 1
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    Dim i As Integer = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        comp.FracaoMassica = 0
                        i += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.molarflow = Nothing
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
                .Tag = Me.GraphicObject.Tag
                .Tipo = TipoObjeto.Heater
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

                Dim valor As String

                If Me.DeltaP.HasValue Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP), nf)
                Else
                    valor = DWSIM.App.GetLocalString("NC")
                End If
                .Item(0).Value = valor
                .Item(0).Unit = su.spmp_deltaP

                If Me.DeltaT.HasValue Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT), nf)
                Else
                    valor = DWSIM.App.GetLocalString("NC")
                End If
                .Item(1).Value = valor
                .Item(1).Unit = su.spmp_deltaT

                If Me.DeltaQ.HasValue Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ), nf)
                Else
                    valor = DWSIM.App.GetLocalString("NC")
                End If
                .Item(2).Value = valor
                .Item(2).Unit = su.spmp_heatflow

            End With

        End Sub

        Public Overrides Sub QTFillNodeItems()

            With Me.QTNodeTableItems

                .Clear()

                .Add(0, New DWSIM.Outros.NodeItem("DP", "", "", 0, 0, ""))
                .Add(1, New DWSIM.Outros.NodeItem("DT", "", "", 1, 0, ""))
                .Add(2, New DWSIM.Outros.NodeItem("Pot", "", "", 2, 0, ""))

            End With
        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent, saida, energ As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent = ""
                End If
                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida = ""
                End If

                If Me.GraphicObject.InputConnectors(1).IsAttached = True Then
                    energ = Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
                Else
                    energ = ""
                End If

                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
                End With

                Dim valor = Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.spmp_deltaP), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad3"), True)
                With .Item(.Item.Count - 1)
                    .CustomTypeConverter = New System.ComponentModel.StringConverter
                    .Tag2 = "PROP_HT_0"
                    .Tag = New Object() {FlowSheet.Options.NumberFormat, su.spmp_deltaP, "DP"}
                    .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                End With


                .Item.Add(DWSIM.App.GetLocalString("HeaterCoolerCalcMode"), Me, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "CalcMode"

                Select Case Me.CalcMode

                    Case CalculationMode.EnergyStream

                    Case CalculationMode.HeatAdded

                        valor = Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("CalorFornecido"), su.spmp_heatflow), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quantidadedecalorced"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_HT_3"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.spmp_heatflow, "E"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                    Case CalculationMode.OutletTemperature

                        valor = Format(Conversor.ConverterDoSI(su.spmp_temperature, Me.OutletTemperature.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.spmp_temperature), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_HT_2"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.spmp_temperature, "T"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                    Case CalculationMode.OutletVaporFraction

                        valor = Format(Me.OutletVaporFraction.GetValueOrDefault, FlowSheet.Options.NumberFormat)
                        .Item.Add(DWSIM.App.GetLocalString("FraomolardafaseFaseV"), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                        With .Item(.Item.Count - 1)
                            .Tag2 = "PROP_HT_4"
                        End With

                End Select

                .Item.Add(DWSIM.App.GetLocalString("Eficincia0100"), Me, "Eficiencia", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciadoaquecedor"), True)
                With .Item(.Item.Count - 1)
                    .Tag2 = "PROP_HT_1"
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                If Me.GraphicObject.Calculated And Not Me.CalcMode = CalculationMode.HeatAdded Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("CalorFornecido"), su.spmp_heatflow), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Quantidadedecalorced"), True)
                End If

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.spmp_deltaT), Format(Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                If Me.GraphicObject.Calculated = False Then
                    .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), Me, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                    With .Item(.Item.Count - 1)
                        .DefaultType = GetType(System.String)
                    End With
                End If



            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object

            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As Double = 0
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_HT_0	Pressure Drop
                    value = Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP.GetValueOrDefault)
                Case 1
                    'PROP_HT_1(Efficiency)
                    value = Me.Eficiencia.GetValueOrDefault
                Case 2
                    'PROP_HT_2	Outlet Temperature
                    value = Conversor.ConverterDoSI(su.spmp_temperature, Me.OutletTemperature.GetValueOrDefault)
                Case 3
                    'PROP_HT_3	Heat Added
                    value = Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ.GetValueOrDefault)
                Case 4
                    'PROP_HT_4 Outlet molar vapour fraction
                    value = Me.OutletVaporFraction.GetValueOrDefault
                Case 5
                    'PROP_HT_5 DeltaT
                    value = Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT.GetValueOrDefault)
            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 5 To 5
                        proplist.Add("PROP_HT_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 5
                        proplist.Add("PROP_HT_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 4
                        proplist.Add("PROP_HT_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 5
                        proplist.Add("PROP_HT_" + CStr(i))
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

                Case 0
                    'PROP_HT_0	Pressure Drop
                    Me.DeltaP = Conversor.ConverterParaSI(su.spmp_deltaP, propval)
                Case 1
                    'PROP_HT_1(Efficiency)
                    Me.Eficiencia = propval
                Case 2
                    'PROP_HT_2	Outlet Temperature
                    Me.OutletTemperature = Conversor.ConverterParaSI(su.spmp_temperature, propval)
                Case 3
                    'PROP_HT_3	Heat Added
                    Me.DeltaQ = Conversor.ConverterParaSI(su.spmp_heatflow, propval)
                Case 4
                    'PROP_HT_3	Outlet molar vapour fraction
                    Me.OutletVaporFraction = propval
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
                    'PROP_HT_0	Pressure Drop
                    value = su.spmp_deltaP
                Case 1
                    'PROP_HT_1(Efficiency)
                    value = ""
                Case 2
                    'PROP_HT_2	Outlet Temperature
                    value = su.spmp_temperature
                Case 3
                    'PROP_HT_3	Heat Added
                    value = su.spmp_heatflow
                Case 4
                    'PROP_HT_4	Outlet vapour fraction
                    value = ""
                Case 5
                    'PROP_HT_5 DeltaT
                    value = su.spmp_deltaT

            End Select

            Return value
        End Function
    End Class

End Namespace
