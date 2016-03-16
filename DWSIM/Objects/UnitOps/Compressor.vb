'    Compressor Calculation Routines 
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

    <System.Serializable()> Public Class Compressor

        Inherits SimulationObjects_UnitOpBaseClass

        Public Enum CalculationMode
            OutletPressure = 0
            Delta_P = 1
            EnergyStream = 2
        End Enum

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)
        Protected m_POut As Nullable(Of Double) = 101325.0#
        Protected m_cmode As CalculationMode = CalculationMode.Delta_P

        Protected m_eta_a As Nullable(Of Double) = 75.0#
        Protected m_eta_p As Nullable(Of Double) = Nothing

        Protected m_ignorephase As Boolean = True

        Public Property CalcMode() As CalculationMode
            Get
                Return m_cmode
            End Get
            Set(ByVal value As CalculationMode)
                m_cmode = value
            End Set
        End Property

        Public Property IgnorePhase() As Boolean
            Get
                Return m_ignorephase
            End Get
            Set(ByVal value As Boolean)
                m_ignorephase = value
            End Set
        End Property

        Public Sub New(ByVal nome As String, ByVal descricao As String)
            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
        End Sub

        Public Property EficienciaPolitropica() As Nullable(Of Double)
            Get
                Return m_eta_p
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_eta_p = value
            End Set
        End Property

        Public Property EficienciaAdiabatica() As Nullable(Of Double)
            Get
                Return m_eta_a
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_eta_a = value
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

        Public Property POut() As Nullable(Of Double)
            Get
                Return m_POut
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_POut = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            If Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.Compressor
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedeenergia"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.Compressor
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.Compressor
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            End If

            Dim Ti, Pi, Hi, Si, Wi, rho_vi, qvi, qli, ei, ein, T2, P2, H2, cpig, cp, cv, mw As Double

            qli = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(1).SPMProperties.volumetric_flow.GetValueOrDefault.ToString

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            If Not IgnorePhase And DebugMode Then AppendDebugLine("Checking if there is a liquid phase in the inlet stream...")

            If qli > 0 And Not Me.IgnorePhase Then Throw New Exception(DWSIM.App.GetLocalString("Existeumafaselquidan"))

            If form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name).GraphicObject.Active Then

                If Me.CalcMode <> CalculationMode.EnergyStream And form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name).GraphicObject.InputConnectors(0).IsAttached = False Then GoTo fix

                Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
                Ti = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.temperature.GetValueOrDefault.ToString
                Pi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.pressure.GetValueOrDefault.ToString
                rho_vi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(2).SPMProperties.density.GetValueOrDefault.ToString
                cpig = Me.PropertyPackage.AUX_CPm(PropertyPackages.Fase.Vapor, Ti)
                cp = Me.PropertyPackage.DW_CalcCp_ISOL(PropertyPackages.Fase.Vapor, Ti, Pi)
                cv = Me.PropertyPackage.DW_CalcCv_ISOL(PropertyPackages.Fase.Vapor, Ti, Pi)
                mw = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.molecularWeight.GetValueOrDefault
                qvi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault.ToString
                Hi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.enthalpy.GetValueOrDefault.ToString
                Si = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.entropy.GetValueOrDefault.ToString
                Wi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.massflow.GetValueOrDefault.ToString
                ei = Hi * Wi
                ein = ei

                If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
                If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))
                If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, S = {3} kJ/[kg.K], W = {4} kg/s, cp = {5} kJ/[kg.K]", Ti, Pi, Hi, Si, Wi, cp))

                'Corrente de energia - pegar valor do DH
                With form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                    Me.DeltaQ = .Energia.GetValueOrDefault 'Wi * (H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100)
                End With

                If DebugMode Then AppendDebugLine(String.Format("Power from energy stream: {0} kW", DeltaQ))

                CheckSpec(Me.DeltaQ, True, "power")

                Dim k As Double = cp / cv

                P2 = Pi * ((1 + DeltaQ.GetValueOrDefault * (Me.EficienciaAdiabatica.GetValueOrDefault / 100) / Wi * (k - 1) / k * mw / 8.314 / Ti)) ^ (k / (k - 1))

                If DebugMode Then AppendDebugLine(String.Format("Calculated outlet pressure: {0} Pa", P2))

                CheckSpec(P2, True, "outlet pressure")

                DeltaP = P2 - Pi
                POut = P2

                CheckSpec(Si, False, "inlet entropy")

                If DebugMode Then AppendDebugLine(String.Format("Doing a PS flash to calculate ideal outlet enthalpy... P = {0} Pa, S = {1} kJ/[kg.K]", P2, Si))

                Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.S, P2, Si, 0)

                If DebugMode Then AppendDebugLine(String.Format("Calculated ideal outlet enthalpy Hid = {0} kJ/kg", tmp(4)))

                H2 = Hi + (tmp(4) - Hi) / (Me.EficienciaAdiabatica.GetValueOrDefault / 100)

                If DebugMode Then AppendDebugLine(String.Format("Calculated real outlet enthalpy Hr = {0} kJ/kg", H2))

                CheckSpec(H2, False, "outlet enthalpy")

                If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)

                T2 = tmp(2)

                If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                CheckSpec(T2, True, "outlet temperature")

                Me.DeltaT = T2 - Ti

                If Not DebugMode Then

                    'Atribuir valores à corrente de matéria conectada à jusante
                    With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                        .Fases(0).SPMProperties.temperature = T2
                        .Fases(0).SPMProperties.pressure = P2
                        .Fases(0).SPMProperties.enthalpy = H2
                        Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                        For Each comp In .Fases(0).Componentes.Values
                            comp.FracaoMolar = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMolar
                            comp.FracaoMassica = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMassica
                        Next
                        .Fases(0).SPMProperties.massflow = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.massflow.GetValueOrDefault
                    End With

                End If

            Else

fix:            Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
                Ti = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.temperature.GetValueOrDefault.ToString
                Pi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.pressure.GetValueOrDefault.ToString
                rho_vi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(2).SPMProperties.density.GetValueOrDefault.ToString
                qvi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault.ToString
                Hi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.enthalpy.GetValueOrDefault.ToString
                Si = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.entropy.GetValueOrDefault.ToString
                Wi = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.massflow.GetValueOrDefault.ToString
                ei = Hi * Wi
                ein = ei

                If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
                If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))
                If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, S = {3} kJ/[kg.K], W = {4} kg/s", Ti, Pi, Hi, Si, Wi, cp))

                Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)

                Select Case Me.CalcMode
                    Case CalculationMode.Delta_P
                        P2 = Pi + Me.DeltaP.GetValueOrDefault
                        POut = P2
                    Case CalculationMode.OutletPressure
                        P2 = Me.POut.GetValueOrDefault
                        DeltaP = P2 - Pi
                End Select

                CheckSpec(Si, False, "inlet entropy")

                If DebugMode Then AppendDebugLine(String.Format("Doing a PS flash to calculate ideal outlet enthalpy... P = {0} Pa, S = {1} kJ/[kg.K]", P2, Si))

                Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.S, P2, Si, 0)
                T2 = tmp(2)
                H2 = tmp(4)

                If DebugMode Then AppendDebugLine(String.Format("Calculated ideal outlet enthalpy Hid = {0} kJ/kg", tmp(4)))

                CheckSpec(T2, True, "outlet temperature")
                CheckSpec(H2, False, "outlet enthalpy")

                Me.DeltaQ = Wi * (H2 - Hi) / (Me.EficienciaAdiabatica.GetValueOrDefault / 100)

                If DebugMode Then AppendDebugLine(String.Format("Calculated real compressor power = {0} kW", DeltaQ))

                CheckSpec(DeltaQ, True, "power")

                If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, Hi + Me.DeltaQ.GetValueOrDefault / Wi))

                tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, Hi + Me.DeltaQ.GetValueOrDefault / Wi, T2)
                T2 = tmp(2)
                Me.DeltaT = T2 - Ti

                If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                CheckSpec(T2, True, "outlet temperature")

                H2 = Hi + Me.DeltaQ.GetValueOrDefault / Wi

                If Not DebugMode Then

                    'Atribuir valores à corrente de matéria conectada à jusante
                    With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                        .Fases(0).SPMProperties.temperature = T2
                        .Fases(0).SPMProperties.pressure = P2
                        .Fases(0).SPMProperties.enthalpy = H2
                        Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                        For Each comp In .Fases(0).Componentes.Values
                            comp.FracaoMolar = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMolar
                            comp.FracaoMassica = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMassica
                        Next
                        .Fases(0).SPMProperties.massflow = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.massflow.GetValueOrDefault
                    End With

                    'Corrente de energia - atualizar valor da potência (kJ/s)
                    With form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                        .Energia = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                End If

            End If

            If DebugMode Then AppendDebugLine("Calculation finished successfully.")

            If Not DebugMode Then

                'Call function to calculate flowsheet
                With objargs
                    .Calculado = True
                    .Nome = Me.Nome
                    .Tag = Me.GraphicObject.Tag
                    .Tipo = TipoObjeto.Compressor
                End With
                form.CalculationQueue.Enqueue(objargs)

            End If

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet

            'Zerar valores da corrente de matéria conectada a jusante
            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                Dim msj As DWSIM.SimulationObjects.Streams.MaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                With msj
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
                .Tipo = TipoObjeto.Compressor
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

                .Item.Add(DWSIM.App.GetLocalString("HeaterCoolerCalcMode"), Me, "CalcMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "CalcMode"

                Select Case Me.CalcMode
                    Case CalculationMode.Delta_P
                        Dim valor = Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT("Delta P", su.spmp_deltaP), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_CO_0"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.spmp_deltaP, "DP"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With
                    Case CalculationMode.OutletPressure
                        Dim valor = Format(Conversor.ConverterDoSI(su.spmp_pressure, Me.POut.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(DWSIM.App.GetLocalString("Presso"), su.spmp_pressure), Double.Parse(valor), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Pressoajusante"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_CO_4"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.spmp_pressure, "P"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With
                End Select

                .Item.Add(DWSIM.App.GetLocalString("EficinciaAdiabtica01"), Me, "EficienciaAdiabatica", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Eficinciadocompresso"), True)
                With .Item(.Item.Count - 1)
                    .Tag2 = "PROP_CO_1"
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(DWSIM.App.GetLocalString("IgnorarLquidonaEntra"), Me, "IgnorePhase", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("SelecioLiquidrueparaign2"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(Boolean)
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.spmp_deltaT), Format(Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT.GetValueOrDefault), Me.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(FT(DWSIM.App.GetLocalString("Energianecessria"), su.spmp_heatflow), Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ.GetValueOrDefault), Me.FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Potnciarequeridapelo"), True)
                With .Item(.Item.Count - 1)
                    .Tag2 = "PROP_CO_3"
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
                    'PROP_CO_0	Pressure Increase (Head)
                    value = Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP.GetValueOrDefault)
                Case 1
                    'PROP_CO_1(Efficiency)
                    value = Me.EficienciaAdiabatica.GetValueOrDefault
                Case 2
                    'PROP_CO_2(Delta - T)
                    value = Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT.GetValueOrDefault)
                Case 3
                    'PROP_CO_3	Power Required
                    value = Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ.GetValueOrDefault)
                Case 4
                    'PROP_CO_4	Pressure Out
                    value = Conversor.ConverterDoSI(su.spmp_pressure, Me.POut.GetValueOrDefault)

            End Select

            Return value


        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 2 To 3
                        proplist.Add("PROP_CO_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 4
                        proplist.Add("PROP_CO_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 1
                        proplist.Add("PROP_CO_" + CStr(i))
                    Next
                    proplist.Add("PROP_CO_4")
                Case PropertyType.ALL
                    For i = 0 To 4
                        proplist.Add("PROP_CO_" + CStr(i))
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
                    'PROP_CO_0	Pressure Increase (Head)
                    Me.DeltaP = Conversor.ConverterParaSI(su.spmp_deltaP, propval)
                Case 1
                    'PROP_CO_1(Efficiency)
                    Me.EficienciaAdiabatica = propval
                Case 4
                    'PROP_CO_4(Pressure Out)
                    Me.POut = Conversor.ConverterParaSI(su.spmp_pressure, propval)
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
                    'PROP_CO_0	Pressure Increase (Head)
                    value = su.spmp_deltaP
                Case 1
                    'PROP_CO_1(Efficiency)
                    value = ""
                Case 2
                    'PROP_CO_2(Delta - T)
                    value = su.spmp_deltaT
                Case 3
                    'PROP_CO_3	Power Required
                    value = su.spmp_heatflow
                Case 4
                    'PROP_CO_4	Pressure Out
                    value = su.spmp_pressure
            End Select

            Return value
        End Function
    End Class

End Namespace