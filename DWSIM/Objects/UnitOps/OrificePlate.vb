'    ISO 5167 Orifice Plate Calculation Routines 
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
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.SimulationObjects.UnitOps

    <System.Serializable()> Public Class OrificePlate

        Inherits SimulationObjects_UnitOpBaseClass

        Public Enum CalcMethod
            Homogeneous = 0
            Slip = 1
        End Enum

        Public Enum OrificeType
            CornerTaps = 0
            FlangeTaps = 1
            RadiusTaps = 2
        End Enum

        Protected m_dt As Nullable(Of Double)
        Protected _orificeDP As Double = 0
        Protected _fluidDP As Double = 0
        Protected _beta As Double = 0
        Protected _orificediameter As Double = 0
        Protected _orificetype As OrificeType = OrificeType.FlangeTaps
        Protected _calcmethod As CalcMethod
        Protected _corrfactor As Double = 1

        Public Sub New(ByVal nome As String, ByVal descricao As String)

            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()

        End Sub

        Public Property OrifType() As OrificeType
            Get
                Return _orificetype
            End Get
            Set(ByVal value As OrificeType)
                _orificetype = value
            End Set
        End Property

        Public Property OverallPressureDrop() As Double
            Get
                Return _fluidDP
            End Get
            Set(ByVal value As Double)
                _fluidDP = value
            End Set
        End Property

        Public Property OrificePressureDrop() As Double
            Get
                Return _orificeDP
            End Get
            Set(ByVal value As Double)
                _orificeDP = value
            End Set
        End Property

        Public Property CorrectionFactor() As Double
            Get
                Return _corrfactor
            End Get
            Set(ByVal value As Double)
                _corrfactor = value
            End Set
        End Property

        Public Property CalculationMethod() As CalcMethod
            Get
                Return _calcmethod
            End Get
            Set(ByVal value As CalcMethod)
                _calcmethod = value
            End Set
        End Property

        Public Property Beta() As Double
            Get
                Return _beta
            End Get
            Set(ByVal value As Double)
                _beta = value
            End Set
        End Property

        Public Property OrificeDiameter() As Double
            Get
                Return _orificediameter
            End Get
            Set(ByVal value As Double)
                _orificediameter = value
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

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.OrificePlate
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.OrificePlate
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            End If

            Dim Ti, Pi, Wi, T2, P2, H2, H1, xv, xl, wv, wl As Double
            Dim rhom, mum, rhov, rhol, muv, mul As Double

            Dim instr, outstr As Streams.MaterialStream

            instr = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            outstr = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)

            Me.PropertyPackage.CurrentMaterialStream = instr
            Ti = instr.Fases(0).SPMProperties.temperature.GetValueOrDefault
            Pi = instr.Fases(0).SPMProperties.pressure.GetValueOrDefault
            Wi = instr.Fases(0).SPMProperties.massflow.GetValueOrDefault
            H1 = instr.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
            rhom = instr.Fases(0).SPMProperties.density.GetValueOrDefault
            rhov = instr.Fases(2).SPMProperties.density.GetValueOrDefault
            rhol = instr.Fases(1).SPMProperties.density.GetValueOrDefault
            muv = instr.Fases(2).SPMProperties.viscosity.GetValueOrDefault
            mul = instr.Fases(1).SPMProperties.viscosity.GetValueOrDefault
            xv = instr.Fases(2).SPMProperties.molarfraction.GetValueOrDefault
            xl = instr.Fases(1).SPMProperties.molarfraction.GetValueOrDefault
            wv = instr.Fases(2).SPMProperties.massfraction.GetValueOrDefault
            wl = instr.Fases(1).SPMProperties.massfraction.GetValueOrDefault
            If xv > 0 And xl > 0 Then
                mum = (xv / muv + xl / mul) ^ -1
            ElseIf xl = 0 Then
                mum = muv
            ElseIf xv = 0 Then
                mum = mul
            End If

            Dim beta, A1, A2, s2_s1, L1, L2 As Double

            beta = _beta
            A1 = 3.1416 * (_orificediameter / _beta) ^ 2 / 4
            A2 = 3.1416 * (_orificediameter) ^ 2 / 4

            Select Case _orificetype

                Case OrificeType.CornerTaps

                    'placa de orifício corner taps

                    s2_s1 = 0
                    L1 = 0
                    L2 = 0

                Case OrificeType.FlangeTaps

                    'placa de orifício flange taps

                    s2_s1 = 0.0508
                    L1 = 1 / (_orificediameter / 0.0254)
                    L2 = 1 / (_orificediameter / 0.0254)

                Case OrificeType.RadiusTaps

                    'placa de orifício radius taps

                    s2_s1 = 1.5 * _orificediameter
                    L1 = 1
                    L2 = 0.47

            End Select

            Dim ReD, Cd, DP As Double

            ReD = Wi * _orificediameter / (A1 * mum)
            If L1 < 0.4333 Then
                Cd = 0.5959 + 0.312 * beta ^ 2.1 - 0.184 * beta ^ 8 + 0.0029 * beta ^ 2.5 * (10 ^ 6 / ReD) ^ 0.75 + 0.09 * L1 * (beta ^ 4 / (1 - beta ^ 4)) - 0.0337 * L2 * beta ^ 3
            Else
                Cd = 0.5959 + 0.312 * beta ^ 2.1 - 0.184 * beta ^ 8 + 0.0029 * beta ^ 2.5 * (10 ^ 6 / ReD) ^ 0.75 + 0.039 * L1 * (beta ^ 4 / (1 - beta ^ 4)) - 0.0337 * L2 * beta ^ 3
            End If

            DP = (Wi / (_corrfactor * Cd * A2)) ^ 2 * (1 - beta ^ 4) / (2 * rhom)
            DP = DP + (rhom * 9.8 * (s2_s1))

            _orificeDP = DP
            _fluidDP = DP * ((1 - beta ^ 4 * (1 - Cd ^ 2)) ^ 0.5 - Cd * beta ^ 2) / ((1 - beta ^ 4 * (1 - Cd ^ 2)) ^ 0.5 + Cd * beta ^ 2)

            P2 = Pi - _fluidDP
            H2 = H1

            Dim tmp As Object
            tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)
            T2 = tmp(2)

            Me.DeltaT = T2 - Ti

            'Atribuir valores à corrente de matéria conectada à jusante
            With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                .Fases(0).SPMProperties.temperature = T2
                .Fases(0).SPMProperties.pressure = P2
                .Fases(0).SPMProperties.enthalpy = H2
                Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                Dim i As Integer = 0
                For Each comp In .Fases(0).Componentes.Values
                    comp.FracaoMolar = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMolar
                    comp.FracaoMassica = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).Componentes(comp.Nome).FracaoMassica
                    i += 1
                Next
                .Fases(0).SPMProperties.massflow = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Fases(0).SPMProperties.massflow.GetValueOrDefault
            End With

            'Call function to calculate flowsheet
            With objargs
                .Calculado = True
                .Nome = Me.Nome
                .Tag = Me.GraphicObject.Tag
                .Tipo = TipoObjeto.OrificePlate
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function


        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de matéria conectada a jusante
                With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.molarfraction = 1
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.enthalpy = Nothing
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

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Nome = Me.Nome
                .Tipo = TipoObjeto.OrificePlate
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

                valor = Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.OverallPressureDrop), nf)

                .Item(0).Value = valor
                .Item(0).Unit = su.spmp_deltaP

                If Me.DeltaT.HasValue Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT), nf)
                Else
                    valor = DWSIM.App.GetLocalString("NC")
                End If

                .Item(1).Value = valor
                .Item(1).Unit = su.spmp_deltaT

            End With

        End Sub

        Public Overrides Sub QTFillNodeItems()


            With Me.QTNodeTableItems

                .Clear()

                .Add(0, New DWSIM.Outros.NodeItem("DP", "", "", 0, 0, ""))
                .Add(1, New DWSIM.Outros.NodeItem("DT", "", "", 1, 0, ""))

            End With
        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent, saida As String
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

                '.Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                'With .Item(.Item.Count - 1)
                '    .DefaultValue = Nothing
                '    .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
                'End With

                '.Item.Add(DWSIM.App.GetLocalString("OPCalculationMethod"), Me, "CalculationMethod", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("OPOrificeType"), Me, "OrifType", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "OrifType"

                Dim valor As Double = 0

                valor = Conversor.ConverterDoSI(su.diameter, Me.OrificeDiameter)
                .Item.Add(FT(DWSIM.App.GetLocalString("OPOrificeDiameter"), su.diameter), Format(valor, FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).CustomTypeConverter = New System.ComponentModel.StringConverter
                .Item(.Item.Count - 1).Tag2 = "PROP_OP_1"
                .Item.Add(DWSIM.App.GetLocalString("OPBeta"), Format(Me.Beta, FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "PROP_OP_2"
                .Item.Add(DWSIM.App.GetLocalString("OPCorrectionFactor"), Format(Me.CorrectionFactor, FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "PROP_OP_3"

                .Item.Add(FT(DWSIM.App.GetLocalString("OPOrificePressureDrop"), su.spmp_deltaP), Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.OrificePressureDrop), FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                .Item(.Item.Count - 1).Tag2 = "PROP_OP_4"
                .Item.Add(FT(DWSIM.App.GetLocalString("OPOverallPressureDrop"), su.spmp_deltaP), Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.OverallPressureDrop), FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                .Item(.Item.Count - 1).Tag2 = "PROP_OP_5"
                .Item.Add(FT(DWSIM.App.GetLocalString("OPDeltaT"), su.spmp_deltaT), Format(Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                .Item(.Item.Count - 1).Tag2 = "PROP_OP_6"


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
                    'PROP_OP_0	Orifice Type	1
                    value = Me.OrifType
                Case 1
                    'PROP_OP_1	Orifice Diameter	1
                    value = Conversor.ConverterDoSI(su.diameter, Me.OrificeDiameter)
                Case 2
                    'PROP_OP_2	Beta (d/D)	1
                    value = Me.Beta
                Case 3
                    'PROP_OP_3	Correction Factor	1
                    value = Me.CorrectionFactor
                Case 4
                    'PROP_OP_4	Overall Pressure Drop	0
                    value = Conversor.ConverterDoSI(su.spmp_deltaP, Me.OverallPressureDrop)
                Case 5
                    'PROP_OP_5	Orifice Pressure Drop	0
                    value = Conversor.ConverterDoSI(su.spmp_deltaP, Me.OrificePressureDrop)
                Case 6
                    'PROP_OP_6	Delta T	0
                    value = Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT.GetValueOrDefault)
            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 4 To 6
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 6
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 3
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 6
                        proplist.Add("PROP_OP_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
            'PROP_OP_0	Orifice Type	1
            'PROP_OP_1	Orifice Diameter	1
            'PROP_OP_2	Beta (d/D)	1
            'PROP_OP_3	Correction Factor	1
            'PROP_OP_4	Overall Pressure Drop	0
            'PROP_OP_5	Orifice Pressure Drop	0
            'PROP_OP_6	Delta T	0

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    'PROP_OP_0	Orifice Type	1
                    Me.OrifType = propval
                Case 1
                    'PROP_OP_1	Orifice Diameter	1
                    Me.OrificeDiameter = Conversor.ConverterParaSI(su.diameter, propval)
                Case 2
                    'PROP_OP_2	Beta (d/D)	1
                    Me.Beta = propval
                Case 3
                    'PROP_OP_3	Correction Factor	1
                    Me.CorrectionFactor = propval
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim value As String = ""
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_OP_0	Orifice Type	1
                    value = ""
                Case 1
                    'PROP_OP_1	Orifice Diameter	1
                    value = su.diameter
                Case 2
                    'PROP_OP_2	Beta (d/D)	1
                    value = ""
                Case 3
                    'PROP_OP_3	Correction Factor	1
                    value = ""
                Case 4
                    'PROP_OP_4	Overall Pressure Drop	0
                    value = su.spmp_deltaP
                Case 5
                    'PROP_OP_5	Orifice Pressure Drop	0
                    value = su.spmp_deltaP
                Case 6
                    'PROP_OP_6	Delta T	0
                    value = su.spmp_deltaT
            End Select

            Return value
        End Function
    End Class

End Namespace


