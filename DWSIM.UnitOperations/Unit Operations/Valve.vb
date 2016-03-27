'    Valve Calculation Routines 
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
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace UnitOperations

    <System.Serializable()> Public Class Valve

        Inherits SharedClasses.UnitOperations.BaseClass

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)
        Protected m_Pout As Nullable(Of Double) = 101325.0#
        Protected m_cmode As CalculationMode = CalculationMode.DeltaP

        Public Enum CalculationMode
            DeltaP = 0
            OutletPressure = 1
        End Enum

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description



        End Sub

        Public Property OutletPressure() As Nullable(Of Double)
            Get
                Return m_Pout
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_Pout = value
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

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Valve
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Valve
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim Ti, Pi, Hi, Wi, ei, ein, T2, P2, H2, H2c As Double

            Me.PropertyPackage.CurrentMaterialStream = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            Me.PropertyPackage.CurrentMaterialStream.Validate()
            Ti = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.temperature.GetValueOrDefault.ToString
            Pi = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.pressure.GetValueOrDefault.ToString
            Hi = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.enthalpy.GetValueOrDefault.ToString
            Wi = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault.ToString
            ei = Hi * Wi
            ein = ei

            Me.PropertyPackage.CurrentMaterialStream = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            H2 = Hi '- Me.DeltaP.GetValueOrDefault / (rho_li * 1000)

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
            If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, W = {3} kg/s", Ti, Pi, Hi, Wi))

            If Me.CalcMode = CalculationMode.DeltaP Then
                P2 = Pi - Me.DeltaP.GetValueOrDefault
            Else
                P2 = Me.OutletPressure.GetValueOrDefault
                Me.DeltaP = P2 - Pi
            End If
            CheckSpec(P2, True, "outlet pressure")

            If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

            Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)
            T2 = tmp(2)
            CheckSpec(T2, True, "outlet temperature")
            H2c = tmp(4)
            CheckSpec(H2c, False, "outlet enthalpy")

            If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

            'Dim htol As Double = Me.PropertyPackage.Parameters("PP_PHFELT")
            'Dim herr As Double = Math.Abs((H2c - H2) / H2)

            'If herr > 0.01 Then Throw New Exception("The enthalpy of inlet and outlet streams doesn't match. Result is invalid.")

            Me.DeltaT = T2 - Ti
            Me.DeltaQ = 0

            If Not DebugMode Then

                'Atribuir valores à corrente de matéria conectada à jusante
                Dim omstr As MaterialStream = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                With omstr
                    .Phases(0).Properties.temperature = T2
                    .Phases(0).Properties.pressure = P2
                    .Phases(0).Properties.enthalpy = H2
                    Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MoleFraction
                        comp.MassFraction = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MassFraction
                        i += 1
                    Next
                    .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
                    .Phases(0).Properties.massflow = form.Collections.FlowsheetObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault
                End With

                'Call function to calculate flowsheet
                With objargs
                    .Calculated = True
                    .Name = Me.Name
                    .Tag = Me.GraphicObject.Tag
                    .ObjectType = ObjectType.Valve
                End With

                form.CalculationQueue.Enqueue(objargs)

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If

        End Function


        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de matéria conectada a jusante
                With form.Collections.FlowsheetObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.molarflow = Nothing
                    .GraphicObject.Calculated = False
                End With

            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            With objargs
                .Calculated = False
                .Name = Me.Name
                .ObjectType = ObjectType.Valve
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

            Dim Conversor As New SystemsOfUnits.Converter

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

                .Item.Add(Me.FlowSheet.GetTranslatedString("Correntedeentrada"), ent, False, Me.FlowSheet.GetTranslatedString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                .Item.Add(Me.FlowSheet.GetTranslatedString("Correntedesada"), saida, False, Me.FlowSheet.GetTranslatedString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                '.Item.Add(Me.FlowSheet.GetTranslatedString("CorrentedeEnergia"), energ, False, Me.FlowSheet.GetTranslatedString("Conexes1"), "", True)
                'With .Item(.Item.Count - 1)
                '    .DefaultValue = Nothing
                '    .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
                'End With

                .Item.Add(Me.FlowSheet.GetTranslatedString("ValveCalcMode"), Me, "CalcMode", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "CalcMode"

                Dim valor As Double

                Select Case Me.CalcMode

                    Case CalculationMode.DeltaP

                        valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(Me.FlowSheet.GetTranslatedString("Quedadepresso"), su.deltaP), valor, False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString(""), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_VA_1"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                    Case CalculationMode.OutletPressure

                        valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(Me.FlowSheet.GetTranslatedString("ValveOutletPressure"), su.pressure), valor, False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), "", True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_VA_2"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.pressure, "P"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With

                End Select

                .Item.Add(FT(Me.FlowSheet.GetTranslatedString("DeltaT2"), su.deltaT), Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                If Me.GraphicObject.Calculated = False Then
                    .Item.Add(Me.FlowSheet.GetTranslatedString("Mensagemdeerro"), Me, "ErrorMessage", True, Me.FlowSheet.GetTranslatedString("Miscelnea4"), Me.FlowSheet.GetTranslatedString("Mensagemretornadaqua"), True)
                    With .Item(.Item.Count - 1)
                        .DefaultType = GetType(System.String)
                    End With
                End If

            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_VA_0	Calculation Mode
                    value = Me.CalcMode
                Case 1
                    'PROP_VA_1	Pressure Drop
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                Case 2
                    'PROP_VA_2	Outlet Pressure
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure.GetValueOrDefault)
                Case 3
                    'PROP_VA_3	Temperature Drop
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 3 To 3
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 3
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 2
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 3
                        proplist.Add("PROP_VA_" + CStr(i))
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
                    'PROP_VA_0	Calculation Mode
                    Me.CalcMode = propval
                Case 1
                    'PROP_VA_1	Pressure Drop
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 2
                    'PROP_VA_2	Outlet Pressure
                    Me.OutletPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_VA_0	Calculation Mode
                    value = ""
                Case 1
                    'PROP_VA_1	Pressure Drop
                    value = su.deltaP
                Case 2
                    'PROP_VA_2	Outlet Pressure
                    value = su.pressure
                Case 3
                    'PROP_VA_3	Temperature Drop
                    value = su.deltaT

            End Select

            Return value
        End Function
    End Class

End Namespace

