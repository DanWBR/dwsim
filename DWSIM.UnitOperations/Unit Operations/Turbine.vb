'    Expander Calculation Routines 
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
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace UnitOperations

    <System.Serializable()> Public Class Expander

        Inherits SharedClasses.UnitOperations.BaseClass

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)
        Protected m_POut As Nullable(Of Double) = 101325.0#

        Protected m_eta_a As Nullable(Of Double) = 75.0#
        Protected m_eta_p As Nullable(Of Double) = Nothing

        Protected m_ignorephase As Boolean = True

        Public Enum CalculationMode
            OutletPressure = 0
            Delta_P = 1
        End Enum

        Protected m_cmode As CalculationMode = CalculationMode.Delta_P

        Public Property CalcMode() As CalculationMode
            Get
                Return m_cmode
            End Get
            Set(ByVal value As CalculationMode)
                m_cmode = value
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

        Public Property IgnorePhase() As Boolean
            Get
                Return m_ignorephase
            End Get
            Set(ByVal value As Boolean)
                m_ignorephase = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description


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

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.IFLowsheet = Me.Flowsheet

            Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Validate()

            Dim Ti, Pi, Hi, Si, Wi, rho_vi, qvi, qli, ei, ein, T2, P2, H2 As Double

            qli = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(1).Properties.volumetric_flow.GetValueOrDefault.ToString

            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            If Not Me.GraphicObject.EnergyConnector.IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Expander
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow5"))
            ElseIf qli > 0 And Not Me.IgnorePhase Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Expander
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("ExisteumaPhaselquidan2"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Expander
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Expander
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            Ti = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.temperature.GetValueOrDefault.ToString
            Pi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.pressure.GetValueOrDefault.ToString
            rho_vi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(2).Properties.density.GetValueOrDefault.ToString
            qvi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(2).Properties.volumetric_flow.GetValueOrDefault.ToString
            Hi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.enthalpy.GetValueOrDefault.ToString
            Si = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.entropy.GetValueOrDefault.ToString
            Wi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault.ToString
            ei = Hi * Wi
            ein = ei

            Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
            If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, S = {3} kJ/[kg.K], W = {4} kg/s", Ti, Pi, Hi, Si, Wi))

            Select Case Me.CalcMode
                Case CalculationMode.Delta_P
                    P2 = Pi - Me.DeltaP.GetValueOrDefault
                    POut = P2
                Case CalculationMode.OutletPressure
                    P2 = Me.POut.GetValueOrDefault
                    DeltaP = Pi - P2
            End Select
            CheckSpec(P2, True, "outlet pressure")

            If DebugMode Then AppendDebugLine(String.Format("Doing a PS flash to calculate ideal outlet enthalpy... P = {0} Pa, S = {1} kJ/[kg.K]", P2, Si))

            Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.S, P2, Si, Ti)
            T2 = tmp(2)
            CheckSpec(T2, True, "outlet temperature")
            H2 = tmp(4)
            CheckSpec(H2, False, "outlet enthalpy")

            If DebugMode Then AppendDebugLine(String.Format("Calculated ideal outlet enthalpy Hid = {0} kJ/kg", tmp(4)))

            Me.DeltaQ = -Wi * (H2 - Hi) * (Me.EficienciaAdiabatica.GetValueOrDefault / 100)

            If DebugMode Then AppendDebugLine(String.Format("Calculated real generated power = {0} kW", DeltaQ))

            If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, Hi + Me.DeltaQ.GetValueOrDefault / Wi))

            tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, Hi - Me.DeltaQ.GetValueOrDefault / Wi, T2)
            T2 = tmp(2)
            CheckSpec(T2, True, "outlet temperature")
            Me.DeltaT = T2 - Ti

            If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

            H2 = Hi - Me.DeltaQ.GetValueOrDefault / Wi

            If Not DebugMode Then

                'Atribuir valores à corrente de matéria conectada à jusante
                With Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Phases(0).Properties.temperature = T2
                    .Phases(0).Properties.pressure = P2
                    .Phases(0).Properties.enthalpy = H2
                    Dim comp As Interfaces.ICompound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MoleFraction
                        comp.MassFraction = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MassFraction
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault
                End With

                'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
                With Me.FlowSheet.SimulationObjects(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                    .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                    .GraphicObject.Calculated = True
                End With

                'Call function to calculate flowsheet
                With objargs
                    .Calculated = True
                    .Name = Me.Name
                    .Tag = Me.GraphicObject.Tag
                    .ObjectType = ObjectType.Expander
                End With

                form.CalculationQueue.Enqueue(objargs)

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If


        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.IFLowsheet = Me.Flowsheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de matéria conectada a jusante
                With Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As Interfaces.ICompound
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

            'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With Me.FlowSheet.SimulationObjects(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                    .EnergyFlow = Nothing
                    .GraphicObject.Calculated = False
                End With
            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            With objargs
                .Calculated = False
                .Name = Me.Name
                .ObjectType = ObjectType.Expander
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_CO_0	Pressure Increase (Head)
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                Case 1
                    'PROP_CO_1(Efficiency)
                    value = Me.EficienciaAdiabatica.GetValueOrDefault
                Case 2
                    'PROP_CO_2(Delta - T)
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
                Case 3
                    'PROP_CO_3	Power Required
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault)
                Case 4
                    'PROP_CO_4	Pressure Out
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.POut.GetValueOrDefault)
            End Select

            Return value

        End Function



        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 2 To 4
                        proplist.Add("PROP_TU_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 4
                        proplist.Add("PROP_TU_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 1
                        proplist.Add("PROP_TU_" + CStr(i))
                    Next
                    proplist.Add("PROP_TU_4")
                Case PropertyType.ALL
                    For i = 0 To 4
                        proplist.Add("PROP_TU_" + CStr(i))
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
                    'PROP_CO_0	Pressure Increase (Head)
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 1
                    'PROP_CO_1(Efficiency)
                    Me.EficienciaAdiabatica = propval
                Case 4
                    'PROP_CO_4(Pressure Out)
                    Me.POut = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
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
                    'PROP_CO_0	Pressure Increase (Head)
                    value = su.deltaP
                Case 1
                    'PROP_CO_1(Efficiency)
                    value = ""
                Case 2
                    'PROP_CO_2(Delta - T)
                    value = su.deltaT
                Case 3
                    'PROP_CO_3	Power Required
                    value = su.heatflow
                Case 4
                    'PROP_CO_4	Pressure Out
                    value = su.pressure
            End Select

            Return value
        End Function
    End Class

End Namespace
