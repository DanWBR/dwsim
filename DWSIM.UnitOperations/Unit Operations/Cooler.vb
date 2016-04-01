'    Cooler Calculation Routines 
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
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace UnitOperations

    <System.Serializable()> Public Class Cooler

        Inherits SharedClasses.UnitOperations.BaseClass

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)
        Protected m_Tout As Nullable(Of Double) = 298.15#
        Protected m_VFout As Nullable(Of Double)
        Protected m_cmode As CalculationMode = CalculationMode.HeatRemoved

        Protected m_eta As Nullable(Of Double) = 100

        Public Enum CalculationMode
            HeatRemoved = 0
            OutletTemperature = 1
            OutletVaporFraction = 2
        End Enum

        Public Property OutletTemperature() As Nullable(Of Double)
            Get
                Return m_Tout
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_Tout = value
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

        Public Property CalcMode() As CalculationMode
            Get
                Return m_cmode
            End Get
            Set(ByVal value As CalculationMode)
                m_cmode = value
            End Set
        End Property

        Public Sub New(ByVal name As String, ByVal description As String)
            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description


        End Sub

        Public Property Eficiencia() As Nullable(Of Double)
            Get
                Return m_eta
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_eta = value
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

            Dim form As Global.DWSIM.IFLowsheet = Me.FlowSheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

            If Not Me.GraphicObject.EnergyConnector.IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Cooler
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow2"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Cooler
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculated = False
                    .Name = Me.Name
                    .ObjectType = ObjectType.Cooler
                End With

                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Validate()

            Dim Ti, Pi, Hi, Wi, ei, ein, T2, P2, H2, V2 As Double

            Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            Ti = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.temperature.GetValueOrDefault
            Pi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.pressure.GetValueOrDefault
            Hi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.enthalpy.GetValueOrDefault
            Wi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault
            ei = Hi * Wi
            ein = ei

            Dim es As DWSIM.SimulationObjects.Streams.EnergyStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)

            P2 = Pi - Me.DeltaP.GetValueOrDefault

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
            If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, W = {3} kg/s", Ti, Pi, Hi, Wi))

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            Select Case Me.CalcMode

                Case CalculationMode.HeatRemoved

                    H2 = -Me.DeltaQ.GetValueOrDefault * (Me.Eficiencia.GetValueOrDefault / 100) / Wi + Hi
                    CheckSpec(H2, False, "outlet enthalpy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, 0)
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")
                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
                    With es
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                Case CalculationMode.OutletTemperature

                    T2 = Me.OutletTemperature.GetValueOrDefault

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate outlet enthalpy... P = {0} Pa, T = {1} K", P2, T2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P, T2, P2, 0)
                    H2 = tmp(4)
                    CheckSpec(H2, False, "outlet enthalpy")
                    Me.DeltaT = T2 - Ti
                    Me.DeltaQ = -(H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
                    With es
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                Case CalculationMode.OutletVaporFraction

                    V2 = m_VFout.GetValueOrDefault

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PVF flash to calculate outlet temperature... P = {0} Pa, VF = {1}", P2, V2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.VAP, P2, m_VFout.GetValueOrDefault, Ti)

                    H2 = tmp(4)
                    CheckSpec(H2, False, "outlet enthalpy")
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")
                    Me.DeltaT = T2 - Ti
                    Me.DeltaQ = -(H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
                    With es
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

            End Select

            If Not DebugMode Then

                'Atribuir valores à corrente de matéria conectada à jusante
                With DirectCast(Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name), MaterialStream)
                    .Phases(0).Properties.temperature = T2
                    .Phases(0).Properties.pressure = P2
                    .Phases(0).Properties.enthalpy = H2
                    .Phases(2).Properties.molarfraction = V2
                    Dim comp As Interfaces.ICompound
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MoleFraction
                        comp.MassFraction = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MassFraction
                    Next
                    .Phases(0).Properties.massflow = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault
                End With

                'Call function to calculate flowsheet
                With objargs
                    .Calculated = True
                    .Name = Me.Name
                    .ObjectType = ObjectType.Cooler
                End With

                form.CalculationQueue.Enqueue(objargs)

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.IFLowsheet = Me.FlowSheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Zerar valores da corrente de matéria conectada a jusante
                With Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(0).Properties.massfraction = 1
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
                .Tag = Me.GraphicObject.Tag
                .ObjectType = ObjectType.Cooler
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
                    'PROP_HT_0	Pressure Drop
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                Case 1
                    'PROP_HT_1(Efficiency)
                    value = Me.Eficiencia.GetValueOrDefault
                Case 2
                    'PROP_HT_2	Outlet Temperature
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature.GetValueOrDefault)
                Case 3
                    'PROP_CL_3	Heat Removed
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault)
                Case 4
                    'PROP_HT_4 Outlet Vapour fraction
                    value = Me.OutletVaporFraction.GetValueOrDefault
                Case 5
                    'PROP_HT_5 DeltaT
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)

            End Select

            Return value

        End Function


        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 4 To 5
                        proplist.Add("PROP_CL_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 5
                        proplist.Add("PROP_CL_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 4
                        proplist.Add("PROP_CL_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 5
                        proplist.Add("PROP_CL_" + CStr(i))
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
                Case 1
                    'PROP_HT_1(Efficiency)
                    Me.Eficiencia = propval
                Case 2
                    'PROP_HT_2	Outlet Temperature
                    Me.OutletTemperature = SystemsOfUnits.Converter.ConvertToSI(su.temperature, propval)
                Case 3
                    'PROP_HT_3	Heat Added
                    Me.DeltaQ = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
                Case 4
                    'PROP_HT_4	Outlet Vapour fraction
                    Me.OutletVaporFraction = propval

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
                    'PROP_CL_0	Pressure Drop
                    value = su.deltaP
                Case 1
                    'PROP_CL_1  Efficiency
                    value = ""
                Case 2
                    'PROP_CL_2	Outlet Temperature
                    value = su.temperature
                Case 3
                    'PROP_CL_3	Heat Added
                    value = su.heatflow
                Case 4
                    'PROP_CL_4	Outlet Vapour Fraction
                    value = ""
                Case 5
                    'PROP_CL_4  DeltaT
                    value = su.deltaT

            End Select

            Return value
        End Function
    End Class

End Namespace
