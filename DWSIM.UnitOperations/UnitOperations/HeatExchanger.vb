'    Heat Exchanger Calculation Routines 
'    Copyright 2008-2024 Daniel Wagner O. de Medeiros
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


Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums
Imports System.Globalization
Imports System.Reflection
Imports System.Threading.Tasks
Imports System.Math
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.HeatExchanger

Namespace UnitOperations

    Public Enum HeatExchangerCalcMode

        CalcTempHotOut = 0
        CalcTempColdOut = 1
        CalcBothTemp = 2
        CalcBothTemp_UA = 3
        CalcArea = 4
        ShellandTube_Rating = 5
        ShellandTube_CalcFoulingFactor = 6
        PinchPoint = 7
        ThermalEfficiency = 8
        OutletVaporFraction1 = 9
        OutletVaporFraction2 = 10

    End Enum

    Public Enum SpecifiedTemperature
        Hot_Fluid
        Cold_Fluid
    End Enum

    Public Enum FlowDirection
        CounterCurrent
        CoCurrent
    End Enum

    Public Enum HeatExchangerType

        DoublePipe
        ShellTubes_E
        ShellTubes_F
        ShellTubes_G
        ShellTubes_H
        ShellTubes_J
        ShellTubes_K
        ShellTubes_X

    End Enum

    <System.Serializable()> Public Class HeatExchanger

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Exchangers

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = True


        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_HeatExchanger

        Protected m_Q As Nullable(Of Double) = 0
        Protected m_dp As Nullable(Of Double) = 0
        Protected m_OverallCoefficient As Nullable(Of Double) = 1000
        Protected m_Area As Nullable(Of Double) = 1.0#
        Protected TempHotOut As Nullable(Of Double) = 298.15#
        Protected TempColdOut As Nullable(Of Double) = 298.15#
        Protected m_tempdiff As Double = 0
        Protected FoulingFactor As Nullable(Of Double) = 0
        Protected Type As Integer
        Protected CalcMode As HeatExchangerCalcMode = HeatExchangerCalcMode.CalcBothTemp_UA
        Protected m_HotSidePressureDrop As Double = 0
        Protected m_ColdSidePressureDrop As Double = 0
        Protected m_specifiedtemperature As SpecifiedTemperature = SpecifiedTemperature.Cold_Fluid
        Protected m_flowdirection As FlowDirection = FlowDirection.CounterCurrent
        Protected m_stprops As New STHXProperties
        Protected m_f As Double = 1.0#

        Public Property HeatProfile As Double() = {}
        Public Property TemperatureProfileCold As Double() = {}
        Public Property TemperatureProfileHot As Double() = {}
        Public Property IgnoreLMTDError As Boolean = True
        Public Property CorrectionFactorLMTD As Double = 1.0
        Public Property HeatLoss As Double = 0.0
        Public Property OutletVaporFraction1 As Double = 0.0
        Public Property OutletVaporFraction2 As Double = 0.0

        Public Property PinchPointAtOutlets As Boolean = False

        Public Property UseShellAndTubeGeometryInformation As Boolean = False

        Public Property CalculateHeatExchangeProfile As Boolean = False

        Public Property STProperties() As STHXProperties
            Get
                If m_stprops Is Nothing Then m_stprops = New STHXProperties
                Return m_stprops
            End Get
            Set(value As STHXProperties)
                m_stprops = value
            End Set
        End Property

        Public Property LMTD_F() As Double
            Get
                Return m_f
            End Get
            Set(ByVal value As Double)
                m_f = value
            End Set
        End Property

        Public Property FlowDir() As FlowDirection
            Get
                Return m_flowdirection
            End Get
            Set(ByVal value As FlowDirection)
                m_flowdirection = value
            End Set
        End Property

        Public Property DefinedTemperature() As SpecifiedTemperature
            Get
                Return m_specifiedtemperature
            End Get
            Set(ByVal value As SpecifiedTemperature)
                m_specifiedtemperature = value
            End Set
        End Property

        Public Property ThermalEfficiency As Double = 0.0#

        Public Property MaxHeatExchange As Double = 0.0#

        Public Property MITA As Double = 0.0#

        'proxy properties

        Public Overrides Function GetCalculationModes() As String()

            Dim modes As New List(Of String)

            For Each tstEnum As HeatExchangerCalcMode In System.Enum.GetValues(GetType(HeatExchangerCalcMode))
                modes.Add(String.Format("Name: {0}  Value: {1}", tstEnum.ToString, CInt(tstEnum).ToString()))
            Next

            Return modes.ToArray()

        End Function

        Public Overrides Function SetCalculationMode(modeID As Integer) As Object

            Me.CalcMode = modeID

            Return CalcMode.ToString()

        End Function

        Public Property Efficiency As Double
            Get
                Return ThermalEfficiency
            End Get
            Set(value As Double)
                ThermalEfficiency = value
            End Set
        End Property


        Public Property HeatDuty As Double
            Get
                Return Q.GetValueOrDefault()
            End Get
            Set(value As Double)
                Q = value
            End Set
        End Property

        Public Property HotSideTemperatureChange As Double

        Public Property ColdSideTemperatureChange As Double

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean
            'workaround for renaming CalcBothTemp_KA calculation type to CalcBothTemp_UA
            For Each xel In data
                If xel.Name = "CalculationMode" Then
                    xel.Value = xel.Value.Replace("CalcBothTemp_KA", "CalcBothTemp_UA")
                End If
            Next
            Dim ael = (From xel As XElement In data Select xel Where xel.Name = "AccumulationStreamHot").FirstOrDefault
            If Not ael Is Nothing Then
                AccumulationStreamHot = New Thermodynamics.Streams.MaterialStream()
                AccumulationStreamHot.LoadData(ael.Elements.ToList)
            End If
            Dim ael2 = (From xel As XElement In data Select xel Where xel.Name = "AccumulationStreamCold").FirstOrDefault
            If Not ael2 Is Nothing Then
                AccumulationStreamCold = New Thermodynamics.Streams.MaterialStream()
                AccumulationStreamCold.LoadData(ael2.Elements.ToList)
            End If
            Return MyBase.LoadData(data)
        End Function

        Public Overrides Function SaveData() As List(Of XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()

            If AccumulationStreamHot IsNot Nothing Then
                elements.Add(New XElement("AccumulationStreamHot", AccumulationStreamHot.SaveData()))
            End If
            If AccumulationStreamCold IsNot Nothing Then
                elements.Add(New XElement("AccumulationStreamCold", AccumulationStreamCold.SaveData()))
            End If

            Return elements

        End Function

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            ComponentName = name
            ComponentDescription = description
            Type = HeatExchangerType.DoublePipe

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New HeatExchanger()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of HeatExchanger)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Property CalculationMode() As HeatExchangerCalcMode
            Get
                Return Me.CalcMode
            End Get
            Set(ByVal value As HeatExchangerCalcMode)
                Me.CalcMode = value
            End Set
        End Property

        Public Property OverallCoefficient() As Nullable(Of Double)
            Get
                Return m_OverallCoefficient
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_OverallCoefficient = value
            End Set
        End Property

        Public Property Area As Nullable(Of Double)
            Get
                Return m_Area
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_Area = value
            End Set
        End Property

        Public Property DeltaP As Nullable(Of Double)
            Get
                Return m_dp
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dp = value
            End Set
        End Property

        Public Property Q As Nullable(Of Double)
            Get
                Return m_Q
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_Q = value
            End Set
        End Property

        Public Property HotSidePressureDrop As Double
            Get
                Return m_HotSidePressureDrop
            End Get
            Set(ByVal value As Double)
                m_HotSidePressureDrop = value
            End Set
        End Property

        Public Property ColdSidePressureDrop As Double
            Get
                Return m_ColdSidePressureDrop
            End Get
            Set(ByVal value As Double)
                m_ColdSidePressureDrop = value
            End Set
        End Property

        Public Property HotSideOutletTemperature As Double
            Get
                Return TempHotOut
            End Get
            Set(ByVal value As Double)
                TempHotOut = value
            End Set
        End Property

        Public Property ColdSideOutletTemperature As Double
            Get
                Return TempColdOut
            End Get
            Set(ByVal value As Double)
                TempColdOut = value
            End Set
        End Property

        Public Property LMTD() As Double
            Get
                Return m_tempdiff
            End Get
            Set(ByVal value As Double)
                m_tempdiff = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Sub Validate()

            MyBase.Validate()

        End Sub

        Public AccumulationStreamCold As Thermodynamics.Streams.MaterialStream

        Public AccumulationStreamHot As Thermodynamics.Streams.MaterialStream

        Private prevMHot, currentMHot, prevMCold, currentMCold As Double

        Public Overrides Sub CreateDynamicProperties()

            AddDynamicProperty("Cold Fluid Flow Conductance", "Flow Conductance (inverse of Resistance).", 1, UnitOfMeasure.conductance, 1.0.GetType())
            AddDynamicProperty("Hot Fluid Flow Conductance", "Flow Conductance (inverse of Resistance) for the Hot Fluid.", 1, UnitOfMeasure.conductance, 1.0.GetType())
            AddDynamicProperty("Volume for Cold Fluid", "Available Volume for Cold Fluid", 1, UnitOfMeasure.volume, 1.0.GetType())
            AddDynamicProperty("Volume for Hot Fluid", "Available Volume for Cold Fluid", 1, UnitOfMeasure.volume, 1.0.GetType())
            AddDynamicProperty("Cold Side Pressure", "Dynamic Pressure for the Cold Fluid side.", 101325, UnitOfMeasure.pressure, 1.0.GetType())
            AddDynamicProperty("Hot Side Pressure", "Dynamic Pressure for the Hot Fluid side.", 101325, UnitOfMeasure.pressure, 1.0.GetType())
            AddDynamicProperty("Minimum Pressure", "Minimum Dynamic Pressure for this Unit Operation.", 101325, UnitOfMeasure.pressure, 1.0.GetType())
            AddDynamicProperty("Initialize using Inlet Streams", "Initializes the volume contents with information from the inlet streams, if the content is null.", False, UnitOfMeasure.none, True.GetType())
            AddDynamicProperty("Reset Contents", "Empties the volume contents on the next run.", False, UnitOfMeasure.none, True.GetType())

        End Sub

        Public Overrides Sub RunDynamicModel()

            Dim integratorID = FlowSheet.DynamicsManager.ScheduleList(FlowSheet.DynamicsManager.CurrentSchedule).CurrentIntegrator
            Dim integrator = FlowSheet.DynamicsManager.IntegratorList(integratorID)

            Dim timestep = integrator.IntegrationStep.TotalSeconds

            If integrator.RealTime Then timestep = Convert.ToDouble(integrator.RealTimeStepMs) / 1000.0

            Dim KrCold As Double = GetDynamicProperty("Cold Fluid Flow Conductance")
            Dim KrHot As Double = GetDynamicProperty("Cold Fluid Flow Conductance")

            Dim VolumeCold As Double = GetDynamicProperty("Volume for Cold Fluid")
            Dim VolumeHot As Double = GetDynamicProperty("Volume for Hot Fluid")

            If CalcMode = HeatExchangerCalcMode.ShellandTube_Rating Then

                Dim Vshell, Vtubes As Double

                Vshell = Math.PI * (STProperties.Shell_Di / 1000) ^ 2 / 4 * STProperties.Tube_Length

                Vtubes = Math.PI * (STProperties.Tube_Di / 1000) ^ 2 / 4 * STProperties.Tube_Length * STProperties.Tube_NumberPerShell

                If STProperties.Tube_Fluid = 0 Then
                    'cold
                    VolumeCold = Vtubes
                    VolumeHot = Vshell - Vtubes
                Else
                    'hot
                    VolumeHot = Vtubes
                    VolumeCold = Vshell - Vtubes
                End If

            End If

            Dim InitializeFromInlet As Boolean = GetDynamicProperty("Initialize using Inlet Streams")

            Dim Pmin = GetDynamicProperty("Minimum Pressure")

            Dim Reset As Boolean = GetDynamicProperty("Reset Contents")

            If Reset Then
                AccumulationStreamCold = Nothing
                AccumulationStreamHot = Nothing
                SetDynamicProperty("Reset Contents", 0)
            End If

            Dim Ti1, Ti2, A, Tc1, Th1, Wc, Wh, P1, P2, Th2, Tc2, U As Double
            Dim Pc1, Ph1, Pc2, Ph2, DeltaHc, DeltaHh, H1, H2, Hc1, Hh1, Hc2, Hh2 As Double
            Dim StIn0, StIn1, StOut0, StOut1, StInCold, StInHot, StOutHot, StOutCold As MaterialStream

            'Validate unitop status.
            Me.Validate()

            StIn0 = Me.GetInletMaterialStream(0)
            StIn1 = Me.GetInletMaterialStream(1)

            StOut0 = Me.GetOutletMaterialStream(0)
            StOut1 = Me.GetOutletMaterialStream(1)

            'First input stream.
            Ti1 = StIn0.Phases(0).Properties.temperature.GetValueOrDefault
            P1 = StIn0.Phases(0).Properties.pressure.GetValueOrDefault
            H1 = StIn0.Phases(0).Properties.enthalpy.GetValueOrDefault

            'Second input stream.
            Ti2 = StIn1.Phases(0).Properties.temperature.GetValueOrDefault
            P2 = StIn1.Phases(0).Properties.pressure.GetValueOrDefault
            H2 = StIn1.Phases(0).Properties.enthalpy.GetValueOrDefault

            'Let us use properties at the entrance as an initial implementation.

            If Ti1 < Ti2 Then
                'Input1 is the cold stream.
                'Identify cold and hot streams.
                StInCold = StIn0
                StInHot = StIn1
                StOutCold = StOut0
                StOutHot = StOut1
            Else
                'Input2 is the cold stream.
                'Identify cold and hot streams.
                StInCold = StIn1
                StInHot = StIn0
                StOutCold = StOut1
                StOutHot = StOut0
            End If

            Dim liquidcase As Boolean = False

            ' hot accumulation stream

            If AccumulationStreamHot Is Nothing Then

                If InitializeFromInlet Then

                    AccumulationStreamHot = StInHot.CloneXML

                Else

                    AccumulationStreamHot = StInHot.Subtract(StOutHot, timestep)
                    AccumulationStreamHot = AccumulationStreamHot.Subtract(StOutHot, timestep)

                End If

                Dim density = AccumulationStreamHot.Phases(0).Properties.density.GetValueOrDefault

                AccumulationStreamHot.SetMassFlow(density * VolumeHot)
                AccumulationStreamHot.SpecType = StreamSpec.Pressure_and_Enthalpy
                AccumulationStreamHot.PropertyPackage = PropertyPackage
                AccumulationStreamHot.PropertyPackage.CurrentMaterialStream = AccumulationStreamHot
                AccumulationStreamHot.Calculate()

            Else

                AccumulationStreamHot.PropertyPackage = StInHot.PropertyPackage
                AccumulationStreamHot.SetFlowsheet(FlowSheet)
                If StInHot.GetMassFlow() > 0 Then AccumulationStreamHot = AccumulationStreamHot.Add(StInHot, timestep)
                AccumulationStreamHot.PropertyPackage.CurrentMaterialStream = AccumulationStreamHot
                AccumulationStreamHot.SpecType = StreamSpec.Temperature_and_Pressure
                AccumulationStreamHot.Calculate()
                If StOutHot.GetMassFlow() > 0 Then AccumulationStreamHot = AccumulationStreamHot.Subtract(StOutHot, timestep)
                If AccumulationStreamHot.GetMassFlow <= 0.0 Then AccumulationStreamHot.SetMassFlow(0.0)

            End If

            AccumulationStreamHot.SetFlowsheet(FlowSheet)

            ' cold accumulation stream

            If AccumulationStreamCold Is Nothing Then

                If InitializeFromInlet Then

                    AccumulationStreamCold = StInCold.CloneXML

                Else

                    AccumulationStreamCold = StInCold.Subtract(StOutCold, timestep)
                    AccumulationStreamCold = AccumulationStreamCold.Subtract(StOutCold, timestep)

                End If

                Dim density = AccumulationStreamCold.Phases(0).Properties.density.GetValueOrDefault

                AccumulationStreamCold.SetMassFlow(density * VolumeCold)
                AccumulationStreamCold.SpecType = StreamSpec.Pressure_and_Enthalpy
                AccumulationStreamCold.PropertyPackage = PropertyPackage
                AccumulationStreamCold.PropertyPackage.CurrentMaterialStream = AccumulationStreamCold
                AccumulationStreamCold.Calculate()

            Else

                AccumulationStreamCold.PropertyPackage = StInCold.PropertyPackage
                AccumulationStreamCold.SetFlowsheet(FlowSheet)
                If StInCold.GetMassFlow() > 0 Then AccumulationStreamCold = AccumulationStreamCold.Add(StInCold, timestep)
                AccumulationStreamCold.PropertyPackage.CurrentMaterialStream = AccumulationStreamCold
                AccumulationStreamCold.SpecType = StreamSpec.Temperature_and_Pressure
                AccumulationStreamCold.Calculate()
                If StOutCold.GetMassFlow() > 0 Then AccumulationStreamCold = AccumulationStreamCold.Subtract(StOutCold, timestep)
                If AccumulationStreamCold.GetMassFlow <= 0.0 Then AccumulationStreamCold.SetMassFlow(0.0)

            End If

            AccumulationStreamCold.SetFlowsheet(FlowSheet)

            'calculate pressure (hot)

            Dim MHot = AccumulationStreamHot.GetMolarFlow()

            Dim TemperatureHot = AccumulationStreamHot.GetTemperature

            Dim PressureHot = StInHot.GetPressure

            'm3/mol

            If MHot > 0 Then

                prevMHot = currentMHot

                currentMHot = VolumeHot / MHot

                If Math.Abs(currentMHot - prevMHot) < 0.0001 And AccumulationStreamHot.Phases(1).Properties.molarfraction.GetValueOrDefault > 0.99999 Then
                    liquidcase = True
                End If

                PropertyPackage.CurrentMaterialStream = AccumulationStreamHot

                If AccumulationStreamHot.GetPressure > 0 Then

                    If (prevMHot = 0.0 Or integrator.ShouldCalculateEquilibrium) And Not liquidcase Then

                        Dim result As IFlashCalculationResult

                        result = PropertyPackage.CalculateEquilibrium2(FlashCalculationType.VolumeTemperature, currentMHot, TemperatureHot, PressureHot)

                        PressureHot = result.CalculatedPressure

                    Else

                        If prevMHot > 0.0 Then PressureHot = currentMHot / prevMHot * PressureHot

                    End If

                Else

                    PressureHot = Pmin

                End If

            Else

                PressureHot = Pmin

            End If

            AccumulationStreamHot.SetPressure(PressureHot)

            Ph1 = PressureHot

            'calculate pressure (cold)

            Dim MCold = AccumulationStreamCold.GetMolarFlow()

            Dim TemperatureCold = AccumulationStreamCold.GetTemperature

            Dim PressureCold = StInCold.GetPressure

            'm3/mol

            If MCold > 0 Then

                prevMCold = currentMCold

                currentMCold = VolumeCold / MCold

                If Math.Abs(currentMCold - prevMCold) < 0.0001 And AccumulationStreamCold.Phases(1).Properties.molarfraction.GetValueOrDefault > 0.99999 Then
                    liquidcase = True
                End If

                AccumulationStreamCold.PropertyPackage.CurrentMaterialStream = AccumulationStreamCold

                If AccumulationStreamCold.GetPressure > 0 Then

                    If (prevMCold = 0.0 Or integrator.ShouldCalculateEquilibrium) And Not liquidcase Then

                        Dim result As IFlashCalculationResult

                        result = PropertyPackage.CalculateEquilibrium2(FlashCalculationType.VolumeTemperature, currentMCold, TemperatureCold, PressureCold)

                        PressureCold = result.CalculatedPressure

                    Else

                        If prevMCold > 0.0 Then PressureCold = currentMCold / prevMCold * PressureCold

                    End If

                Else

                    PressureCold = Pmin

                End If

            Else

                PressureCold = Pmin

            End If

            AccumulationStreamCold.SetPressure(PressureCold)

            Pc1 = PressureCold

            'enthalpies

            Hc1 = AccumulationStreamCold.GetMassEnthalpy()

            Hh1 = AccumulationStreamHot.GetMassEnthalpy()

            'temperatures

            Tc1 = TemperatureCold

            Th1 = TemperatureHot

            ' flow rates (mass)

            Wc = AccumulationStreamCold.GetMassFlow()

            Wh = AccumulationStreamHot.GetMassFlow()

            'calculate maximum theoretical heat exchange 

            Dim HHx As Double
            Dim tmpstr As MaterialStream = AccumulationStreamHot.Clone
            tmpstr.PropertyPackage = AccumulationStreamHot.PropertyPackage
            tmpstr.SetFlowsheet(AccumulationStreamHot.FlowSheet)
            tmpstr.PropertyPackage.CurrentMaterialStream = tmpstr
            tmpstr.SetTemperature(Tc1)
            tmpstr.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
            tmpstr.Calculate(False, True)
            HHx = tmpstr.Phases(0).Properties.enthalpy.GetValueOrDefault
            DeltaHh = StInHot.GetMassFlow() * (Hh1 - HHx) 'kW

            tmpstr = AccumulationStreamCold.Clone
            tmpstr.PropertyPackage = AccumulationStreamCold.PropertyPackage
            tmpstr.SetFlowsheet(AccumulationStreamCold.FlowSheet)
            tmpstr.PropertyPackage.CurrentMaterialStream = tmpstr
            tmpstr.SetTemperature(Th1)
            tmpstr.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
            tmpstr.Calculate(False, True)
            HHx = tmpstr.Phases(0).Properties.enthalpy.GetValueOrDefault
            DeltaHc = StInCold.GetMassFlow() * (HHx - Hc1) 'kW

            MaxHeatExchange = Min(DeltaHc, DeltaHh) 'kW

            tmpstr.PropertyPackage = Nothing
            tmpstr.Dispose()

            Th2 = AccumulationStreamHot.GetTemperature()

            Tc2 = AccumulationStreamCold.GetTemperature()

            Dim tmp As IFlashCalculationResult

            Select Case CalcMode

                Case HeatExchangerCalcMode.CalcArea, HeatExchangerCalcMode.CalcTempColdOut,
                     HeatExchangerCalcMode.CalcBothTemp, HeatExchangerCalcMode.CalcTempHotOut,
                     HeatExchangerCalcMode.PinchPoint, HeatExchangerCalcMode.ThermalEfficiency,
                     HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor,
                     HeatExchangerCalcMode.OutletVaporFraction1, HeatExchangerCalcMode.OutletVaporFraction2

                    Throw New Exception("This calculation mode is not supported while in Dynamic Mode.")

                Case HeatExchangerCalcMode.CalcBothTemp_UA

                    A = Area
                    U = OverallCoefficient

                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            If (Th1 - Tc1) / (Th2 - Tc2) = 1 Then
                                LMTD = ((Th1 - Tc1) + (Th2 - Tc2)) / 2
                            Else
                                LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                            End If
                        Case FlowDirection.CounterCurrent
                            If (Th1 - Tc2) / (Th2 - Tc1) = 1 Then
                                LMTD = ((Th1 - Tc2) + (Th2 - Tc1)) / 2
                            Else
                                LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                            End If
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then LMTD = 0.0

                    Q = U / 1000 * A * LMTD * timestep

                    If Q > MaxHeatExchange Then Q = MaxHeatExchange

                    Ph2 = Ph1 - (StInHot.GetMassFlow() / KrHot) ^ 2
                    Pc2 = Pc1 - (StInCold.GetMassFlow() / KrCold) ^ 2

                    Hc2 = (Q - HeatLoss) / Wc + Hc1
                    Hh2 = Hh1 - Q / Wh

                Case HeatExchangerCalcMode.ShellandTube_Rating

                    'Shell and Tube HX calculation using Tinker's method.

                    Dim Cpc, Cph As Double
                    Dim Tcm, Thm As Double

                    Dim rhoc, muc, kc, rhoh, muh, kh, rs, rt, Atc, Nc, di, de, pitch, L, n, hi, nt, vt, Ret, Prt As Double

                    '3

                    Tcm = AccumulationStreamCold.GetTemperature
                    Thm = AccumulationStreamHot.GetTemperature

                    '4, 5

                    rhoc = AccumulationStreamCold.Phases(0).Properties.density.GetValueOrDefault
                    Cpc = AccumulationStreamCold.Phases(0).Properties.heatCapacityCp.GetValueOrDefault
                    kc = AccumulationStreamCold.Phases(0).Properties.thermalConductivity.GetValueOrDefault
                    muc = AccumulationStreamCold.Phases(1).Properties.viscosity.GetValueOrDefault * AccumulationStreamCold.Phases(1).Properties.molarfraction.GetValueOrDefault + AccumulationStreamCold.Phases(2).Properties.viscosity.GetValueOrDefault * AccumulationStreamCold.Phases(2).Properties.molarfraction.GetValueOrDefault

                    rhoh = AccumulationStreamHot.Phases(0).Properties.density.GetValueOrDefault
                    Cph = AccumulationStreamHot.Phases(0).Properties.heatCapacityCp.GetValueOrDefault
                    kh = AccumulationStreamHot.Phases(0).Properties.thermalConductivity.GetValueOrDefault
                    muh = AccumulationStreamHot.Phases(1).Properties.viscosity.GetValueOrDefault * AccumulationStreamHot.Phases(1).Properties.molarfraction.GetValueOrDefault + AccumulationStreamHot.Phases(2).Properties.viscosity.GetValueOrDefault * AccumulationStreamHot.Phases(2).Properties.molarfraction.GetValueOrDefault

                    '6

                    rs = Me.STProperties.Shell_Fouling
                    rt = Me.STProperties.Tube_Fouling
                    Nc = STProperties.Shell_NumberOfShellsInSeries
                    de = STProperties.Tube_De / 1000
                    di = STProperties.Tube_Di / 1000
                    L = STProperties.Tube_Length
                    pitch = STProperties.Tube_Pitch / 1000
                    n = STProperties.Tube_NumberPerShell
                    nt = n / STProperties.Tube_PassesPerShell
                    A = n * Math.PI * de * (L - 2 * de)
                    Atc = A / Nc

                    If STProperties.Tube_Fluid = 0 Then
                        'cold
                        vt = Wc / (rhoc * nt * Math.PI * di ^ 2 / 4)
                        Ret = rhoc * vt * di / muc
                        Prt = muc * Cpc / kc * 1000
                    Else
                        'hot
                        vt = Wh / (rhoh * nt * Math.PI * di ^ 2 / 4)
                        Ret = rhoh * vt * di / muh
                        Prt = muh * Cph / kh * 1000
                    End If

                    'calcular DeltaP

                    Dim dpt, dps As Double

                    'tube

                    Dim fric As Double
                    Dim epsilon As Double = STProperties.Tube_Roughness / 1000
                    If Ret > 3250 Then
                        Dim a1 = Math.Log(((epsilon / di) ^ 1.1096) / 2.8257 + (7.149 / Ret) ^ 0.8961) / Math.Log(10.0#)
                        Dim b1 = -2 * Math.Log((epsilon / di) / 3.7065 - 5.0452 * a1 / Ret) / Math.Log(10.0#)
                        fric = (1 / b1) ^ 2
                    Else
                        fric = 64 / Ret
                    End If
                    fric *= STProperties.Tube_Scaling_FricCorrFactor

                    If STProperties.Tube_Fluid = 0 Then
                        'cold
                        dpt = fric * L * STProperties.Tube_PassesPerShell / di * vt ^ 2 / 2 * rhoc
                    Else
                        'hot
                        dpt = fric * L * STProperties.Tube_PassesPerShell / di * vt ^ 2 / 2 * rhoh
                    End If

                    'tube heat transfer coeff

                    If STProperties.Tube_Fluid = 0 Then
                        'cold
                        hi = kc / di * (fric / 8) * Ret * Prt / (1.07 + 12.7 * (fric / 8) ^ 0.5 * (Prt ^ (2 / 3) - 1))
                    Else
                        'hot
                        hi = kh / di * (fric / 8) * Ret * Prt / (1.07 + 12.7 * (fric / 8) ^ 0.5 * (Prt ^ (2 / 3) - 1))
                    End If

                    'shell internal diameter

                    Dim Dsi, Dsf, nsc, HDi, Nb As Double
                    Select Case STProperties.Tube_Layout
                        Case 0, 1
                            nsc = 1.1 * n ^ 0.5
                        Case 2, 3
                            nsc = 1.19 * n ^ 0.5
                    End Select
                    Dsf = (nsc - 1) * pitch + de
                    Dsi = STProperties.Shell_Di / 1000 'Dsf / 1.075

                    'Dsf = Dsi / 1.075 * Dsi

                    HDi = STProperties.Shell_BaffleCut / 100
                    Nb = L / (STProperties.Shell_BaffleSpacing / 1000) + 1 'review (l1, l2)

                    'shell pressure drop

                    Dim Gsf, Np, Fp, Ss, Ssf, fs, Cb, Ca, Res, Prs, jh, aa, bb, cc, xx, yy, Nh, Y As Double
                    xx = Dsi / (STProperties.Shell_BaffleSpacing / 1000)
                    yy = pitch / de

                    Select Case STProperties.Tube_Layout
                        Case 0, 1
                            aa = 0.9078565328950694
                            bb = 0.66331106126564476
                            cc = -4.4329764639656482
                            Nh = aa * xx ^ bb * yy ^ cc
                            aa = 5.3718559074820611
                            bb = -0.33416765138071414
                            cc = 0.7267144209289168
                            Y = aa * xx ^ bb * yy ^ cc
                            aa = 0.53807650470841084
                            bb = 0.3761125784751041
                            cc = -3.8741224386187474
                            Np = aa * xx ^ bb * yy ^ cc
                        Case 2
                            aa = 0.84134824361715088
                            bb = 0.61374520485097339
                            cc = -4.2696318466170409
                            Nh = aa * xx ^ bb * yy ^ cc
                            aa = 4.9901814007765743
                            bb = -0.32437442510328618
                            cc = 1.084850423269188
                            Y = aa * xx ^ bb * yy ^ cc
                            aa = 0.5502379008813062
                            bb = 0.36559560225434834
                            cc = -3.99041305625483
                            Np = aa * xx ^ bb * yy ^ cc
                        Case 3
                            aa = 0.66738654406767639
                            bb = 0.680260033886211
                            cc = -4.522291113086232
                            Nh = aa * xx ^ bb * yy ^ cc
                            aa = 4.5749169651729105
                            bb = -0.32201759442337358
                            cc = 1.17295183743691
                            Y = aa * xx ^ bb * yy ^ cc
                            aa = 0.36869631130961067
                            bb = 0.38397859475813922
                            cc = -3.6273465996780421
                            Np = aa * xx ^ bb * yy ^ cc
                    End Select
                    Fp = 1 / (0.8 + Np * (Dsi / pitch) ^ 0.5)
                    Select Case STProperties.Tube_Layout
                        Case 0, 1, 2
                            Cb = 0.97
                        Case 3
                            Cb = 1.37
                    End Select
                    Ca = Cb * (pitch - de) / pitch
                    Ss = Ca * STProperties.Shell_BaffleSpacing / 1000 * Dsf
                    Ssf = Ss / Fp
                    Ssf = Math.PI / 4 * (Dsi ^ 2 - nt * de ^ 2)
                    If STProperties.Shell_Fluid = 0 Then
                        'cold
                        Gsf = Wc / Ssf
                        Res = Gsf * de / muc
                        Prs = muc * Cpc / kc * 1000
                    Else
                        'hot
                        Gsf = Wh / Ssf
                        Res = Gsf * de / muh
                        Prs = muh * Cph / kh * 1000
                    End If

                    Select Case STProperties.Tube_Layout
                        Case 0, 1
                            If Res < 100 Then
                                jh = 0.497 * Res ^ 0.54
                            Else
                                jh = 0.378 * Res ^ 0.59
                            End If
                            If pitch / de <= 1.2 Then
                                If Res < 100 Then
                                    fs = 276.46 * Res ^ -0.979
                                ElseIf Res < 1000 Then
                                    fs = 30.26 * Res ^ -0.523
                                Else
                                    fs = 2.93 * Res ^ -0.186
                                End If
                            ElseIf pitch / de <= 1.3 Then
                                If Res < 100 Then
                                    fs = 208.14 * Res ^ -0.945
                                ElseIf Res < 1000 Then
                                    fs = 27.6 * Res ^ -0.525
                                Else
                                    fs = 2.27 * Res ^ -0.163
                                End If
                            ElseIf pitch / de <= 1.4 Then
                                If Res < 100 Then
                                    fs = 122.73 * Res ^ -0.865
                                ElseIf Res < 1000 Then
                                    fs = 17.82 * Res ^ -0.474
                                Else
                                    fs = 1.86 * Res ^ -0.146
                                End If
                            ElseIf pitch / de <= 1.5 Then
                                If Res < 100 Then
                                    fs = 104.33 * Res ^ -0.869
                                ElseIf Res < 1000 Then
                                    fs = 12.69 * Res ^ -0.434
                                Else
                                    fs = 1.526 * Res ^ -0.129
                                End If
                            End If
                        Case 2, 3
                            If Res < 100 Then
                                If STProperties.Tube_Layout = 2 Then
                                    jh = 0.385 * Res ^ 0.526
                                Else
                                    jh = 0.496 * Res ^ 0.54
                                End If
                            Else
                                If STProperties.Tube_Layout = 2 Then
                                    jh = 0.2487 * Res ^ 0.625
                                Else
                                    jh = 0.354 * Res ^ 0.61
                                End If
                            End If
                            If pitch / de <= 1.2 Then
                                If Res < 100 Then
                                    fs = 230 * Res ^ -1
                                ElseIf Res < 1000 Then
                                    fs = 16.23 * Res ^ -0.43
                                Else
                                    fs = 2.67 * Res ^ -0.173
                                End If
                            ElseIf pitch / de <= 1.3 Then
                                If Res < 100 Then
                                    fs = 142.22 * Res ^ -0.949
                                ElseIf Res < 1000 Then
                                    fs = 11.93 * Res ^ -0.43
                                Else
                                    fs = 1.77 * Res ^ -0.144
                                End If
                            ElseIf pitch / de <= 1.4 Then
                                If Res < 100 Then
                                    fs = 110.77 * Res ^ -0.965
                                ElseIf Res < 1000 Then
                                    fs = 7.524 * Res ^ -0.4
                                Else
                                    fs = 1.01 * Res ^ -0.104
                                End If
                            ElseIf pitch / de <= 1.5 Then
                                If Res < 100 Then
                                    fs = 58.18 * Res ^ -0.862
                                ElseIf Res < 1000 Then
                                    fs = 6.76 * Res ^ -0.411
                                Else
                                    fs = 0.718 * Res ^ -0.008
                                End If
                            End If
                    End Select

                    'Cx
                    Dim Cx As Double = 0
                    Select Case STProperties.Tube_Layout
                        Case 0, 1
                            Cx = 1.154
                        Case 2
                            Cx = 1.0#
                        Case 3
                            Cx = 1.414
                    End Select
                    Dim Gsh, Ssh, Fh, Rsh, dis As Double
                    If STProperties.Shell_Fluid = 0 Then
                        dps = 4 * fs * Gsf ^ 2 / (2 * rhoc) * Cx * (1 - HDi) * Dsi / pitch * Nb * (1 + Y * pitch / Dsi)
                    Else
                        dps = 4 * fs * Gsf ^ 2 / (2 * rhoh) * Cx * (1 - HDi) * Dsi / pitch * Nb * (1 + Y * pitch / Dsi)
                    End If
                    dps *= Nc

                    'shell htc

                    Dim M As Double = 0.96#
                    dis = STProperties.Shell_Di / 1000
                    Fh = 1 / (1 + Nh * (dis / pitch) ^ 0.5)
                    Ssh = Ss * M / Fh
                    Ssh = Math.PI / 4 * (Dsi ^ 2 - nt * de ^ 2)
                    If STProperties.Shell_Fluid = 0 Then
                        Gsh = Wc / Ssh
                        Rsh = Gsh * de / muc
                    Else
                        Gsh = Wh / Ssh
                        Rsh = Gsh * de / muh
                    End If
                    Dim Ec, lb, he As Double
                    Select Case STProperties.Tube_Layout
                        Case 0, 1
                            If Rsh < 100 Then
                                jh = 0.497 * Rsh ^ 0.54
                            Else
                                jh = 0.378 * Rsh ^ 0.61
                            End If
                        Case 2, 3
                            If Rsh < 100 Then
                                jh = 0.385 * Rsh ^ 0.526
                            Else
                                jh = 0.2487 * Rsh ^ 0.625
                            End If
                    End Select
                    If STProperties.Shell_Fluid = 0 Then
                        he = jh * kc * Prs ^ 0.34 / de
                    Else
                        he = jh * kh * Prs ^ 0.34 / de
                    End If
                    lb = STProperties.Shell_BaffleSpacing / 1000 * (Nb - 1)
                    Ec = lb + (L - lb) * (2 * STProperties.Shell_BaffleSpacing / 1000 / (L - lb)) ^ 0.6 / L
                    If Double.IsNaN(Ec) Then Ec = 1
                    he *= Ec

                    'global HTC (U)

                    Dim kt As Double = STProperties.Tube_ThermalConductivity
                    Dim f1, f2, f3, f4, f5 As Double
                    f1 = de / (hi * di)
                    f2 = rt * de / di
                    f3 = de / (2 * kt) * Math.Log(de / di)
                    f4 = rs
                    f5 = 1 / he

                    U = f1 + f2 + f3 + f4 + f5

                    STProperties.OverallFoulingFactor = f2 + f4

                    U = 1 / U

                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            If (Th1 - Tc1) / (Th2 - Tc2) = 1 Then
                                LMTD = ((Th1 - Tc1) + (Th2 - Tc2)) / 2
                            Else
                                LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                            End If
                        Case FlowDirection.CounterCurrent
                            If (Th1 - Tc2) / (Th2 - Tc1) = 1 Then
                                LMTD = ((Th1 - Tc2) + (Th2 - Tc1)) / 2
                            Else
                                LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                            End If
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then LMTD = 0.0

                    Q = U / 1000 * A * LMTD / 1000 * timestep

                    If Q > MaxHeatExchange Then Q = MaxHeatExchange

                    If STProperties.Shell_Fluid = 0 Then
                        'cold
                        DeltaHc = (Q - HeatLoss) / Wc
                        DeltaHh = -Q / Wh
                    Else
                        'hot
                        DeltaHc = Q / Wc
                        DeltaHh = -(Q + HeatLoss) / Wh
                    End If

                    Hc2 = Hc1 + DeltaHc
                    Hh2 = Hh1 + DeltaHh

                    STProperties.Ft = f1 'tube side
                    STProperties.Fc = f3 'heat conductivity pipe
                    STProperties.Fs = f5 'shell side
                    STProperties.Ff = STProperties.OverallFoulingFactor
                    STProperties.ReS = Res 'Reynolds number shell side
                    STProperties.ReT = Ret 'Reynolds number tube side

                    If STProperties.Shell_Fluid = 0 Then
                        Pc2 = Pc1 - dps
                        Ph2 = Ph1 - dpt
                    Else
                        Pc2 = Pc1 - dpt
                        Ph2 = Ph1 - dps
                    End If

            End Select

            AccumulationStreamCold.PropertyPackage.CurrentMaterialStream = AccumulationStreamCold
            tmp = AccumulationStreamCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, Tc2)
            Tc2 = tmp.CalculatedTemperature

            AccumulationStreamHot.PropertyPackage.CurrentMaterialStream = StInHot
            tmp = AccumulationStreamHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, Th2)
            Th2 = tmp.CalculatedTemperature

            ThermalEfficiency = (Q - HeatLoss) / MaxHeatExchange * 100

            If HeatLoss > Math.Abs(Q.GetValueOrDefault) Then Throw New Exception("Invalid Heat Loss.")

            ColdSideOutletTemperature = Tc2
            HotSideOutletTemperature = Th2
            ColdSidePressureDrop = Pc1 - Pc2
            HotSidePressureDrop = Ph1 - Ph2
            OverallCoefficient = U
            Area = A

            SetDynamicProperty("Cold Side Pressure", (Pc1 + Pc2) / 2)
            SetDynamicProperty("Hot Side Pressure", (Ph1 + Ph2) / 2)

            'Define new calculated properties.

            AccumulationStreamHot.SetTemperature((Th1 + Th2) / 2)
            AccumulationStreamCold.SetTemperature((Tc1 + Tc2) / 2)
            AccumulationStreamHot.SetPressure(Ph1)
            AccumulationStreamCold.SetPressure(Pc1)
            AccumulationStreamHot.SetMassEnthalpy(Hh2)
            AccumulationStreamCold.SetMassEnthalpy(Hc2)

            StOutHot.AssignFromPhase(PhaseLabel.Mixture, AccumulationStreamHot, False)
            StOutCold.AssignFromPhase(PhaseLabel.Mixture, AccumulationStreamCold, False)

            StInHot.SetPressure(Ph1)
            StInCold.SetPressure(Pc1)

            If Th2 < Tc1 Or Tc2 > Th1 Then
                FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": Temperature Cross", IFlowsheet.MessageType.Warning)
            End If

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("DWSIM has a model for the countercurrent, two-stream heat 
                                exchanger which supports phase change and multiple phases in a 
                                stream.")

            IObj?.Paragraphs.Add("Input Parameters")

            IObj?.Paragraphs.Add("The heat exchanger in DWSIM has five calculation modes: ")

            IObj?.Paragraphs.Add("1. Calculate hot fluid outlet temperature: you must provide the 
                              cold fluid outlet temperature and the exchange area to 
                              calculate the hot fluid temperature.")

            IObj?.Paragraphs.Add("2. Calculate cold fluid outlet temperature: in this mode, DWSIM 
                                needs the hot fluid outlet temperature and the exchange area to 
                                calculate the cold fluid temperature.")

            IObj?.Paragraphs.Add("3. Calculate both temperatures: in this mode, DWSIM needs the 
                              exchange area and the heat exchanged to calculate both 
                              temperatures.")

            IObj?.Paragraphs.Add("4. Calculate area: in this mode you must provide the HTC and both temperatures to calculate the exchange area.")

            IObj?.Paragraphs.Add("5. Rate a Shell and Tube exchanger: in this mode you must provide 
                          the exchanger geometry and DWSIM will calculate output 
                          temperatures, pressure drop on the shell and tubes, overall 
                          HTC, LMTD, and exchange area. This calculation mode uses a 
                          simplified version of Tinker's method for Shell and Tube 
                          exchanger calculations. ")

            IObj?.Paragraphs.Add("You can provide the pressure drop for both fluids in the exchanger for modes 1 to 4 only.")

            IObj?.Paragraphs.Add("Calculation Mode")

            IObj?.Paragraphs.Add("The heat exchanger in DWSIM is calculated using the simple  convection heat equation:")

            IObj?.Paragraphs.Add("<m>Q=UA\Delta T_{ml},</m>")

            IObj?.Paragraphs.Add("where: Q = heat transferred, A = heat transfer area (external 
                            surface) and <mi>\Delta T_{ml}</mi> = Logarithmic Mean Temperature 
                            Difference (LMTD). We also remember that:")

            IObj?.Paragraphs.Add("<m>Q=m\Delta H,</m>")

            IObj?.Paragraphs.Add("where: <mi>Q</mi> = heat transferred from/to the fluid and <mi>\Delta H</mi> = outlet-inlet enthalpy difference.")

            IObj?.Paragraphs.Add("The calculation procedure depends on the mode selected:")

            IObj?.Paragraphs.Add("1. Calculate hot fluid outlet temperature: HTC (Heat Transfer Coefficient), hot fluid outlet temperature, heat load and LMTD.")

            IObj?.Paragraphs.Add("2. Calculate cold fluid outlet temperature: HTC, cold fluid outlet temperature, heat load and LMTD.")

            IObj?.Paragraphs.Add("3. Calculate both temperatures: HTC, cold and hot fluid outlet temperatures and LMTD.")

            IObj?.Paragraphs.Add("4. Calculate area: exchange area and LMTD.")

            IObj?.Paragraphs.Add("5. Rate Shell and Tube exchanger: exchanger geometry information.")

            IObj?.Paragraphs.Add("<h2>Inlet Streams</h2>")

            Dim Ti1, Ti2, w1, w2, A, Tc1, Th1, Wc, Wh, P1, P2, Th2, Tc2, U As Double
            Dim Pc1, Ph1, Pc2, Ph2, DeltaHc, DeltaHh, H1, H2, Hc1, Hh1, Hc2, Hh2, CPC, CPH As Double
            Dim StIn0, StIn1, StOut0, StOut1, StInCold, StInHot, StOutHot, StOutCold As MaterialStream
            Dim coldidx As Integer = 0

            'Validate unitop status.
            Me.Validate()

            HeatProfile = New Double() {}
            TemperatureProfileCold = New Double() {}
            TemperatureProfileHot = New Double() {}

            StIn0 = Me.GetInletMaterialStream(0)
            StIn1 = Me.GetInletMaterialStream(1)

            StOut0 = Me.GetOutletMaterialStream(0)
            StOut1 = Me.GetOutletMaterialStream(1)

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)
            If DebugMode Then AppendDebugLine("Validating inlet stream 1...")
            StIn0.Validate()
            If DebugMode Then AppendDebugLine("Validating inlet stream 2...")
            StIn1.Validate()

            'First input stream.
            Ti1 = StIn0.Phases(0).Properties.temperature.GetValueOrDefault
            w1 = StIn0.Phases(0).Properties.massflow.GetValueOrDefault
            P1 = StIn0.Phases(0).Properties.pressure.GetValueOrDefault
            H1 = StIn0.Phases(0).Properties.enthalpy.GetValueOrDefault
            'Second input stream.
            Ti2 = StIn1.Phases(0).Properties.temperature.GetValueOrDefault
            w2 = StIn1.Phases(0).Properties.massflow.GetValueOrDefault
            P2 = StIn1.Phases(0).Properties.pressure.GetValueOrDefault
            H2 = StIn1.Phases(0).Properties.enthalpy.GetValueOrDefault

            'Let us use properties at the entrance as an initial implementation.

            If Ti1 < Ti2 Then
                'Input1 is the cold stream.
                Tc1 = Ti1
                Th1 = Ti2
                Wc = w1
                Wh = w2
                Pc1 = P1
                Ph1 = P2
                Hc1 = H1
                Hh1 = H2
                coldidx = 0
                'Identify cold and hot streams.
                StInCold = StIn0
                StInHot = StIn1
                StOutCold = StOut0
                StOutHot = StOut1
            Else
                'Input2 is the cold stream.
                Tc1 = Ti2
                Th1 = Ti1
                Wc = w2
                Wh = w1
                Pc1 = P2
                Ph1 = P1
                Hc1 = H2
                Hh1 = H1
                coldidx = 1
                'Identify cold and hot streams.
                StInCold = StIn1
                StInHot = StIn0
                StOutCold = StOut1
                StOutHot = StOut0
            End If

            IObj?.Paragraphs.Add(String.Format("<h3>Cold Stream: {0}</h3>", StInCold.GraphicObject.Tag))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", StInCold.Phases(0).Properties.temperature.GetValueOrDefault))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", StInCold.Phases(0).Properties.pressure.GetValueOrDefault))
            IObj?.Paragraphs.Add(String.Format("Mass Flow: {0} kg/s", StInCold.Phases(0).Properties.massflow.GetValueOrDefault))
            IObj?.Paragraphs.Add(String.Format("Specific Enthalpy: {0} kJ/kg", StInCold.Phases(0).Properties.enthalpy.GetValueOrDefault))


            IObj?.Paragraphs.Add(String.Format("<h3>Hot Stream: {0}</h3>", StInHot.GraphicObject.Tag))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", StInHot.Phases(0).Properties.temperature.GetValueOrDefault))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", StInHot.Phases(0).Properties.pressure.GetValueOrDefault))
            IObj?.Paragraphs.Add(String.Format("Mass Flow: {0} kg/s", StInHot.Phases(0).Properties.massflow.GetValueOrDefault))
            IObj?.Paragraphs.Add(String.Format("Specific Enthalpy: {0} kJ/kg", StInHot.Phases(0).Properties.enthalpy.GetValueOrDefault))

            IObj?.Paragraphs.Add("<h2>Maximum Heat Exchange</h2>")

            IObj?.Paragraphs.Add("Calculating maximum theoretical heat exchange...")

            IObj?.Paragraphs.Add("The maximum theoretical heat exchange is calculated as the smallest value from")

            IObj?.Paragraphs.Add("<m>Q_{max,hot}=W_{hot}(H_{hot,in}-H_{hot,c})</m>")
            IObj?.Paragraphs.Add("<m>Q_{max,cold}=W_{cold}(H_{cold,h}-H_{cold,in})</m>")

            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<mi>H_{hot,in}</mi> is the hot stream inlet enthalpy")
            IObj?.Paragraphs.Add("<mi>H_{hot,c}</mi> is the hot stream enthalpy at cold stream inlet temperature")
            IObj?.Paragraphs.Add("<mi>H_{cold,in}</mi> is the cold stream inlet enthalpy ")
            IObj?.Paragraphs.Add("<mi>H_{cold,h}</mi> is the cold stream enthalpy at hot stream inlet temperature")

            Pc2 = Pc1 - ColdSidePressureDrop
            Ph2 = Ph1 - HotSidePressureDrop

            If DebugMode Then AppendDebugLine(StInCold.GraphicObject.Tag & " is the cold stream.")
            If DebugMode Then AppendDebugLine(StInHot.GraphicObject.Tag & " is the hot stream.")

            'calculate maximum theoretical heat exchange
            If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate hot stream outlet enthalpy... P = {0} Pa, T = {1} K", Ph2, Tc1))
            Dim HHx As Double
            Dim tmpstr As MaterialStream = StInHot.Clone
            tmpstr.PropertyPackage = StInHot.PropertyPackage.Clone
            tmpstr.SetFlowsheet(StInHot.FlowSheet)
            tmpstr.PropertyPackage.CurrentMaterialStream = tmpstr
            tmpstr.Phases("0").Properties.temperature = Tc1
            tmpstr.Phases("0").Properties.pressure = Ph2
            IObj?.SetCurrent()
            tmpstr.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
            IObj?.SetCurrent()
            tmpstr.Calculate(False, True)
            HHx = tmpstr.Phases(0).Properties.enthalpy.GetValueOrDefault
            DeltaHh = Wh * (Hh1 - HHx) 'kW

            IObj?.Paragraphs.Add("<mi>Q_{max,hot}</mi> = " & DeltaHh & " kW")

            If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate cold stream outlet enthalpy... P = {0} Pa, T = {1} K", Pc2, Th1))
            tmpstr = StInCold.Clone
            tmpstr.PropertyPackage = StInCold.PropertyPackage.Clone
            tmpstr.SetFlowsheet(StInHot.FlowSheet)
            tmpstr.PropertyPackage.CurrentMaterialStream = tmpstr
            tmpstr.Phases("0").Properties.temperature = Th1
            tmpstr.Phases("0").Properties.pressure = Pc2
            IObj?.SetCurrent()
            tmpstr.PropertyPackage.DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
            IObj?.SetCurrent()
            tmpstr.Calculate(False, True)
            HHx = tmpstr.Phases(0).Properties.enthalpy.GetValueOrDefault
            DeltaHc = Wc * (HHx - Hc1) 'kW

            IObj?.Paragraphs.Add("<mi>Q_{max,cold}</mi> = " & DeltaHc & " kW")

            MaxHeatExchange = Min(DeltaHc, DeltaHh) 'kW

            IObj?.Paragraphs.Add("<mi>Q_{max}</mi> = " & MaxHeatExchange & " kW")

            tmpstr.PropertyPackage = Nothing
            tmpstr.Dispose()
            tmpstr = Nothing

            If DebugMode Then AppendDebugLine("Maximum possible heat exchange is " & MaxHeatExchange.ToString & " kW.")

            'Copy properties from the input streams.
            StOut0.Assign(StIn0)
            StOut1.Assign(StIn1)

            CPC = StInCold.Phases(0).Properties.heatCapacityCp.GetValueOrDefault
            CPH = StInHot.Phases(0).Properties.heatCapacityCp.GetValueOrDefault

            IObj?.Paragraphs.Add("<h2>Actual Heat Exchange</h2>")

            IObj?.Paragraphs.Add("<mi>Q_{loss}</mi> = " & HeatLoss & " kW")

            IObj?.Paragraphs.Add("Calculating heat exchanged...")

            IObj?.Paragraphs.Add(String.Format("Calculation mode: {0}", [Enum].GetName(CalcMode.GetType, CalcMode)))

            Select Case CalcMode

                Case HeatExchangerCalcMode.ThermalEfficiency

                    Q = MaxHeatExchange * ThermalEfficiency / 100.0

                    If Q.GetValueOrDefault() / MaxHeatExchange > 1.001 Then
                        Throw New Exception("Defined heat exchange is invalid (higher than the theoretical maximum).")
                    End If

                    DeltaHc = Q / Wc
                    DeltaHh = -(Q + HeatLoss) / Wh
                    Hc2 = Hc1 + DeltaHc
                    Hh2 = Hh1 + DeltaHh

                    StInCold.PropertyPackage.CurrentMaterialStream = StInCold

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate cold stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Pc2, Hc2))
                    IObj?.SetCurrent()
                    Dim tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, Tc1)
                    Tc2 = tmp.CalculatedTemperature
                    Hh2 = Hh1 + DeltaHh
                    StInHot.PropertyPackage.CurrentMaterialStream = StInHot

                    If DebugMode Then AppendDebugLine(String.Format("Calculated cold stream outlet temperature T2 = {0} K", Tc2))
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate hot stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Ph2, Hh2))

                    IObj?.SetCurrent()
                    tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, Th1)
                    Th2 = tmp.CalculatedTemperature

                    If DebugMode Then AppendDebugLine(String.Format("Calculated hot stream outlet temperature T2 = {0} K", Th2))

                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                        Case FlowDirection.CounterCurrent
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = OverallCoefficient.GetValueOrDefault

                    A = Q * 1000 / U / LMTD

                    Area = A

                Case HeatExchangerCalcMode.PinchPoint

                    Dim dhc, dhh, dq, fx As Double, nsteps As Integer

                    nsteps = 25

                    Dim tcprof, thprof, dtprof, qprof, seg_ua, seg_lmtd As New List(Of Double)

                    Dim brt As New MathOps.MathEx.BrentOpt.BrentMinimize

                    dq = brt.brentoptimize2(0, MaxHeatExchange, 0.01,
                                             Function(dqx)

                                                 dhc = dqx / Wc
                                                 dhh = dqx / Wh

                                                 'calculate profiles

                                                 tcprof.Clear()
                                                 thprof.Clear()
                                                 dtprof.Clear()
                                                 qprof.Clear()

                                                 tmpstr = StInCold.Clone
                                                 tmpstr.PropertyPackage = StInCold.PropertyPackage
                                                 tmpstr.SetFlowsheet(StInCold.FlowSheet)

                                                 For i As Integer = 0 To nsteps

                                                     tmpstr.Phases(0).Properties.enthalpy = Hc1 + Convert.ToDouble(i) / Convert.ToDouble(nsteps) * dhc
                                                     tmpstr.Phases(0).Properties.pressure = Pc1 - Convert.ToDouble(i) / Convert.ToDouble(nsteps) * ColdSidePressureDrop
                                                     tmpstr.SpecType = StreamSpec.Pressure_and_Enthalpy
                                                     IObj?.SetCurrent()
                                                     tmpstr.Calculate(True, True)

                                                     qprof.Add(i / nsteps * dqx)
                                                     tcprof.Add(tmpstr.Phases(0).Properties.temperature.GetValueOrDefault)

                                                 Next

                                                 tmpstr = StInHot.Clone
                                                 tmpstr.PropertyPackage = StInHot.PropertyPackage
                                                 tmpstr.SetFlowsheet(StInHot.FlowSheet)

                                                 For i As Integer = 0 To nsteps

                                                     tmpstr.Phases(0).Properties.enthalpy = Hh1 - Convert.ToDouble(i) / Convert.ToDouble(nsteps) * dhh
                                                     tmpstr.Phases(0).Properties.pressure = Ph1 - Convert.ToDouble(i) / Convert.ToDouble(nsteps) * HotSidePressureDrop
                                                     tmpstr.SpecType = StreamSpec.Pressure_and_Enthalpy
                                                     IObj?.SetCurrent()
                                                     tmpstr.Calculate(True, True)

                                                     thprof.Add(tmpstr.Phases(0).Properties.temperature.GetValueOrDefault)

                                                 Next

                                                 If Not PinchPointAtOutlets And FlowDir = FlowDirection.CounterCurrent Then
                                                     thprof.Reverse()
                                                 End If

                                                 seg_ua.Clear()
                                                 seg_lmtd.Clear()
                                                 For i As Integer = 0 To nsteps
                                                     dtprof.Add(Abs(thprof(i) - tcprof(i)))
                                                     If i > 0 Then
                                                         seg_lmtd.Add((dtprof(i) - dtprof(i - 1)) / Log(dtprof(i) / dtprof(i - 1)))
                                                         seg_ua.Add((qprof(i) - qprof(i - 1)) / seg_lmtd.Last)
                                                     End If
                                                 Next

                                                 fx = dtprof.Min - MITA

                                                 Return fx ^ 2

                                             End Function)

                    If Double.IsNaN(fx) Or Double.IsNaN(dhc) Then Throw New Exception("Error calculating temperature profile.")

                    Me.HeatProfile = qprof.ToArray
                    Me.TemperatureProfileCold = tcprof.ToArray
                    Me.TemperatureProfileHot = thprof.ToArray

                    dhc = dq / Wc

                    Hc2 = Hc1 + dhc
                    Q = dhc * Wc

                    Tc2 = tcprof.Last

                    Dim tmp As IFlashCalculationResult

                    DeltaHh = -(Q + HeatLoss) / Wh

                    Hh2 = Hh1 + DeltaHh
                    StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate hot stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Ph2, Hh2))
                    IObj?.SetCurrent()
                    tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, 0)
                    Th2 = tmp.CalculatedTemperature
                    If DebugMode Then AppendDebugLine(String.Format("Calculated hot stream outlet temperature T2 = {0} K", Th2))

                    LMTD = Q / seg_ua.Sum

                    LMTD *= CorrectionFactorLMTD

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = Me.OverallCoefficient

                    A = Q / (LMTD * U) * 1000

                    'If Double.IsNaN(A) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                Case HeatExchangerCalcMode.CalcBothTemp_UA

                    Dim Qi, Q_old, PIc1, PIc2, PIh1, PIh2 As Double
                    Dim NTUh, NTUc, WWh, WWc, RRh, RRc, PPh, PPc As Double
                    Dim tmp As IFlashCalculationResult
                    Dim count As Integer
                    A = Area
                    U = OverallCoefficient
                    Qi = MaxHeatExchange * 0.7
                    Q_old = 10000000000.0
                    Tc2 = Tc1 + (Th1 - Tc1) / 2 * 0.7
                    Th2 = Th1 - (Th1 - Tc1) / 2 * 0.7

                    If DebugMode Then AppendDebugLine(String.Format("Start with Max Heat Exchange Q = {0} KW", Qi))

                    Do

                        If DebugMode Then AppendDebugLine(String.Format("======================================================"))
                        If DebugMode Then AppendDebugLine(String.Format("Iteration loop: {0}", count))

                        Hc2 = Qi / Wc + Hc1
                        Hh2 = Hh1 - Qi / Wh - HeatLoss / Wh
                        StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                        IObj?.SetCurrent()
                        tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, Tc2)
                        Tc2 = tmp.CalculatedTemperature
                        PIc2 = (1 + tmp.GetLiquidPhase1MoleFraction) * (1 + tmp.GetVaporPhaseMoleFraction * (1 + tmp.GetSolidPhaseMoleFraction)) 'phase indicator cold stream
                        If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate cold stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]  ===> Tc2 = {2} K", Pc2, Hc2, Tc2))

                        StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                        IObj?.SetCurrent()
                        tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, Th2)
                        Th2 = tmp.CalculatedTemperature
                        PIh2 = (1 + tmp.GetLiquidPhase1MoleFraction) * (1 + tmp.GetVaporPhaseMoleFraction * (1 + tmp.GetSolidPhaseMoleFraction)) 'phase indicator hot stream
                        If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate hot stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]  ===> Th2 = {2} K", Ph2, Hh2, Th2))

                        If Abs((Qi - Q_old) / Q_old) < 0.001 Or count > 300 Then Exit Do

                        WWc = Wc * (Hc2 - Hc1) / (Tc2 - Tc1) * 1000 'Heat Capacity Rate cold side
                        WWh = Wh * (Hh2 - Hh1) / (Th2 - Th1) * 1000 'Heat Capacity Rate hot side
                        NTUc = U * A / WWc 'Numbers of transfer units - cold side
                        NTUh = U * A / WWh 'Numbers of transfer units - hot side
                        RRc = WWc / WWh 'Heat capacity ratio cold side
                        RRh = WWh / WWc 'Heat capacity ratio hot side

                        If DebugMode Then AppendDebugLine(String.Format("Calculating heat exchanger"))
                        If DebugMode Then AppendDebugLine(String.Format("Number of Transfer Units - NTU_cold :{0}  NTU_hot: {1}", NTUc, NTUh))
                        If DebugMode Then AppendDebugLine(String.Format("Heat Capacity Rates - W_cold :{0}  W_hot: {1}", WWc, WWh))
                        If DebugMode Then AppendDebugLine(String.Format("Heat Capacity Ratios - R_cold :{0}  R_hot: {1}", RRc, RRh))

                        Select Case Me.FlowDir
                            Case FlowDirection.CoCurrent
                                PPc = (1 - Exp(-NTUc * (1 + RRc))) / (1 + RRc)
                                PPh = (1 - Exp(-NTUh * (1 + RRh))) / (1 + RRh)
                            Case FlowDirection.CounterCurrent
                                PPc = (1 - Exp((RRc - 1) * NTUc)) / (1 - RRc * Exp((RRc - 1) * NTUc))
                                PPh = (1 - Exp((RRh - 1) * NTUh)) / (1 - RRh * Exp((RRh - 1) * NTUh))
                        End Select
                        If DebugMode Then AppendDebugLine(String.Format("Dimensionless Temp Change - P_cold :{0}  P_hot: {1}", PPc, PPh))

                        If Double.IsNaN(PPc) Or Double.IsNaN(PPh) Then
                            Throw New Exception("failed to calculate the Number of Transfer Units (NTU) with the current input and specs")
                        End If

                        Tc2 = Tc1 + PPc * (Th1 - Tc1)
                        Th2 = Th1 - PPh * (Th1 - Tc1)
                        If DebugMode Then AppendDebugLine(String.Format("Outlet Temperatures - Tc2 :{0} K  Th2: {1} K", Tc2, Th2))

                        Select Case Me.FlowDir
                            Case FlowDirection.CoCurrent
                                If (Th1 - Tc1) / (Th2 - Tc2) = 1 Then
                                    LMTD = ((Th1 - Tc1) + (Th2 - Tc2)) / 2
                                Else
                                    LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                                End If
                            Case FlowDirection.CounterCurrent
                                If (Th1 - Tc2) / (Th2 - Tc1) = 1 Then
                                    LMTD = ((Th1 - Tc2) + (Th2 - Tc1)) / 2
                                Else
                                    LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                                End If
                        End Select
                        LMTD *= CorrectionFactorLMTD
                        Q_old = Qi
                        If LMTD > 0 Then
                            Qi = U * A * LMTD / 1000
                        Else
                            Qi = Wh * (Hh1 - Hh2)
                            LMTD = Qi / U / A * 1000
                        End If

                        If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                        If DebugMode Then
                            AppendDebugLine(String.Format("Logarithmic Temperature Difference :{0} K", LMTD))
                            AppendDebugLine(String.Format("Heat Exchange Q = {0} KW", Qi))
                        End If

                        count += 1

                    Loop

                    ColdSideOutletTemperature = Tc2

                    Q = Qi

                    If count > 300 Then Throw New Exception("Reached maximum number of iterations! Final Q change: " & Qi - Q_old & " kW ; " & Abs((Qi - Q_old) / Q_old * 100) & " % ")

                    PIc1 = (1 + StInCold.Phases(1).Properties.molarfraction.GetValueOrDefault) * (1 + StInCold.Phases(2).Properties.molarfraction.GetValueOrDefault) * (1 + StInCold.Phases(7).Properties.molarfraction.GetValueOrDefault)
                    PIh1 = (1 + StInHot.Phases(1).Properties.molarfraction.GetValueOrDefault) * (1 + StInHot.Phases(2).Properties.molarfraction.GetValueOrDefault) * (1 + StInHot.Phases(7).Properties.molarfraction.GetValueOrDefault)

                    If (PIc1 = 2 And PIc2 > 2) Or (PIc1 > 2 And PIc2 = 2) Then FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": Phase change in cold stream detected! Heat exchange result is an aproximation.", IFlowsheet.MessageType.Warning)
                    If (PIh1 = 2 And PIh2 > 2) Or (PIh1 > 2 And PIh2 = 2) Then FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": Phase change in hot stream detected! Heat exchange result is an aproximation.", IFlowsheet.MessageType.Warning)

                Case HeatExchangerCalcMode.CalcBothTemp

                    If Q > MaxHeatExchange Then Throw New Exception("Defined heat exchange is invalid (higher than the theoretical maximum).")

                    A = Area
                    DeltaHc = Q / Wc
                    DeltaHh = -(Q + HeatLoss) / Wh
                    Hc2 = Hc1 + DeltaHc
                    Hh2 = Hh1 + DeltaHh

                    StInCold.PropertyPackage.CurrentMaterialStream = StInCold

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate cold stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Pc2, Hc2))
                    IObj?.SetCurrent()
                    Dim tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, Tc1)
                    Tc2 = tmp.CalculatedTemperature
                    Hh2 = Hh1 + DeltaHh
                    StInHot.PropertyPackage.CurrentMaterialStream = StInHot

                    If DebugMode Then AppendDebugLine(String.Format("Calculated cold stream outlet temperature T2 = {0} K", Tc2))
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate hot stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Ph2, Hh2))

                    IObj?.SetCurrent()
                    tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, Th1)
                    Th2 = tmp.CalculatedTemperature

                    If DebugMode Then AppendDebugLine(String.Format("Calculated hot stream outlet temperature T2 = {0} K", Th2))

                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                        Case FlowDirection.CounterCurrent
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = Q / (A * LMTD) * 1000

                Case HeatExchangerCalcMode.CalcTempColdOut

                    A = Area
                    Th2 = TempHotOut

                    StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate hot stream outlet enthalpy... P = {0} Pa, T = K", Ph2, Th2))
                    IObj?.SetCurrent()
                    Dim tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, Ph2, Th2, 0.0#)
                    Hh2 = tmp.CalculatedEnthalpy
                    Q = -Wh * (Hh2 - Hh1)
                    If Q > MaxHeatExchange Then
                        Throw New Exception(String.Format("Invalid Outlet Temperature for Hot Fluid: {0} kW required but only {1} kW are available", Q, MaxHeatExchange))
                    End If
                    DeltaHc = (Q - HeatLoss) / Wc
                    Hc2 = Hc1 + DeltaHc
                    StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate cold stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Pc2, Hc2))
                    IObj?.SetCurrent()
                    tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, Th2)
                    Tc2 = tmp.CalculatedTemperature
                    If DebugMode Then AppendDebugLine(String.Format("Calculated cold stream outlet temperature T2 = {0} K", Tc2))
                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                        Case FlowDirection.CounterCurrent
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                    End Select

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = Q / (A * LMTD) * 1000

                Case HeatExchangerCalcMode.CalcTempHotOut

                    A = Area
                    Tc2 = TempColdOut
                    StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate cold stream outlet enthalpy... P = {0} Pa, T = K", Pc2, Tc2))
                    IObj?.SetCurrent()
                    Dim tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, Pc2, Tc2, 0)
                    Hc2 = tmp.CalculatedEnthalpy
                    Q = Wc * (Hc2 - Hc1)
                    If Q > MaxHeatExchange Then
                        Throw New Exception(String.Format("Invalid Outlet Temperature for Cold Fluid: {0} kW required but only {1} kW are available", Q, MaxHeatExchange))
                    End If
                    DeltaHh = -(Q + HeatLoss) / Wh
                    Hh2 = Hh1 + DeltaHh
                    StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                    IObj?.SetCurrent()
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate hot stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Ph2, Hh2))
                    tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, Th1)
                    Th2 = tmp.CalculatedTemperature
                    If DebugMode Then AppendDebugLine(String.Format("Calculated hot stream outlet temperature T2 = {0} K", Th2))

                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                        Case FlowDirection.CounterCurrent
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = Q / (A * LMTD) * 1000

                Case HeatExchangerCalcMode.CalcArea

                    Select Case Me.DefinedTemperature
                        Case SpecifiedTemperature.Cold_Fluid
                            Tc2 = TempColdOut
                            StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                            If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate cold stream outlet enthalpy... P = {0} Pa, T = {1} K", Pc2, Tc2))
                            IObj?.SetCurrent()
                            Dim tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, Pc2, Tc2, 0)
                            Hc2 = tmp.CalculatedEnthalpy
                            Q = Wc * (Hc2 - Hc1)
                            DeltaHh = -(Q + HeatLoss) / Wh
                            Hh2 = Hh1 + DeltaHh
                            StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                            If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate hot stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Ph2, Hh2))
                            IObj?.SetCurrent()
                            tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, 0)
                            Th2 = tmp.CalculatedTemperature
                            If DebugMode Then AppendDebugLine(String.Format("Calculated hot stream outlet temperature T2 = {0} K", Th2))
                        Case SpecifiedTemperature.Hot_Fluid
                            Th2 = TempHotOut
                            StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                            If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate hot stream outlet enthalpy... P = {0} Pa, T = {1} K", Ph2, Th2))
                            IObj?.SetCurrent()
                            Dim tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, Ph2, Th2, 0)
                            Hh2 = tmp.CalculatedEnthalpy
                            Q = -Wh * (Hh2 - Hh1)
                            DeltaHc = (Q - HeatLoss) / Wc
                            Hc2 = Hc1 + DeltaHc
                            StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                            If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate cold stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Pc2, Hc2))
                            IObj?.SetCurrent()
                            tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, 0)
                            Tc2 = tmp.CalculatedTemperature
                            If DebugMode Then AppendDebugLine(String.Format("Calculated cold stream outlet temperature T2 = {0} K", Tc2))
                    End Select
                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                        Case FlowDirection.CounterCurrent
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = Me.OverallCoefficient

                    A = Q / (LMTD * U) * 1000

                Case HeatExchangerCalcMode.OutletVaporFraction1

                    Dim Q1, H20, H21, H10, H11, VF0, T10, T11, T20, T21, DP1, DP2 As Double

                    T10 = StIn0.GetTemperature()
                    T20 = StIn1.GetTemperature()

                    If T10 > T20 Then
                        P1 = Ph1
                        P2 = Pc1
                        DP1 = HotSidePressureDrop
                        DP2 = ColdSidePressureDrop
                    Else
                        P2 = Ph1
                        P1 = Pc1
                        DP2 = HotSidePressureDrop
                        DP1 = ColdSidePressureDrop
                    End If

                    A = Area

                    VF0 = StIn0.GetPhase("Vapor").Properties.molarfraction.GetValueOrDefault()
                    H10 = StIn0.GetMassEnthalpy()

                    StIn0.PropertyPackage.CurrentMaterialStream = StIn0
                    IObj?.SetCurrent()
                    Dim tmp = StIn0.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, P1 - DP1, OutletVaporFraction1, T10)
                    T11 = tmp.CalculatedTemperature.GetValueOrDefault()
                    H11 = tmp.CalculatedEnthalpy()
                    Q1 = -StIn0.GetMassFlow() * (tmp.CalculatedEnthalpy - StIn0.GetMassEnthalpy())

                    Q = Math.Abs(Q1)

                    If Q > MaxHeatExchange Then

                        Q = MaxHeatExchange

                        H11 = H10 - (Math.Sign(Q1) * Q - HeatLoss) / StIn1.GetMassFlow()

                        StIn1.PropertyPackage.CurrentMaterialStream = StIn1
                        IObj?.SetCurrent()
                        tmp = StIn1.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P1 - DP1, H11, T11)
                        T11 = tmp.CalculatedTemperature.GetValueOrDefault()

                    End If

                    H20 = StIn1.GetMassEnthalpy()
                    H21 = H20 + (Math.Sign(Q1) * Q - HeatLoss) / StIn1.GetMassFlow()

                    StIn1.PropertyPackage.CurrentMaterialStream = StIn1
                    IObj?.SetCurrent()
                    tmp = StIn1.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2 - DP2, H21, T21)
                    T21 = tmp.CalculatedTemperature.GetValueOrDefault()
                    'OutletVaporFraction2 = tmp.GetVaporPhaseMoleFraction()

                    If T10 > T20 Then
                        Tc1 = T20
                        Tc2 = T21
                        Th1 = T10
                        Th2 = T11
                        Ph2 = Ph1 - DP1
                        Pc2 = Pc1 - DP2
                        Hc2 = H21
                        Hh2 = H11
                    Else
                        Tc1 = T10
                        Tc2 = T11
                        Th1 = T20
                        Th2 = T21
                        Ph2 = Ph1 - DP2
                        Pc2 = Pc1 - DP1
                        Hc2 = H11
                        Hh2 = H21
                    End If

                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                        Case FlowDirection.CounterCurrent
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = Q / (A * LMTD) * 1000

                Case HeatExchangerCalcMode.OutletVaporFraction2

                    Dim Q1, H10, H11, H20, H21, VF0, T10, T11, T20, T21, DP1, DP2 As Double

                    If T10 > T20 Then
                        P1 = Ph1
                        P2 = Pc1
                        DP1 = HotSidePressureDrop
                        DP2 = ColdSidePressureDrop
                    Else
                        P2 = Ph1
                        P1 = Pc1
                        DP2 = HotSidePressureDrop
                        DP1 = ColdSidePressureDrop
                    End If

                    A = Area

                    VF0 = StIn1.GetPhase("Vapor").Properties.molarfraction.GetValueOrDefault()
                    T10 = StIn1.GetTemperature()
                    H10 = StIn1.GetMassEnthalpy()

                    StIn1.PropertyPackage.CurrentMaterialStream = StIn1
                    IObj?.SetCurrent()
                    Dim tmp = StIn1.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureVaporFraction, P1 - DP1, OutletVaporFraction2, T10)
                    T11 = tmp.CalculatedTemperature.GetValueOrDefault()
                    H11 = tmp.CalculatedEnthalpy()
                    Q1 = -StIn1.GetMassFlow() * (tmp.CalculatedEnthalpy - StIn1.GetMassEnthalpy())

                    Q = Math.Abs(Q1)

                    If Q > MaxHeatExchange Then

                        Q = MaxHeatExchange

                        H11 = H10 - (Math.Sign(Q1) * Q - HeatLoss) / StIn1.GetMassFlow()

                        StIn1.PropertyPackage.CurrentMaterialStream = StIn1
                        IObj?.SetCurrent()
                        tmp = StIn1.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P1 - DP1, H11, T11)
                        T11 = tmp.CalculatedTemperature.GetValueOrDefault()

                    End If

                    T20 = StIn0.GetTemperature()
                    H20 = StIn0.GetMassEnthalpy()
                    H21 = H20 + (Math.Sign(Q1) * Q - HeatLoss) / StIn0.GetMassFlow()

                    StIn0.PropertyPackage.CurrentMaterialStream = StIn0
                    IObj?.SetCurrent()
                    tmp = StIn0.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2 - DP2, H21, T21)
                    T21 = tmp.CalculatedTemperature.GetValueOrDefault()
                    'OutletVaporFraction1 = tmp.GetVaporPhaseMoleFraction()

                    If T10 > T20 Then
                        Tc1 = T20
                        Tc2 = T21
                        Th1 = T10
                        Th2 = T11
                        Ph2 = Ph1 - DP1
                        Pc2 = Pc1 - DP2
                        Hc2 = H21
                        Hh2 = H11
                    Else
                        Tc1 = T10
                        Tc2 = T11
                        Th1 = T20
                        Th2 = T21
                        Ph2 = Ph1 - DP2
                        Pc2 = Pc1 - DP1
                        Hc2 = H11
                        Hh2 = H21
                    End If

                    Select Case Me.FlowDir
                        Case FlowDirection.CoCurrent
                            LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                        Case FlowDirection.CounterCurrent
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                    End Select

                    LMTD *= CorrectionFactorLMTD

                    If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                    U = Q / (A * LMTD) * 1000

                Case HeatExchangerCalcMode.ShellandTube_Rating, HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor

                    'Shell and Tube HX calculation using Tinker's method.

                    IObj?.Paragraphs.Add("Shell and Tube HX calculation uses Tinker's method, more details <a href='http://essel.com.br/cursos/03_trocadores.htm'>on this link</a>, Chapter 5 (Capitulo 5).")

                    Dim Tc2_ant, Th2_ant As Double
                    Dim Ud, Ur, U_ant, Rf, fx, Fant, F As Double
                    Dim DTm, Tcm, Thm, R, Sf, P As Double

                    If CalculationMode = HeatExchangerCalcMode.ShellandTube_Rating Then

                        'initial estimates for R and P to calculate outlet temperatures

                        R = 0.4
                        P = 0.6

                        If STProperties.Tube_Fluid = 0 Then
                            'cold
                            Tc2 = P * (Th1 - Tc1) + Tc1
                            Th2 = Th1 - R * (Tc2 - Tc1)
                        Else
                            'hot
                            Th2 = P * (Tc1 - Th1) + Th1
                            Tc2 = Tc1 - R * (Th2 - Th1)
                        End If

                        If Th2 <= Tc2 Then Th2 = Tc2 * 1.2

                    Else

                        Tc2 = TempColdOut
                        Th2 = TempHotOut

                    End If

                    Pc2 = Pc1
                    Ph2 = Ph1
                    F = 1.0#
                    U = 500.0#

                    IObj?.Paragraphs.Add("<h3>Initial Estimates</h3>")

                    IObj?.Paragraphs.Add("<mi>T_{c,out}</mi> = " & Tc2 & " K")
                    IObj?.Paragraphs.Add("<mi>T_{h,out}</mi> = " & Th2 & " K")
                    IObj?.Paragraphs.Add("<mi>U</mi> = " & U & " W/[m2.K]")

                    Dim rhoc, muc, kc, rhoh, muh, kh, rs, rt, Atc, Nc, di, de, pitch, L, n, hi, nt, vt, Ret, Prt As Double

                    Dim icnt As Integer = 0

                    Do

                        IObj?.Paragraphs.Add("<h4>Convergence Loop #" & icnt & "</h4>")

                        Select Case Me.FlowDir
                            Case FlowDirection.CoCurrent
                                LMTD = ((Th1 - Tc1) - (Th2 - Tc2)) / Math.Log((Th1 - Tc1) / (Th2 - Tc2))
                            Case FlowDirection.CounterCurrent
                                LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                        End Select

                        LMTD *= CorrectionFactorLMTD

                        IObj?.Paragraphs.Add("<mi>\Delta T_{ml}</mi> = " & LMTD & " K")

                        If Not IgnoreLMTDError Then If Double.IsNaN(LMTD) Or Double.IsInfinity(LMTD) Then Throw New Exception(FlowSheet.GetTranslatedString("HXCalcError"))

                        If STProperties.Tube_Fluid = 0 Then
                            'cold
                            R = (Th1 - Th2) / (Tc2 - Tc1)
                            P = (Tc2 - Tc1) / (Th1 - Tc1)
                        Else
                            'hot
                            R = (Tc1 - Tc2) / (Th2 - Th1)
                            P = (Th2 - Th1) / (Tc1 - Th1)
                        End If

                        IObj?.Paragraphs.Add("<mi>R</mi> = " & R)
                        IObj?.Paragraphs.Add("<mi>P</mi> = " & P)

                        Fant = F

                        If R <> 1.0# Then
                            Dim alpha As Double
                            alpha = ((1 - R * P) / (1 - P)) ^ (1 / Me.STProperties.Shell_NumberOfPasses)
                            Sf = (alpha - 1) / (alpha - R)
                            F = (R ^ 2 + 1) ^ 0.5 * Math.Log((1 - Sf) / (1 - R * Sf)) / ((R - 1) * Math.Log((2 - Sf * (R + 1 - (R ^ 2 + 1) ^ 0.5)) / (2 - Sf * (R + 1 + (R ^ 2 + 1) ^ 0.5))))
                        Else
                            Sf = P / (Me.STProperties.Shell_NumberOfPasses * (1 - P) + P)
                            F = Sf * 2 ^ 0.5 / ((1 - Sf) * Math.Log((2 * (1 - Sf) + Sf * 2 ^ 0.5) / (2 * (1 - Sf) - Sf * 2 ^ 0.5)))
                        End If
                        If Double.IsNaN(F) Then
                            F = Fant
                            'Throw New Exception("LMTD correction factor 'F'  could not be calculated. R = " & R & ", S = " & Sf)
                        End If
                        DTm = F * LMTD
                        '3
                        Tcm = (Tc2 - Tc1) / 2 + Tc1
                        Thm = (Th1 - Th2) / 2 + Th2


                        IObj?.Paragraphs.Add("<mi>F</mi> = " & F)
                        IObj?.Paragraphs.Add("<mi>\Delta T_m</mi> = " & DTm & " K")
                        IObj?.Paragraphs.Add("<mi>T_{c,m}</mi> = " & Tcm & " K")
                        IObj?.Paragraphs.Add("<mi>T_{h,m}</mi> = " & Thm & " K")

                        '4, 5

                        StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                        IObj?.SetCurrent()
                        Dim tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, Pc2, Tc2, 0)
                        Dim tms As MaterialStream = StInCold.Clone
                        tms.SetFlowsheet(StInCold.FlowSheet)
                        tms.Phases(0).Properties.temperature = Tcm
                        With tms.PropertyPackage
                            .CurrentMaterialStream = tms
                            IObj?.SetCurrent()
                            .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                            If tms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid1)
                            Else
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid1)
                            End If
                            If tms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                            Else
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Vapor)
                            End If
                            If tms.Phases(2).Properties.molarfraction.GetValueOrDefault >= 0 And tms.Phases(2).Properties.molarfraction.GetValueOrDefault <= 1 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid)
                            Else
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid)
                            End If
                            IObj?.SetCurrent()
                            tms.PropertyPackage.DW_CalcPhaseProps(PropertyPackages.Phase.Mixture)
                        End With
                        rhoc = tms.Phases(0).Properties.density.GetValueOrDefault
                        CPC = tms.Phases(0).Properties.heatCapacityCp.GetValueOrDefault
                        kc = tms.Phases(0).Properties.thermalConductivity.GetValueOrDefault
                        muc = tms.Phases(1).Properties.viscosity.GetValueOrDefault * tms.Phases(1).Properties.molarfraction.GetValueOrDefault + tms.Phases(2).Properties.viscosity.GetValueOrDefault * tms.Phases(2).Properties.molarfraction.GetValueOrDefault
                        tms = StInHot.Clone
                        tms.SetFlowsheet(StInHot.FlowSheet)
                        tms.Phases(0).Properties.temperature = Thm
                        tms.PropertyPackage.CurrentMaterialStream = tms
                        With tms.PropertyPackage
                            .CurrentMaterialStream = tms
                            IObj?.SetCurrent()
                            .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                            If tms.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid1)
                            Else
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid1)
                            End If
                            If tms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                            Else
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Vapor)
                            End If
                            If tms.Phases(2).Properties.molarfraction.GetValueOrDefault >= 0 And tms.Phases(2).Properties.molarfraction.GetValueOrDefault <= 1 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid)
                            Else
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid)
                            End If
                            IObj?.SetCurrent()
                            tms.PropertyPackage.DW_CalcPhaseProps(PropertyPackages.Phase.Mixture)
                        End With
                        rhoh = tms.Phases(0).Properties.density.GetValueOrDefault
                        CPH = tms.Phases(0).Properties.heatCapacityCp.GetValueOrDefault
                        kh = tms.Phases(0).Properties.thermalConductivity.GetValueOrDefault
                        muh = tms.Phases(1).Properties.viscosity.GetValueOrDefault * tms.Phases(1).Properties.molarfraction.GetValueOrDefault + tms.Phases(2).Properties.viscosity.GetValueOrDefault * tms.Phases(2).Properties.molarfraction.GetValueOrDefault

                        '6

                        rs = Me.STProperties.Shell_Fouling
                        rt = Me.STProperties.Tube_Fouling
                        Nc = STProperties.Shell_NumberOfShellsInSeries
                        de = STProperties.Tube_De / 1000
                        di = STProperties.Tube_Di / 1000
                        L = STProperties.Tube_Length
                        pitch = STProperties.Tube_Pitch / 1000
                        n = STProperties.Tube_NumberPerShell
                        nt = n / STProperties.Tube_PassesPerShell
                        A = n * Math.PI * de * (L - 2 * de)
                        Atc = A / Nc

                        If pitch < de Then Throw New Exception("Invalid input: tube spacing (pitch) is smaller than the tube's external diameter.")

                        If CalculationMode = HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor Then
                            Ud = Q * 1000 / (A * DTm)
                        End If
                        If STProperties.Tube_Fluid = 0 Then
                            'cold
                            vt = Wc / (rhoc * nt * Math.PI * di ^ 2 / 4)
                            Ret = rhoc * vt * di / muc
                            Prt = muc * CPC / kc * 1000
                        Else
                            'hot
                            vt = Wh / (rhoh * nt * Math.PI * di ^ 2 / 4)
                            Ret = rhoh * vt * di / muh
                            Prt = muh * CPH / kh * 1000
                        End If

                        IObj?.Paragraphs.Add("<mi>Re_{tube}</mi> = " & Ret)
                        IObj?.Paragraphs.Add("<mi>Pr_{tube}</mi> = " & Prt)

                        'calcular DeltaP

                        Dim dpt, dps As Double
                        'tube
                        dpt = 0.0#
                        Dim fric As Double = 0
                        Dim epsilon As Double = STProperties.Tube_Roughness / 1000
                        If Ret > 3250 Then
                            Dim a1 = Math.Log(((epsilon / di) ^ 1.1096) / 2.8257 + (7.149 / Ret) ^ 0.8961) / Math.Log(10.0#)
                            Dim b1 = -2 * Math.Log((epsilon / di) / 3.7065 - 5.0452 * a1 / Ret) / Math.Log(10.0#)
                            fric = (1 / b1) ^ 2
                        Else
                            fric = 64 / Ret
                        End If
                        fric *= STProperties.Tube_Scaling_FricCorrFactor
                        If STProperties.Tube_Fluid = 0 Then
                            'cold
                            dpt = fric * L * STProperties.Tube_PassesPerShell / di * vt ^ 2 / 2 * rhoc
                        Else
                            'hot
                            dpt = fric * L * STProperties.Tube_PassesPerShell / di * vt ^ 2 / 2 * rhoh
                        End If

                        IObj?.Paragraphs.Add("<mi>\Delta P_{tube}</mi> = " & dpt & " Pa")

                        'tube heat transfer coeff
                        If STProperties.Tube_Fluid = 0 Then
                            'cold
                            hi = kc / di * (fric / 8) * Ret * Prt / (1.07 + 12.7 * (fric / 8) ^ 0.5 * (Prt ^ (2 / 3) - 1))
                        Else
                            'hot
                            hi = kh / di * (fric / 8) * Ret * Prt / (1.07 + 12.7 * (fric / 8) ^ 0.5 * (Prt ^ (2 / 3) - 1))
                        End If

                        IObj?.Paragraphs.Add("<mi>h_{int,tube}</mi> = " & hi & " W/[m2.K]")

                        'shell internal diameter
                        Dim Dsi, Dsf, nsc, HDi, Nb As Double
                        Select Case STProperties.Tube_Layout
                            Case 0, 1
                                nsc = 1.1 * n ^ 0.5
                            Case 2, 3
                                nsc = 1.19 * n ^ 0.5
                        End Select
                        Dsf = (nsc - 1) * pitch + de
                        Dsi = STProperties.Shell_Di / 1000 'Dsf / 1.075

                        'Dsf = Dsi / 1.075 * Dsi
                        HDi = STProperties.Shell_BaffleCut / 100
                        Nb = L / (STProperties.Shell_BaffleSpacing / 1000) + 1 'review (l1, l2)

                        'shell pressure drop
                        Dim Gsf, Np, Fp, Ss, Ssf, fs, Cb, Ca, Res, Prs, jh, aa, bb, cc, xx, yy, Nh, Y As Double
                        xx = Dsi / (STProperties.Shell_BaffleSpacing / 1000)
                        yy = pitch / de
                        Select Case STProperties.Tube_Layout
                            Case 0, 1
                                aa = 0.9078565328950694
                                bb = 0.66331106126564476
                                cc = -4.4329764639656482
                                Nh = aa * xx ^ bb * yy ^ cc
                                aa = 5.3718559074820611
                                bb = -0.33416765138071414
                                cc = 0.7267144209289168
                                Y = aa * xx ^ bb * yy ^ cc
                                aa = 0.53807650470841084
                                bb = 0.3761125784751041
                                cc = -3.8741224386187474
                                Np = aa * xx ^ bb * yy ^ cc
                            Case 2
                                aa = 0.84134824361715088
                                bb = 0.61374520485097339
                                cc = -4.2696318466170409
                                Nh = aa * xx ^ bb * yy ^ cc
                                aa = 4.9901814007765743
                                bb = -0.32437442510328618
                                cc = 1.084850423269188
                                Y = aa * xx ^ bb * yy ^ cc
                                aa = 0.5502379008813062
                                bb = 0.36559560225434834
                                cc = -3.99041305625483
                                Np = aa * xx ^ bb * yy ^ cc
                            Case 3
                                aa = 0.66738654406767639
                                bb = 0.680260033886211
                                cc = -4.522291113086232
                                Nh = aa * xx ^ bb * yy ^ cc
                                aa = 4.5749169651729105
                                bb = -0.32201759442337358
                                cc = 1.17295183743691
                                Y = aa * xx ^ bb * yy ^ cc
                                aa = 0.36869631130961067
                                bb = 0.38397859475813922
                                cc = -3.6273465996780421
                                Np = aa * xx ^ bb * yy ^ cc
                        End Select
                        Fp = 1 / (0.8 + Np * (Dsi / pitch) ^ 0.5)
                        Select Case STProperties.Tube_Layout
                            Case 0, 1, 2
                                Cb = 0.97
                            Case 3
                                Cb = 1.37
                        End Select
                        Ca = Cb * (pitch - de) / pitch
                        Ss = Ca * STProperties.Shell_BaffleSpacing / 1000 * Dsf
                        Ssf = Ss / Fp
                        'Ssf = Math.PI / 4 * (Dsi ^ 2 - nt * de ^ 2)
                        If STProperties.Shell_Fluid = 0 Then
                            'cold
                            Gsf = Wc / Ssf
                            Res = Gsf * de / muc
                            Prs = muc * CPC / kc * 1000
                        Else
                            'hot
                            Gsf = Wh / Ssf
                            Res = Gsf * de / muh
                            Prs = muh * CPH / kh * 1000
                        End If

                        IObj?.Paragraphs.Add("<mi>Re_{shell}</mi> = " & Res)
                        IObj?.Paragraphs.Add("<mi>Pr_{shell}</mi> = " & Prs)

                        Select Case STProperties.Tube_Layout
                            Case 0, 1
                                If Res < 100 Then
                                    jh = 0.497 * Res ^ 0.54
                                Else
                                    jh = 0.378 * Res ^ 0.59
                                End If
                                If pitch / de <= 1.2 Then
                                    If Res < 100 Then
                                        fs = 276.46 * Res ^ -0.979
                                    ElseIf Res < 1000 Then
                                        fs = 30.26 * Res ^ -0.523
                                    Else
                                        fs = 2.93 * Res ^ -0.186
                                    End If
                                ElseIf pitch / de <= 1.3 Then
                                    If Res < 100 Then
                                        fs = 208.14 * Res ^ -0.945
                                    ElseIf Res < 1000 Then
                                        fs = 27.6 * Res ^ -0.525
                                    Else
                                        fs = 2.27 * Res ^ -0.163
                                    End If
                                ElseIf pitch / de <= 1.4 Then
                                    If Res < 100 Then
                                        fs = 122.73 * Res ^ -0.865
                                    ElseIf Res < 1000 Then
                                        fs = 17.82 * Res ^ -0.474
                                    Else
                                        fs = 1.86 * Res ^ -0.146
                                    End If
                                ElseIf pitch / de <= 1.5 Then
                                    If Res < 100 Then
                                        fs = 104.33 * Res ^ -0.869
                                    ElseIf Res < 1000 Then
                                        fs = 12.69 * Res ^ -0.434
                                    Else
                                        fs = 1.526 * Res ^ -0.129
                                    End If
                                End If
                            Case 2, 3
                                If Res < 100 Then
                                    If STProperties.Tube_Layout = 2 Then
                                        jh = 0.385 * Res ^ 0.526
                                    Else
                                        jh = 0.496 * Res ^ 0.54
                                    End If
                                Else
                                    If STProperties.Tube_Layout = 2 Then
                                        jh = 0.2487 * Res ^ 0.625
                                    Else
                                        jh = 0.354 * Res ^ 0.61
                                    End If
                                End If
                                If pitch / de <= 1.2 Then
                                    If Res < 100 Then
                                        fs = 230 * Res ^ -1
                                    ElseIf Res < 1000 Then
                                        fs = 16.23 * Res ^ -0.43
                                    Else
                                        fs = 2.67 * Res ^ -0.173
                                    End If
                                ElseIf pitch / de <= 1.3 Then
                                    If Res < 100 Then
                                        fs = 142.22 * Res ^ -0.949
                                    ElseIf Res < 1000 Then
                                        fs = 11.93 * Res ^ -0.43
                                    Else
                                        fs = 1.77 * Res ^ -0.144
                                    End If
                                ElseIf pitch / de <= 1.4 Then
                                    If Res < 100 Then
                                        fs = 110.77 * Res ^ -0.965
                                    ElseIf Res < 1000 Then
                                        fs = 7.524 * Res ^ -0.4
                                    Else
                                        fs = 1.01 * Res ^ -0.104
                                    End If
                                ElseIf pitch / de <= 1.5 Then
                                    If Res < 100 Then
                                        fs = 58.18 * Res ^ -0.862
                                    ElseIf Res < 1000 Then
                                        fs = 6.76 * Res ^ -0.411
                                    Else
                                        fs = 0.718 * Res ^ -0.008
                                    End If
                                End If
                        End Select

                        'Cx
                        Dim Cx As Double = 0
                        Select Case STProperties.Tube_Layout
                            Case 0, 1
                                Cx = 1.154
                            Case 2
                                Cx = 1.0#
                            Case 3
                                Cx = 1.414
                        End Select
                        Dim Gsh, Ssh, Fh, Rsh, dis As Double
                        If STProperties.Shell_Fluid = 0 Then
                            dps = 4 * fs * Gsf ^ 2 / (2 * rhoc) * Cx * (1 - HDi) * Dsi / pitch * Nb * (1 + Y * pitch / Dsi)
                        Else
                            dps = 4 * fs * Gsf ^ 2 / (2 * rhoh) * Cx * (1 - HDi) * Dsi / pitch * Nb * (1 + Y * pitch / Dsi)
                        End If
                        dps *= Nc

                        IObj?.Paragraphs.Add("<mi>\Delta P_{shell}</mi> = " & dps & " Pa")

                        'shell htc

                        Dim M As Double = 0.96#
                        dis = STProperties.Shell_Di / 1000
                        Fh = 1 / (1 + Nh * (dis / pitch) ^ 0.5)
                        Ssh = Ss * M / Fh
                        'Ssh = Math.PI / 4 * (Dsi ^ 2 - nt * de ^ 2)
                        If STProperties.Shell_Fluid = 0 Then
                            Gsh = Wc / Ssh
                            Rsh = Gsh * de / muc
                        Else
                            Gsh = Wh / Ssh
                            Rsh = Gsh * de / muh
                        End If
                        Dim Ec, lb, he As Double
                        Select Case STProperties.Tube_Layout
                            Case 0, 1
                                If Rsh < 100 Then
                                    jh = 0.497 * Rsh ^ 0.54
                                Else
                                    jh = 0.378 * Rsh ^ 0.61
                                End If
                            Case 2, 3
                                If Rsh < 100 Then
                                    jh = 0.385 * Rsh ^ 0.526
                                Else
                                    jh = 0.2487 * Rsh ^ 0.625
                                End If
                        End Select
                        If STProperties.Shell_Fluid = 0 Then
                            he = jh * kc * Prs ^ 0.34 / de
                        Else
                            he = jh * kh * Prs ^ 0.34 / de
                        End If
                        lb = STProperties.Shell_BaffleSpacing / 1000 * (Nb - 1)
                        Ec = lb + (L - lb) * (2 * STProperties.Shell_BaffleSpacing / 1000 / (L - lb)) ^ 0.6 / L
                        If Double.IsNaN(Ec) Then Ec = 1
                        he *= Ec

                        IObj?.Paragraphs.Add("<mi>h_{ext,shell}</mi> = " & he & " W/[m2.K]")

                        'global HTC (U)
                        Dim kt As Double = STProperties.Tube_ThermalConductivity
                        Dim f1, f2, f3, f4, f5 As Double
                        f1 = de / (hi * di)
                        f2 = rt * de / di
                        f3 = de / (2 * kt) * Math.Log(de / di)
                        f4 = rs
                        f5 = 1 / he
                        If CalculationMode = HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor Then
                            Ur = f1 + f3 + f5
                            Ur = 1 / Ur
                            Rf = 1 / Ud - 1 / Ur
                            STProperties.OverallFoulingFactor = Rf
                            U_ant = U
                            U = 1 / Ur + Rf
                            U = 1 / U
                        Else
                            U_ant = U
                            U = f1 + f2 + f3 + f4 + f5
                            STProperties.OverallFoulingFactor = f2 + f4
                            U = 1 / U
                            Q = U * A * F * LMTD / 1000
                            If Q > MaxHeatExchange Then
                                Q = MaxHeatExchange
                                F = Q * 1000 / (U * A * LMTD)
                            End If
                            If STProperties.Shell_Fluid = 0 Then
                                'cold
                                DeltaHc = (Q - HeatLoss) / Wc
                                DeltaHh = -Q / Wh
                            Else
                                'hot
                                DeltaHc = Q / Wc
                                DeltaHh = -(Q + HeatLoss) / Wh
                            End If
                            Hc2 = Hc1 + DeltaHc
                            Hh2 = Hh1 + DeltaHh
                            StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                            IObj?.SetCurrent()
                            tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, Tc2)
                            Tc2_ant = Tc2
                            Tc2 = tmp.CalculatedTemperature
                            Tc2 = 0.1 * Tc2 + 0.9 * Tc2_ant
                            StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                            IObj?.SetCurrent()
                            tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, Th2)
                            Th2_ant = Th2
                            Th2 = tmp.CalculatedTemperature
                            Th2 = 0.1 * Th2 + 0.9 * Th2_ant
                        End If

                        IObj?.Paragraphs.Add("<mi>Q</mi> = " & Q & " kW")
                        IObj?.Paragraphs.Add("<mi>U</mi> = " & U & " W/[m2.K]")

                        IObj?.Paragraphs.Add("<mi>T_{c,out}</mi> = " & Tc2 & " K")
                        IObj?.Paragraphs.Add("<mi>T_{h,out}</mi> = " & Th2 & " K")

                        STProperties.Ft = f1 'tube side
                        STProperties.Fc = f3 'heat conductivity pipe
                        STProperties.Fs = f5 'shell side
                        STProperties.Ff = STProperties.OverallFoulingFactor
                        STProperties.ReS = Res 'Reynolds number shell side
                        STProperties.ReT = Ret 'Reynolds number tube side

                        If STProperties.Shell_Fluid = 0 Then
                            Pc2 = Pc1 - dps
                            Ph2 = Ph1 - dpt
                        Else
                            Pc2 = Pc1 - dpt
                            Ph2 = Ph1 - dps
                        End If
                        Me.LMTD_F = F
                        If CalculationMode = HeatExchangerCalcMode.ShellandTube_Rating Then
                            fx = Math.Abs((Th2 - Th2_ant) ^ 2 + (Tc2 - Tc2_ant) ^ 2)
                            IObj?.Paragraphs.Add("Temperature error = " & fx)
                        Else
                            fx = Math.Abs((U - U_ant)) ^ 2
                            IObj?.Paragraphs.Add("Overall HTC error = " & fx)
                        End If

                        FlowSheet.CheckStatus()
                        icnt += 1
                        If icnt > 100 Then
                            Throw New Exception("Calculation did not converge in 100 iteratons.")
                        End If
                    Loop Until fx < 0.001

                    StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                    IObj?.SetCurrent()
                    Dim tmp2 = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, 0.0)
                    Tc2 = tmp2.CalculatedTemperature
                    StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                    IObj?.SetCurrent()
                    tmp2 = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Ph2, Hh2, 0.0)
                    Th2 = tmp2.CalculatedTemperature

            End Select

            CheckSpec(Tc2, True, "cold stream outlet temperature")
            CheckSpec(Th2, True, "hot stream outlet temperature")
            CheckSpec(Ph2, True, "hot stream outlet pressure")
            CheckSpec(Pc2, True, "cold stream outlet pressure")

            If CalcMode <> HeatExchangerCalcMode.PinchPoint And CalculateHeatExchangeProfile Then

                Dim dhc, dhh As Double

                Dim tcprof, thprof, qprof As New List(Of Double)

                tcprof.Clear()
                thprof.Clear()
                qprof.Clear()

                For j = 0 To 10

                    Dim dqx = CDbl(j) / 10.0 * MaxHeatExchange

                    dhc = dqx / Wc
                    dhh = dqx / Wh

                    'calculate profiles

                    tmpstr = StInCold.Clone
                    tmpstr.PropertyPackage = StInCold.PropertyPackage
                    tmpstr.SetFlowsheet(StInCold.FlowSheet)

                    tmpstr.Phases(0).Properties.enthalpy = Hc1 + dhc
                    tmpstr.Phases(0).Properties.pressure = Pc1 - Convert.ToDouble(j) / 10.0 * ColdSidePressureDrop
                    tmpstr.SpecType = StreamSpec.Pressure_and_Enthalpy
                    IObj?.SetCurrent()
                    tmpstr.Calculate(True, True)

                    qprof.Add(dqx)
                    tcprof.Add(tmpstr.Phases(0).Properties.temperature.GetValueOrDefault)

                    tmpstr = StInHot.Clone
                    tmpstr.PropertyPackage = StInHot.PropertyPackage
                    tmpstr.SetFlowsheet(StInHot.FlowSheet)

                    tmpstr.Phases(0).Properties.enthalpy = Hh1 - dhh
                    tmpstr.Phases(0).Properties.pressure = Ph1 - Convert.ToDouble(j) / 10.0 * HotSidePressureDrop
                    tmpstr.SpecType = StreamSpec.Pressure_and_Enthalpy
                    IObj?.SetCurrent()
                    tmpstr.Calculate(True, True)

                    thprof.Add(tmpstr.Phases(0).Properties.temperature.GetValueOrDefault)

                Next

                Me.HeatProfile = qprof.ToArray
                Me.TemperatureProfileCold = tcprof.ToArray
                Me.TemperatureProfileHot = thprof.ToArray

            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add("<mi>T_{c,out}</mi> = " & Tc2 & " K")
            IObj?.Paragraphs.Add("<mi>T_{h,out}</mi> = " & Th2 & " K")
            IObj?.Paragraphs.Add("<mi>P_{c,out}</mi> = " & Pc2 & " Pa")
            IObj?.Paragraphs.Add("<mi>P_{h,out}</mi> = " & Ph2 & " Pa")

            IObj?.Paragraphs.Add("<mi>Q</mi> = " & Q & " kW")

            IObj?.Paragraphs.Add("<mi>U</mi> = " & U & " W/[m2.K]")

            IObj?.Paragraphs.Add("<mi>\Delta T_{ml}</mi> = " & LMTD & " K")

            If CalcMode <> HeatExchangerCalcMode.ThermalEfficiency Then ThermalEfficiency = (Q - HeatLoss) / MaxHeatExchange * 100

            If HeatLoss > Math.Abs(Q.GetValueOrDefault) Then Throw New Exception("Invalid Heat Loss.")

            IObj?.Paragraphs.Add("<mi>Q/Q_{max}</mi> = " & ThermalEfficiency & " %")

            If Not DebugMode Then

                Me.ColdSideOutletTemperature = Tc2
                Me.HotSideOutletTemperature = Th2
                Me.ColdSidePressureDrop = Pc1 - Pc2
                Me.HotSidePressureDrop = Ph1 - Ph2
                Me.OverallCoefficient = U
                Me.Area = A

                'Define new calculated properties.
                StOutHot.Phases(0).Properties.temperature = Th2
                StOutCold.Phases(0).Properties.temperature = Tc2
                StOutHot.Phases(0).Properties.pressure = Ph2
                StOutCold.Phases(0).Properties.pressure = Pc2
                StOutHot.Phases(0).Properties.enthalpy = Hh2
                StOutCold.Phases(0).Properties.enthalpy = Hc2
                StOutHot.SetFlashSpec("PH")
                StOutCold.SetFlashSpec("PH")

                StOutCold.AtEquilibrium = False
                StOutHot.AtEquilibrium = False

                If CalculationMode <> HeatExchangerCalcMode.OutletVaporFraction1 And CalculationMode <> HeatExchangerCalcMode.OutletVaporFraction2 Then
                    If Th2 < Tc1 Or Tc2 > Th1 Then
                        FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": Temperature Cross", IFlowsheet.MessageType.Warning)
                    End If
                End If

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If

            IObj?.Close()

        End Sub

        Public Overrides Sub DeCalculate()

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de materia conectada a jusante
                DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name), MaterialStream).Clear()

            End If

            If Me.GraphicObject.OutputConnectors(1).IsAttached Then

                'Zerar valores da corrente de materia conectada a jusante
                DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name), MaterialStream).Clear()

            End If

        End Sub

        Public Overrides Function GetDefaultProperties() As String()
            Return New String() {"PROP_HX_0", "PROP_HX_1", "PROP_HX_2", "PROP_HX_3", "PROP_HX_4", "PROP_HX_25", "PROP_HX_26", "PROP_HX_27", "PROP_HX_28", "PROP_HX_32", "PROP_HX_33"}
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else


                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As Double = 0
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx

                    Case 0
                        'PROP_HX_0	Global Heat Transfer Coefficient (U)
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heat_transf_coeff, Me.OverallCoefficient.GetValueOrDefault)
                    Case 1
                        'PROP_HX_1	Heat Exchange Area (A)
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.Area.GetValueOrDefault)
                    Case 2
                        'PROP_HX_2	Heat Load
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.Q.GetValueOrDefault)
                    Case 3
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.TempColdOut)
                    Case 4
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.TempHotOut)
                    Case 5
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.STProperties.Shell_Di / 1000)
                    Case 6
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Shell_Fouling)
                    Case 7
                        value = Me.STProperties.Shell_BaffleCut
                    Case 8
                        value = Me.STProperties.Shell_NumberOfShellsInSeries
                    Case 9
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.thickness, Me.STProperties.Shell_BaffleSpacing / 1000)
                    Case 10
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.STProperties.Tube_Di / 1000)
                    Case 11
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.STProperties.Tube_De / 1000)
                    Case 12
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.STProperties.Tube_Length)
                    Case 13
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Tube_Fouling)
                    Case 14
                        value = Me.STProperties.Tube_PassesPerShell
                    Case 15
                        value = Me.STProperties.Tube_NumberPerShell
                    Case 16
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.thickness, Me.STProperties.Tube_Pitch / 1000)
                    Case 17
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.OverallFoulingFactor)
                    Case 18
                        value = Me.LMTD_F
                    Case 19
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.LMTD)
                    Case 20
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Ft)
                    Case 21
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Fc)
                    Case 22
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Fs)
                    Case 23
                        value = SystemsOfUnits.Converter.ConvertFromSI("", Me.STProperties.ReS)
                    Case 24
                        value = SystemsOfUnits.Converter.ConvertFromSI("", Me.STProperties.ReT)
                    Case 25
                        value = ThermalEfficiency
                    Case 26
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, MaxHeatExchange)
                    Case 27
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, MITA)
                    Case 28
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, HeatLoss)
                    Case 29
                        value = CorrectionFactorLMTD
                    Case 30
                        value = OutletVaporFraction1
                    Case 21
                        value = OutletVaporFraction2
                    Case 32
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, ColdSidePressureDrop)
                    Case 33
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, HotSidePressureDrop)
                End Select

                Return value
            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            Select Case proptype
                Case PropertyType.RO
                    For i = 2 To 4
                        proplist.Add("PROP_HX_" + CStr(i))
                    Next
                    proplist.Add("PROP_HX_25")
                    proplist.Add("PROP_HX_26")
                    For i = 17 To 24
                        proplist.Add("PROP_HX_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 33
                        proplist.Add("PROP_HX_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 16
                        proplist.Add("PROP_HX_" + CStr(i))
                    Next
                    proplist.Add("PROP_HX_27")
                    proplist.Add("PROP_HX_28")
                    proplist.Add("PROP_HX_29")
                    proplist.Add("PROP_HX_30")
                    proplist.Add("PROP_HX_31")
                    proplist.Add("PROP_HX_32")
                    proplist.Add("PROP_HX_33")
                Case PropertyType.ALL
                    For i = 0 To 33
                        proplist.Add("PROP_HX_" + CStr(i))
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
                    'PROP_HX_0	Global Heat Transfer Coefficient (U)
                    Me.OverallCoefficient = SystemsOfUnits.Converter.ConvertToSI(su.heat_transf_coeff, propval)
                Case 1
                    'PROP_HX_1	Heat Exchange Area (A)
                    Me.Area = SystemsOfUnits.Converter.ConvertToSI(su.area, propval)
                Case 2
                    'PROP_HX_1	Heat Load (Q)
                    Me.Q = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
                Case 3
                    'PROP_HX_3	Cold Fluid Outlet Temperature
                    Me.TempColdOut = SystemsOfUnits.Converter.ConvertToSI(su.temperature, propval)
                Case 4
                    'PROP_HX_4	Hot Fluid Outlet Temperature
                    Me.TempHotOut = SystemsOfUnits.Converter.ConvertToSI(su.temperature, propval)
                Case 5
                    Me.STProperties.Shell_Di = SystemsOfUnits.Converter.ConvertToSI(su.diameter, propval) * 1000
                Case 6
                    Me.STProperties.Shell_Fouling = SystemsOfUnits.Converter.ConvertToSI(su.foulingfactor, propval)
                Case 7
                    Me.STProperties.Shell_BaffleCut = propval
                Case 8
                    Me.STProperties.Shell_NumberOfShellsInSeries = propval
                Case 9
                    Me.STProperties.Shell_BaffleSpacing = SystemsOfUnits.Converter.ConvertToSI(su.thickness, propval) * 1000
                Case 10
                    Me.STProperties.Tube_Di = SystemsOfUnits.Converter.ConvertToSI(su.diameter, propval) * 1000
                Case 11
                    Me.STProperties.Tube_De = SystemsOfUnits.Converter.ConvertToSI(su.diameter, propval) * 1000
                Case 12
                    Me.STProperties.Tube_Length = SystemsOfUnits.Converter.ConvertToSI(su.distance, propval)
                Case 13
                    Me.STProperties.Tube_Fouling = SystemsOfUnits.Converter.ConvertToSI(su.foulingfactor, propval)
                Case 14
                    Me.STProperties.Tube_PassesPerShell = propval
                Case 15
                    Me.STProperties.Tube_NumberPerShell = propval
                Case 16
                    Me.STProperties.Tube_Pitch = SystemsOfUnits.Converter.ConvertToSI(su.thickness, propval) * 1000
                Case 27
                    Me.MITA = SystemsOfUnits.Converter.ConvertToSI(su.deltaT, propval)
                Case 28
                    Me.HeatLoss = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
                Case 29
                    CorrectionFactorLMTD = propval
                Case 30
                    OutletVaporFraction1 = propval
                Case 31
                    OutletVaporFraction2 = propval
                Case 32
                    ColdSidePressureDrop = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 33
                    HotSidePressureDrop = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
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
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx

                    Case 0
                        'PROP_HX_0	Global Heat Transfer Coefficient (U)
                        value = su.heat_transf_coeff
                    Case 1
                        'PROP_HX_1	Heat Exchange Area (A)
                        value = su.area
                    Case 2, 28
                        'PROP_HX_2	Heat Load
                        value = su.heatflow
                    Case 3
                        'PROP_HX_3	
                        value = su.temperature
                    Case 4
                        'PROP_HX_4
                        value = su.temperature
                    Case 5
                        value = su.diameter
                    Case 6
                        value = su.foulingfactor
                    Case 7
                        value = "%"
                    Case 8
                        value = ""
                    Case 9
                        value = su.thickness
                    Case 10
                        value = su.diameter
                    Case 11
                        value = su.diameter
                    Case 12
                        value = su.distance
                    Case 13
                        value = su.foulingfactor
                    Case 14
                        value = ""
                    Case 15
                        value = ""
                    Case 16
                        value = su.thickness
                    Case 17
                        value = su.foulingfactor
                    Case 18
                        value = ""
                    Case 19, 27
                        value = su.deltaT
                    Case 20, 21, 22
                        value = su.foulingfactor
                    Case 23, 24
                        value = ""
                    Case 25
                        value = "%"
                    Case 26
                        value = su.heatflow
                    Case 32, 33
                        value = su.deltaP
                End Select

                Return value
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_HeatExchanger With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_HeatExchanger With {.SimObject = Me}
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
            Return My.Resources.heat_exchanger
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("HEXCH_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("HEXCH_Name")
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

            Dim istr, istr2 As MaterialStream
            istr = Me.GetInletMaterialStream(0)
            istr2 = Me.GetInletMaterialStream(1)

            str.AppendLine("Heat Exchanger: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Inlet conditions (stream 1)")
            str.AppendLine()
            istr.PropertyPackage.CurrentMaterialStream = istr
            str.AppendLine("    Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, istr.Phases(0).Properties.temperature.GetValueOrDefault).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, istr.Phases(0).Properties.pressure.GetValueOrDefault).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Mass flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.massflow, istr.Phases(0).Properties.massflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.massflow)
            str.AppendLine("    Volumetric flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, istr.Phases(0).Properties.volumetric_flow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.volumetricFlow)
            str.AppendLine("    Vapor fraction: " & istr.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine("    Compounds: " & istr.PropertyPackage.RET_VNAMES.ToArrayString)
            str.AppendLine("    Molar composition: " & istr.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString(ci))
            str.AppendLine()
            str.AppendLine("Inlet conditions (stream 2)")
            str.AppendLine()
            istr2.PropertyPackage.CurrentMaterialStream = istr2
            str.AppendLine("    Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, istr2.Phases(0).Properties.temperature.GetValueOrDefault).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, istr2.Phases(0).Properties.pressure.GetValueOrDefault).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Mass flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.massflow, istr2.Phases(0).Properties.massflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.massflow)
            str.AppendLine("    Volumetric flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, istr2.Phases(0).Properties.volumetric_flow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.volumetricFlow)
            str.AppendLine("    Vapor fraction: " & istr2.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine("    Compounds: " & istr2.PropertyPackage.RET_VNAMES.ToArrayString)
            str.AppendLine("    Molar composition: " & istr2.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString(ci))
            str.AppendLine()
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Exchanger type: " & Me.FlowDir.ToString)
            Select Case Me.CalculationMode
                Case HeatExchangerCalcMode.CalcTempColdOut
                    str.AppendLine("    Hot fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.HotSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Exchange area: " & SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.Area).ToString(numberformat, ci) & " " & su.area)
                Case HeatExchangerCalcMode.CalcTempHotOut
                    str.AppendLine("    Cold fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.ColdSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Exchange area: " & SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.Area).ToString(numberformat, ci) & " " & su.area)
                Case HeatExchangerCalcMode.CalcBothTemp
                    str.AppendLine("    Heat exchanged: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.Q).ToString(numberformat, ci) & " " & su.heatflow)
                    str.AppendLine("    Exchange area: " & SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.Area).ToString(numberformat, ci) & " " & su.area)
                Case HeatExchangerCalcMode.CalcArea
                    str.AppendLine("    Overall heat transfer coefficient: " & SystemsOfUnits.Converter.ConvertFromSI(su.heat_transf_coeff, Me.OverallCoefficient).ToString(numberformat, ci) & " " & su.heat_transf_coeff)
                    If Me.DefinedTemperature = SpecifiedTemperature.Cold_Fluid Then
                        str.AppendLine("    Cold fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.ColdSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    Else
                        str.AppendLine("    Hot fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.HotSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    End If
                Case HeatExchangerCalcMode.OutletVaporFraction1
                    str.AppendLine("    Outlet Vapor Fraction 1: " & OutletVaporFraction1.ToString(numberformat))
                Case HeatExchangerCalcMode.OutletVaporFraction2
                    str.AppendLine("    Outlet Vapor Fraction 2: " & OutletVaporFraction2.ToString(numberformat))
            End Select
            str.AppendLine("    Hot fluid pressure drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.HotSidePressureDrop).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine("    Cold fluid pressure drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.ColdSidePressureDrop).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine("    Heat loss: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.HeatLoss).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case Me.CalculationMode
                Case HeatExchangerCalcMode.CalcTempColdOut
                    str.AppendLine("    Cold fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.ColdSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Overall heat transfer coefficient: " & SystemsOfUnits.Converter.ConvertFromSI(su.heat_transf_coeff, Me.OverallCoefficient).ToString(numberformat, ci) & " " & su.heat_transf_coeff)
                    str.AppendLine("    Heat exchanged: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.Q).ToString(numberformat, ci) & " " & su.heatflow)
                Case HeatExchangerCalcMode.CalcTempHotOut
                    str.AppendLine("    Hot fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.HotSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Overall heat transfer coefficient: " & SystemsOfUnits.Converter.ConvertFromSI(su.heat_transf_coeff, Me.OverallCoefficient).ToString(numberformat, ci) & " " & su.heat_transf_coeff)
                    str.AppendLine("    Heat exchanged: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.Q).ToString(numberformat, ci) & " " & su.heatflow)
                Case HeatExchangerCalcMode.CalcBothTemp
                    str.AppendLine("    Cold fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.ColdSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Hot fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.HotSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Overall heat transfer coefficient: " & SystemsOfUnits.Converter.ConvertFromSI(su.heat_transf_coeff, Me.OverallCoefficient).ToString(numberformat, ci) & " " & su.heat_transf_coeff)
                Case HeatExchangerCalcMode.CalcArea
                    str.AppendLine("    Heat exchanged: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.Q).ToString(numberformat, ci) & " " & su.heatflow)
                    str.AppendLine("    Exchange area: " & SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.Area).ToString(numberformat, ci) & " " & su.area)
                Case HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor, HeatExchangerCalcMode.ShellandTube_Rating
                    str.AppendLine("    Re Shell: " & STProperties.ReS.ToString(numberformat, ci))
                    str.AppendLine("    Re Tube: " & STProperties.ReT.ToString(numberformat, ci))
                    str.AppendLine("    F Shell: " & SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Fs).ToString(numberformat, ci) & " " & su.foulingfactor)
                    str.AppendLine("    F Tube: " & SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Ft).ToString(numberformat, ci) & " " & su.foulingfactor)
                    str.AppendLine("    F Pipe: " & SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Fc).ToString(numberformat, ci) & " " & su.foulingfactor)
                    str.AppendLine("    F Fouling: " & SystemsOfUnits.Converter.ConvertFromSI(su.foulingfactor, Me.STProperties.Ff).ToString(numberformat, ci) & " " & su.foulingfactor)
                    str.AppendLine("    Heat exchanged: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.Q).ToString(numberformat, ci) & " " & su.heatflow)
                    str.AppendLine("    Exchange area: " & SystemsOfUnits.Converter.ConvertFromSI(su.area, Me.Area).ToString(numberformat, ci) & " " & su.area)
                    str.AppendLine("    Cold fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.ColdSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Hot fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.HotSideOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Overall heat transfer coefficient: " & SystemsOfUnits.Converter.ConvertFromSI(su.heat_transf_coeff, Me.OverallCoefficient).ToString(numberformat, ci) & " " & su.heat_transf_coeff)
            End Select
            str.AppendLine("    Log mean temperature difference (LMTD): " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.LMTD).ToString(numberformat, ci) & " " & su.deltaT)
            str.AppendLine("    Maximum Heat Exchange: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.MaxHeatExchange).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine("    Thermal Efficiency (%): " & ThermalEfficiency.ToString(numberformat, ci))

            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Heat Exchanger '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Exchanger Mode",
                    FlowDir.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Calculation Mode",
                    CalcMode.ToString}))

            Select Case Me.CalculationMode
                Case HeatExchangerCalcMode.CalcTempColdOut
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Hot Fluid Outlet Temperature",
                            HotSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                            su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Exchange Area",
                            Area.GetValueOrDefault.ConvertFromSI(su.area).ToString(nf),
                            su.area}))
                Case HeatExchangerCalcMode.CalcTempHotOut
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Cold Fluid Outlet Temperature",
                           ColdSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                           su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Exchange Area",
                            Area.GetValueOrDefault.ConvertFromSI(su.area).ToString(nf),
                            su.area}))
                Case HeatExchangerCalcMode.CalcBothTemp
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Heat Exchanged",
                           Q.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Exchange Area",
                            Area.GetValueOrDefault.ConvertFromSI(su.area).ToString(nf),
                            su.area}))
                Case HeatExchangerCalcMode.CalcArea
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Overall Heat Transfer Coefficient",
                            OverallCoefficient.GetValueOrDefault.ConvertFromSI(su.heat_transf_coeff).ToString(nf),
                            su.heat_transf_coeff}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Hot Fluid Outlet Temperature",
                            HotSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                            su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Cold Fluid Outlet Temperature",
                           ColdSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                           su.temperature}))
                Case HeatExchangerCalcMode.CalcBothTemp_UA
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Heat Exchanged",
                           Q.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Exchange Area",
                            Area.GetValueOrDefault.ConvertFromSI(su.area).ToString(nf),
                            su.area}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Overall Heat Transfer Coefficient",
                           OverallCoefficient.GetValueOrDefault.ConvertFromSI(su.heat_transf_coeff).ToString(nf),
                           su.heat_transf_coeff}))
                Case HeatExchangerCalcMode.OutletVaporFraction1
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Outlet Vapor Fraction 1",
                           OutletVaporFraction1.ToString(nf),
                           ""}))
                Case HeatExchangerCalcMode.OutletVaporFraction2
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Outlet Vapor Fraction 2",
                           OutletVaporFraction2.ToString(nf),
                           ""}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Heat Loss",
                           HeatLoss.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            Select Case Me.CalculationMode
                Case HeatExchangerCalcMode.CalcTempColdOut
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Hot Fluid Pressure Drop",
                           HotSidePressureDrop.ConvertFromSI(su.deltaP).ToString(nf),
                           su.deltaP}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Cold Fluid Pressure Drop",
                           ColdSidePressureDrop.ConvertFromSI(su.deltaP).ToString(nf),
                           su.deltaP}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Cold Fluid Outlet Temperature",
                           ColdSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                           su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Overall Heat Transfer Coefficient",
                           OverallCoefficient.GetValueOrDefault.ConvertFromSI(su.heat_transf_coeff).ToString(nf),
                           su.heat_transf_coeff}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Heat Exchanged",
                           Q.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))
                Case HeatExchangerCalcMode.CalcTempHotOut
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Hot Fluid Pressure Drop",
                           HotSidePressureDrop.ConvertFromSI(su.deltaP).ToString(nf),
                           su.deltaP}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Cold Fluid Pressure Drop",
                           ColdSidePressureDrop.ConvertFromSI(su.deltaP).ToString(nf),
                           su.deltaP}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Hot Fluid Outlet Temperature",
                            HotSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                            su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Overall Heat Transfer Coefficient",
                           OverallCoefficient.GetValueOrDefault.ConvertFromSI(su.heat_transf_coeff).ToString(nf),
                           su.heat_transf_coeff}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Heat Exchanged",
                           Q.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))
                Case HeatExchangerCalcMode.CalcBothTemp
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Hot Fluid Outlet Temperature",
                           HotSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                           su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Cold Fluid Pressure Drop",
                           ColdSidePressureDrop.ConvertFromSI(su.deltaP).ToString(nf),
                           su.deltaP}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Overall Heat Transfer Coefficient",
                           OverallCoefficient.GetValueOrDefault.ConvertFromSI(su.heat_transf_coeff).ToString(nf),
                           su.heat_transf_coeff}))
                Case HeatExchangerCalcMode.CalcArea
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Heat Exchanged",
                           Q.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Exchange Area",
                            Area.GetValueOrDefault.ConvertFromSI(su.area).ToString(nf),
                            su.area}))
                Case HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor, HeatExchangerCalcMode.ShellandTube_Rating
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Reynolds Number (Shell)",
                           STProperties.ReS.ToString(nf),
                           ""}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Reynolds Number (Tube)",
                           STProperties.ReT.ToString(nf),
                           ""}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"F (Shell)",
                           STProperties.Fs.ToString(nf),
                           ""}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"F (Tube)",
                           STProperties.Ft.ToString(nf),
                           ""}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"F (Pipe)",
                           STProperties.Fc.ToString(nf),
                           ""}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"F (Fouling)",
                           STProperties.Ff.ToString(nf),
                           ""}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Heat Exchanged",
                           Q.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Exchange Area",
                            Area.GetValueOrDefault.ConvertFromSI(su.area).ToString(nf),
                            su.area}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Cold Fluid Pressure Drop",
                           ColdSidePressureDrop.ConvertFromSI(su.deltaP).ToString(nf),
                           su.deltaP}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Hot Fluid Outlet Temperature",
                            HotSideOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                            su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Overall Heat Transfer Coefficient",
                           OverallCoefficient.GetValueOrDefault.ConvertFromSI(su.heat_transf_coeff).ToString(nf),
                           su.heat_transf_coeff}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Log Mean Temperature Difference (LMTD)",
                            LMTD.ConvertFromSI(su.deltaT).ToString(nf),
                            su.deltaT}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Maximum Possible Heat Exchange",
                           MaxHeatExchange.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Thermal Efficiency (%)",
                           ThermalEfficiency.ToString(nf),
                           "%"}))

            Return list

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Calculation Mode") Then
                Return "Select the Heat Exchanger calculation mode."
            ElseIf p.Equals("Flow Direction") Then
                Return "Select the flow direction of the inlet streams."
            ElseIf p.Equals("Defined Temperature (for Calc Area Mode)") Then
                Return "Select which temperature you will define if you chose the 'Area' calculation mode."
            ElseIf p.Equals("Pressure Drop (Hot Fluid)") Then
                Return "Enter the pressure drop of the hot fluid. Required for all calculation modes except Shell and Tube Design/Rating."
            ElseIf p.Equals("Pressure Drop (Cold Fluid)") Then
                Return "Enter the pressure drop of the cold fluid. Required for all calculation modes except Shell and Tube Design/Rating."
            ElseIf p.Equals("Outlet Temperature (Cold Fluid)") Then
                Return "Enter the outlet temperature of the cold fluid, if required by the selected calculation mode."
            ElseIf p.Equals("Outlet Temperature (Hot Fluid)") Then
                Return "Enter the outlet temperature of the hot fluid, if required by the selected calculation mode."
            ElseIf p.Equals("Overall HTC") Then
                Return "Enter the overall Heat Exchange Coefficient, if required by the selected calculation mode."
            ElseIf p.Equals("Heat Exchange Area") Then
                Return "Enter the Heat Exchange Area, if required by the selected calculation mode."
            ElseIf p.Equals("Heat Exchanged") Then
                Return "Enter the Heat Exchanged, if required by the selected calculation mode."
            ElseIf p.Equals("MITA") Then
                Return "Enter the Mimimum Internal Temperature Approach (MITA) (for Pinch Point calculation mode only)."
            ElseIf p.Equals("Ignore LMTD Error") Then
                Return "If checked, continues solving even if the calculated LMTD is invalid."
            ElseIf p.Equals("Heat Loss") Then
                Return "Enter the total Heat Loss on this exchanger."
            Else
                Return p
            End If
        End Function
    End Class

End Namespace

Namespace UnitOperations.Auxiliary.HeatExchanger

    <System.Serializable()> Public Class STHXProperties

        Implements Interfaces.ICustomXMLSerialization

        'number of shells in series, integer
        Public Shell_NumberOfShellsInSeries As Integer = 1
        'number of shell passes, integer
        Public Shell_NumberOfPasses As Integer = 2
        'shell internal diameter in mm
        Public Shell_Di As Double = 500.0
        'shell fouling in K.m2/W
        Public Shell_Fouling As Double = 0.0#
        'baffle type: 0 = single, 1 = double, 2 = triple, 3 = grid
        Public Shell_BaffleType As Integer = 0
        'baffle orientation: 0 = horizontal, 1 = vertical
        Public Shell_BaffleOrientation As Integer = 1
        'baffle cut in % diameter
        Public Shell_BaffleCut As Double = 20
        'baffle spacing in mm
        Public Shell_BaffleSpacing As Double = 250.0
        'fluid in shell: 0 = cold, 1 = hot
        Public Shell_Fluid As Integer = 1
        'tube internal diameter in mm
        Public Tube_Di As Double = 50.0
        'tube external diameter in mm
        Public Tube_De As Double = 60.0
        'tube length in m
        Public Tube_Length As Double = 5.0
        'tube fouling in K.m2/W
        Public Tube_Fouling As Double = 0.0#
        'number of tube passes per shell, integer
        Public Tube_PassesPerShell As Integer = 2
        'number of tubes per shell, integer
        Public Tube_NumberPerShell As Integer = 50
        'tube layout: 0 = triangular, 1 = triangular rotated, 2 = square, 2 = square rotated
        Public Tube_Layout As Integer = 0
        'tube pitch in mm
        Public Tube_Pitch As Double = 70.0
        'fluid in tubes: 0 = cold, 1 = hot
        Public Tube_Fluid As Integer = 0
        'tube material roughness in mm
        Public Tube_Roughness As Double = 0.000045 * 1000
        'shell material roughness in mm
        Public Shell_Roughness As Double = 0.000045 * 1000
        'tube scaling friction factor correction
        Public Tube_Scaling_FricCorrFactor As Double = 1.2#
        'tube thermal conductivity
        Public Tube_ThermalConductivity As Double = 70.0#
        'overall fouling factor, used only in design mode (as a calculation result)
        Public OverallFoulingFactor = 0.0#
        'partial heat exchange resistances (tube, conduction, shell, fouling), only as calculation result
        Public Ft, Fc, Fs, Ff As Double
        'Reynold numbers, only as calculation results
        Public ReT, ReS As Double

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me, True)

        End Function

    End Class


End Namespace

