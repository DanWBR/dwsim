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

Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums
Imports DotNumerics.Optimization.TN
Imports NetOffice.ExcelApi

Namespace UnitOperations

    <System.Serializable()> Public Class Valve

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.PressureChangers

        Public Enum OpeningKvRelationshipType
            Linear = 0
            EqualPercentage = 1
            QuickOpening = 2
            UserDefined = 3
            DataTable = 4
        End Enum

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = True

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_Valve

        Protected m_dp As Double?
        Protected m_dt As Double?
        Protected m_DQ As Double?
        Protected m_Pout As Double? = 101325.0#
        Protected m_cmode As CalculationMode = CalculationMode.DeltaP
        Public Property Hinlet As Double
        Public Property Houtlet As Double

        Public Property Kv As Double = 100.0#

        Public Property ActualKv As Double = 0.0

        Private _opening As Double = 50.0

        Public Property OpeningPct As Double
            Get
                Return _opening
            End Get
            Set(value As Double)
                If FlowSheet IsNot Nothing Then
                    If FlowSheet.DynamicMode AndAlso DelayedOpenings IsNot Nothing Then
                        Dim AD As Double = GetDynamicProperty("Actuator Delay")
                        If AD > 0.0 Then
                            DelayedOpenings.Enqueue(value)
                        Else
                            _opening = value
                        End If
                    Else
                        _opening = value
                    End If
                Else
                    _opening = value
                End If
            End Set
        End Property

        Public Property xT As Double = 0.75

        Public Property FL As Double = 0.9

        Public Property FP As Double = 1.0

        Public Property Fs As Double = 1.0

        Public Property Fi As Double = 0.9

        Public Property N6 As Double = 31.6

        Public Property PercentOpeningVersusPercentKvExpression As String = "1.0*OP"

        Public Property EnableOpeningKvRelationship As Boolean = False

        Public Property CharacteristicParameter As Double = 50

        Public Property DefinedOpeningKvRelationShipType As OpeningKvRelationshipType = OpeningKvRelationshipType.UserDefined

        Public Property OpeningKvRelDataTableX As New List(Of Double)

        Public Property OpeningKvRelDataTableY As New List(Of Double)

        Public Property FlowCoefficient As FlowCoefficientType = FlowCoefficientType.Kv

        Private ActuatorTimeToNext As New DateTime

        Private DelayedOpenings As New Queue(Of Double)

        Public Enum FlowCoefficientType
            Kv = 0
            Cv = 1
        End Enum

        Public Enum CalculationMode
            DeltaP = 0
            OutletPressure = 1
            Kv_Liquid = 2
            Kv_Gas = 3
            Kv_Steam = 4
            Kv_General = 5
        End Enum

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Valve()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Valve)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

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

        Property OutletTemperature As Double

        Public Overrides Sub CreateDynamicProperties()

            AddDynamicProperty("Actuator Delay", "Valve Actuator Delay", 0, UnitOfMeasure.time, 1.0.GetType())

        End Sub

        Public Overrides Sub RunDynamicModel()

            Dim integratorID = FlowSheet.DynamicsManager.ScheduleList(FlowSheet.DynamicsManager.CurrentSchedule).CurrentIntegrator
            Dim integrator = FlowSheet.DynamicsManager.IntegratorList(integratorID)

            If Not integrator.ShouldCalculatePressureFlow Then Exit Sub

            Dim AD As Double = GetDynamicProperty("Actuator Delay")

            If AD > 0.0 Then

                Dim DT0 = (integrator.CurrentTime - New Date).TotalSeconds

                If DT0 = 0.0 Then
                    ActuatorTimeToNext = New Date()
                    DelayedOpenings = New Queue(Of Double)
                Else
                    ActuatorTimeToNext = ActuatorTimeToNext.Add(integrator.IntegrationStep)
                End If

                Dim DT = (ActuatorTimeToNext - New Date).TotalSeconds

                If DT >= AD AndAlso DelayedOpenings.Count > 0 Then
                    ActuatorTimeToNext = New Date()
                    OpeningPct = DelayedOpenings.Dequeue()
                End If

            End If

            Dim ims As MaterialStream = Me.GetInletMaterialStream(0)
            Dim oms As MaterialStream = Me.GetOutletMaterialStream(0)

            Dim Ti, P1, Hi, Wi, ei, ein, P2, H2, rho, volf, rhog20, P2ant, v2, Kvc, Pv, Pc, rhol, rhog, k, Cp_ig As Double
            Dim massfrac_gas, massfrac_liq As Double
            Dim icount As Integer

            Me.PropertyPackage.CurrentMaterialStream = ims

            Ti = ims.Phases(0).Properties.temperature.GetValueOrDefault
            P1 = ims.Phases(0).Properties.pressure.GetValueOrDefault
            Hi = ims.Phases(0).Properties.enthalpy.GetValueOrDefault
            Wi = ims.Phases(0).Properties.massflow.GetValueOrDefault
            ei = Hi * Wi
            ein = ei
            rho = ims.Phases(0).Properties.density.GetValueOrDefault
            volf = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault

            H2 = Hi

            Select Case CalcMode

                Case CalculationMode.OutletPressure, CalculationMode.DeltaP

                    If ims.DynamicsSpec = Dynamics.DynamicsSpecType.Flow And
                        oms.DynamicsSpec = Dynamics.DynamicsSpecType.Flow Then

                        Throw New Exception("Inlet and Outlet Streams cannot be both Flow-spec'd at the same time.")

                    ElseIf ims.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure And
                         oms.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure Then

                        Throw New Exception("Inlet and Outlet Streams cannot be both Pressure-spec'd at the same time.")

                    ElseIf ims.DynamicsSpec = Dynamics.DynamicsSpecType.Flow And
                                oms.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure Then

                        Throw New Exception("Inlet Flow + Outlet Pressure specifications not supported by this calculation mode.")

                    ElseIf ims.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure And
                                oms.DynamicsSpec = Dynamics.DynamicsSpecType.Flow Then

                        If CalcMode = CalculationMode.OutletPressure Then
                            P2 = OutletPressure.GetValueOrDefault()
                        Else
                            P2 = P1 - DeltaP.GetValueOrDefault
                        End If

                    End If

                    DeltaP = P1 - P2
                    OutletPressure = P2

                    With oms
                        .AtEquilibrium = False
                        .Phases(0).Properties.temperature = Ti
                        .Phases(0).Properties.pressure = P2
                        .Phases(0).Properties.enthalpy = H2
                        Dim comp As BaseClasses.Compound
                        Dim i As Integer = 0
                        For Each comp In .Phases(0).Compounds.Values
                            comp.MoleFraction = ims.Phases(0).Compounds(comp.Name).MoleFraction
                            comp.MassFraction = ims.Phases(0).Compounds(comp.Name).MassFraction
                            comp.MassFlow = comp.MassFraction * Wi
                            comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight * 1000
                            i += 1
                        Next
                    End With

                    Wi = oms.GetMassFlow
                    If Double.IsNaN(Wi) Or Double.IsInfinity(Wi) Or Wi < 0.0 Then Wi = 1.0E-20

                    If ims.MaximumAllowableDynamicMassFlowRate.HasValue Then
                        Dim WiMax = ims.MaximumAllowableDynamicMassFlowRate.Value
                        If Wi > WiMax Then
                            ims.SetMassFlow(WiMax)
                            oms.SetMassFlow(WiMax)
                        Else
                            ims.SetMassFlow(Wi)
                            oms.SetMassFlow(Wi)
                        End If
                    Else
                        ims.SetMassFlow(Wi)
                        oms.SetMassFlow(Wi)
                    End If

                    ims.SetMassFlow(Wi)

                Case Else

                    Dim FC As Double 'flow coefficient

                    If FlowCoefficient = FlowCoefficientType.Cv Then
                        'Cv = 1.16 Kv
                        'Kv = Cv / 1.16
                        FC = Kv / 1.16
                    Else
                        FC = Kv
                    End If

                    If EnableOpeningKvRelationship Then
                        Select Case DefinedOpeningKvRelationShipType
                            Case OpeningKvRelationshipType.UserDefined
                                Try
                                    Dim ExpContext As New Ciloci.Flee.ExpressionContext
                                    ExpContext.Imports.AddType(GetType(System.Math))
                                    ExpContext.Variables.Clear()
                                    ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                                    ExpContext.Variables.Add("OP", OpeningPct)
                                    Dim Expr = ExpContext.CompileGeneric(Of Double)(PercentOpeningVersusPercentKvExpression)
                                    Kvc = FC * Expr.Evaluate() / 100
                                Catch ex As Exception
                                    Throw New Exception("Invalid expression for Kv[Cv]/Opening relationship.")
                                End Try
                            Case OpeningKvRelationshipType.QuickOpening
                                Kvc = (OpeningPct / 100.0) ^ 0.5 * FC
                            Case OpeningKvRelationshipType.Linear
                                Kvc = OpeningPct / 100.0 * FC
                            Case OpeningKvRelationshipType.EqualPercentage
                                Kvc = CharacteristicParameter ^ (OpeningPct / 100.0 - 1.0) * FC
                            Case OpeningKvRelationshipType.DataTable
                                Try
                                    Dim factor = MathNet.Numerics.Interpolate.RationalWithoutPoles(OpeningKvRelDataTableX, OpeningKvRelDataTableX).Interpolate(OpeningPct) / 100.0
                                    Kvc = factor * FC
                                Catch ex As Exception
                                    Throw New Exception("Error calculating Kv from tabulated data: " + ex.Message)
                                End Try
                        End Select
                    Else
                        Kvc = FC
                    End If

                    If ims.DynamicsSpec = Dynamics.DynamicsSpecType.Flow And
                        oms.DynamicsSpec = Dynamics.DynamicsSpecType.Flow Then

                        'not supported

                        Throw New Exception("Inlet and Outlet Streams cannot be both Flow-spec'd at the same time.")

                    ElseIf ims.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure And
                         oms.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure Then

                        'valid! calculate flow

                        P2 = oms.GetPressure

                        If CalcMode = CalculationMode.Kv_General Or CalcMode = CalculationMode.Kv_Gas Or CalcMode = CalculationMode.Kv_Liquid Then
                            If ims.Phases(1).Properties.molarfraction > 0.99 Or CalcMode = CalculationMode.Kv_Liquid Then
                                Wi = Kvc * (1000.0 * rho * (P1 - P2) / 100000.0) ^ 0.5 / 3600
                            ElseIf ims.Phases(2).Properties.molarfraction > 0.99 Or CalcMode = CalculationMode.Kv_Gas Then
                                ims.PropertyPackage.CurrentMaterialStream = ims
                                rhog20 = ims.PropertyPackage.AUX_VAPDENS(273.15, 101325)
                                If P2 > P1 / 2 Then
                                    Wi = 519 * Kvc / (Ti / (rhog20 * (P1 - P2) / 100000.0 * P1 / 100000.0)) ^ 0.5 / 3600
                                Else
                                    Wi = 259.5 * Kvc * P1 / 100000.0 / (Ti / rhog20) ^ 0.5 / 3600
                                End If
                            Else
                                ims.PropertyPackage.CurrentMaterialStream = ims
                                rhog = ims.Phases(2).Properties.density.GetValueOrDefault
                                Cp_ig = ims.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Ti) * ims.Phases(2).Properties.molecularWeight.GetValueOrDefault
                                k = Cp_ig / (Cp_ig - 8.314)
                                rhol = ims.Phases(1).Properties.density.GetValueOrDefault
                                Pc = ims.PropertyPackage.AUX_PCM(PropertyPackages.Phase.Liquid)
                                Pv = ims.PropertyPackage.AUX_PVAPM(PropertyPackages.Phase.Liquid, Ti)

                                massfrac_gas = ims.Phases(2).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                                massfrac_liq = ims.Phases(1).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault

                                Wi = WTwoPhase(Kvc, P1 / 100000.0, P2 / 100000.0, rhog, rhol, k, Pv / 100000.0, Pc / 100000.0, massfrac_gas, massfrac_liq)
                            End If
                        ElseIf CalcMode = CalculationMode.Kv_Steam Then
                            If P2 > P1 / 2 Then
                                v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P2)
                                Wi = Kvc * 31.62 / (v2 / ((P1 - P2) / 100000.0)) ^ 0.5 / 3600
                            Else
                                v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P1 / 2)
                                Wi = Kvc * 31.62 / (2 * v2 / (P1 / 100000.0)) ^ 0.5 / 3600
                            End If
                        End If

                        If Double.IsNaN(Wi) Or Double.IsInfinity(Wi) Or Wi < 0.0 Then Wi = 0.0

                        If ims.MaximumAllowableDynamicMassFlowRate.HasValue Then
                            Dim WiMax = ims.MaximumAllowableDynamicMassFlowRate.Value
                            If Wi > WiMax Then
                                ims.SetMassFlow(WiMax)
                                oms.SetMassFlow(WiMax)
                            Else
                                ims.SetMassFlow(Wi)
                                oms.SetMassFlow(Wi)
                            End If
                        Else
                            ims.SetMassFlow(Wi)
                            oms.SetMassFlow(Wi)
                        End If

                    ElseIf ims.DynamicsSpec = Dynamics.DynamicsSpecType.Flow And
                                oms.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure Then

                        'valid! calculate P1

                        If Double.IsNaN(Wi) Or Double.IsInfinity(Wi) Or Wi < 0.0 Then Wi = 0.0

                        oms.SetMassFlow(Wi)

                        P2 = oms.GetPressure()

                        If CalcMode = CalculationMode.Kv_General Or CalcMode = CalculationMode.Kv_Gas Or CalcMode = CalculationMode.Kv_Liquid Then
                            If ims.Phases(1).Properties.molarfraction = 1 Or CalcMode = CalculationMode.Kv_Liquid Then
                                P1 = P2 / 100000.0 + 1 / (1000.0 * rho) * (Wi * 3600 / Kvc) ^ 2
                            ElseIf ims.Phases(2).Properties.molarfraction = 1 Or CalcMode = CalculationMode.Kv_Gas Then
                                ims.PropertyPackage.CurrentMaterialStream = ims
                                rhog20 = ims.PropertyPackage.AUX_VAPDENS(273.15, 101325)
                                P1 = P2 / 100000.0 + Ti / rhog20 / (P2 / 100000) * (519 * Kvc / (Wi * 3600)) ^ -2
                            Else
                                ims.PropertyPackage.CurrentMaterialStream = ims
                                rhog20 = ims.PropertyPackage.AUX_VAPDENS(273.15, 101325)
                                rhol = ims.Phases(1).Properties.density.GetValueOrDefault
                                massfrac_gas = ims.Phases(2).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                                massfrac_liq = ims.Phases(1).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                                P1 = P1TwoPhase(Wi * 3600, Kvc, P2 / 100000.0, Ti, rhog20, rhol, massfrac_gas, massfrac_liq)
                            End If
                        ElseIf CalcMode = CalculationMode.Kv_Steam Then
                            v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P2)
                            P1 = P2 / 100000.0 + v2 * (31.62 * Kvc / (Wi * 3600)) ^ -2
                        End If
                        P1 = P1 * 100000.0
                        ims.SetPressure(P1)

                    ElseIf ims.DynamicsSpec = Dynamics.DynamicsSpecType.Pressure And
                                oms.DynamicsSpec = Dynamics.DynamicsSpecType.Flow Then

                        Wi = oms.GetMassFlow

                        If Double.IsNaN(Wi) Or Double.IsInfinity(Wi) Or Wi < 0.0 Then Wi = 1.0E-20

                        ims.SetMassFlow(Wi)

                        'valid! Calculate P2

                        If CalcMode = CalculationMode.Kv_General Or CalcMode = CalculationMode.Kv_Gas Or CalcMode = CalculationMode.Kv_Liquid Then
                            If ims.Phases(1).Properties.molarfraction = 1 Or CalcMode = CalculationMode.Kv_Liquid Then
                                P2 = P1 / 100000.0 - 1 / (1000.0 * rho) * (Wi * 3600 / Kvc) ^ 2
                                P2 = P2 * 100000.0
                            ElseIf ims.Phases(2).Properties.molarfraction = 1 Or CalcMode = CalculationMode.Kv_Gas Then
                                ims.PropertyPackage.CurrentMaterialStream = ims
                                rhog20 = ims.PropertyPackage.AUX_VAPDENS(273.15, 101325)
                                Dim roots = MathOps.Quadratic.quadForm(-rhog20, rhog20 * P1 / 100000, -Ti * (519 * Kvc / (Wi * 3600)) ^ -2)
                                If roots.Item1 > 0 And roots.Item1 > P1 / 100000 / 2 Then
                                    P2 = roots.Item1 * 100000.0
                                ElseIf roots.Item2 > 0 And roots.Item2 > P1 / 100000 / 2 Then
                                    P2 = roots.Item2 * 100000.0
                                Else
                                    Throw New Exception("Unable to calculate the outlet pressure.")
                                End If
                            Else
                                ims.PropertyPackage.CurrentMaterialStream = ims
                                rhog = ims.Phases(2).Properties.density.GetValueOrDefault
                                Cp_ig = ims.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Ti) * ims.Phases(2).Properties.molecularWeight()
                                k = Cp_ig / (Cp_ig - 8.314)
                                rhol = ims.Phases(1).Properties.density.GetValueOrDefault
                                Pc = ims.PropertyPackage.AUX_PCM(PropertyPackages.Phase.Liquid)
                                Pv = P1 'ims.PropertyPackage.AUX_PVAPM(PropertyPackages.Phase.Liquid, Ti)

                                massfrac_gas = ims.Phases(2).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                                massfrac_liq = ims.Phases(1).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                                P2 = 100000.0 * P2TwoPhase(Wi * 3600, Kvc, P1 / 100000.0, rhog, rhol, k, Pv / 100000.0, Pc / 100000.0, massfrac_gas, massfrac_liq)
                            End If
                        ElseIf CalcMode = CalculationMode.Kv_Steam Then
                            P2 = P1 * 0.7 / 100000.0
                            icount = 0
                            Do
                                v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P2)
                                P2ant = P2
                                P2 = P1 / 100000.0 - v2 * (31.62 * Kvc / (Wi * 3600)) ^ -2
                                icount += 1
                                If icount > 10000 Then Throw New Exception("P2 did not converge in 10000 iterations.")
                            Loop Until Math.Abs(P2 - P2ant) < 0.0001
                            P2 = P2 * 100000.0
                        End If
                    End If

                    DeltaP = P1 - P2
                    OutletPressure = P2

                    ActualKv = Kvc

                    With oms
                        .Phases(0).Properties.temperature = Ti
                        .Phases(0).Properties.pressure = P2
                        .Phases(0).Properties.enthalpy = H2
                        Dim comp As BaseClasses.Compound
                        Dim i As Integer = 0
                        For Each comp In .Phases(0).Compounds.Values
                            comp.MoleFraction = ims.Phases(0).Compounds(comp.Name).MoleFraction
                            comp.MassFraction = ims.Phases(0).Compounds(comp.Name).MassFraction
                            comp.MassFlow = comp.MassFraction * Wi
                            comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight * 1000
                            i += 1
                        Next
                    End With

                    With ims
                        Dim comp As BaseClasses.Compound
                        Dim i As Integer = 0
                        For Each comp In .Phases(0).Compounds.Values
                            comp.MassFlow = comp.MassFraction * Wi
                            comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight * 1000
                            i += 1
                        Next
                    End With

            End Select


        End Sub

        Public Function SimpleKvLiquid(Wi As Double, rho As Double, P1 As Double, P2 As Double) As Double

            SimpleKvLiquid = Wi * 3600 / (1000.0 * rho * (P1 - P2) / 100000.0) ^ 0.5
        End Function

        Public Function SimpleKvGas(Wi As Double, rhog20 As Double, P1 As Double, P2 As Double, Ti As Double) As Double
            If P2 > P1 / 2 Then
                SimpleKvGas = Wi * 3600 / 519 * (Ti / (rhog20 * (P1 - P2) / 100000.0 * P2 / 100000.0)) ^ 0.5
            Else
                SimpleKvGas = Wi * 3600 / 259.5 / P1 * (Ti / rhog20) ^ 0.5
            End If
        End Function

        Public Function F_k(k As Double) As Double

            F_k = k / 1.4

        End Function

        Public Function Y_factor(x As Double, k As Double, xT As Double) As Double
            Y_factor = 1 - x / (3 * x_choked(k, xT))

        End Function

        Public Function x_ratio(P1 As Double, P2 As Double) As Double

            x_ratio = (P1 - P2) / P1

        End Function

        Public Function x_choked(k As Double, xT As Double) As Double
            x_choked = F_k(k) * xT
        End Function


        Public Function F_F(Pv As Double, Pc As Double) As Double

            F_F = 0.96 - 0.28 * (Pv / Pc) ^ 0.5

        End Function

        Public Function KvTwoPhase(Wi As Double, P1 As Double, P2 As Double, rhog As Double, rhol As Double, k As Double, Pv As Double, Pc As Double, massfrac_gas As Double, massfrac_liq As Double) As Double

            KvTwoPhase = (massfrac_gas * KvGas(Wi, P1, P2, k, rhog) ^ 2 + massfrac_liq * KvLiquid(Wi, P1, P2, rhol, Pv, Pc) ^ 2) ^ 0.5
        End Function

        Public Function WTwoPhase(Kv As Double, P1 As Double, P2 As Double, rhog As Double, rhol As Double, k As Double, Pv As Double, Pc As Double, massfrac_gas As Double, massfrac_liq As Double) As Double
            WTwoPhase = 1 / (massfrac_liq / WLiquid(Kv, P1, P2, rhol, Pv, Pc) ^ 2 + massfrac_gas / WGas(Kv, P1, P2, k, rhog) ^ 2) ^ 0.5
        End Function

        Public Function SimpleWTwoPhase(Kv As Double, P1 As Double, P2 As Double, Ti As Double, rhog20 As Double, rhol As Double, massfrac_gas As Double, massfrac_liq As Double) As Double
            Dim Wliquid, Wgas As Double

            Wliquid = Kv * (1000.0 * rhol * (P1 - P2)) ^ 0.5
            If P2 > P1 / 2 Then
                Wgas = 519 * Kv / (Ti / (rhog20 * (P1 - P2) * P1)) ^ 0.5
            Else
                Wgas = 259.5 * Kv * P1 / (Ti / rhog20) ^ 0.5
            End If

            SimpleWTwoPhase = 1 / (massfrac_liq / Wliquid ^ 2 + massfrac_gas / Wgas ^ 2) ^ 0.5
        End Function

        Public Function KvLiquid(Wi As Double, P1 As Double, P2 As Double, rho As Double, Pv As Double, Pc As Double) As Double
            Dim dP_choke

            dP_choke = FL ^ 2 * (P1 - F_F(Pv, Pc) * Pv)
            If dP_choke < (P1 - P2) Then
                P2 = P1 - dP_choke
            End If

            KvLiquid = Wi / FP / (rho * 999.1 * (P1 - P2)) ^ 0.5
        End Function

        Public Function WLiquid(Kv As Double, P1 As Double, P2 As Double, rho As Double, Pv As Double, Pc As Double) As Double
            Dim dP_choke

            dP_choke = FL ^ 2 * (P1 - F_F(Pv, Pc) * Pv)
            If dP_choke < (P1 - P2) Then
                P2 = P1 - dP_choke
            End If
            WLiquid = Kv * FP * (rho * 999.1 * (P1 - P2)) ^ 0.5

        End Function

        Public Function P1TwoPhase(Wi As Double, Kv As Double, P2 As Double, Ti As Double, rhog20 As Double, rhol As Double, massfrac_gas As Double, massfrac_liq As Double) As Double

            Dim x_c, Wtemp, Werror, P1, dP1, dW As Double
            Dim icount As Integer

            P1 = P2 * 1.1
            dP1 = 0.001
            Wtemp = SimpleWTwoPhase(Kv, P1, P2, Ti, rhog20, rhol, massfrac_gas, massfrac_liq)

            icount = 0
            Do While (Math.Abs(Wi - Wtemp) > 0.1)
                Werror = Wi - Wtemp
                dW = SimpleWTwoPhase(Kv, P1 + dP1, P2, Ti, rhog20, rhol, massfrac_gas, massfrac_liq) - Wtemp
                P1 = P1 + 0.5 * dP1 / dW * (Werror)
                If P1 < P2 Then P1 = P2 + 0.0001
                Wtemp = SimpleWTwoPhase(Kv, P1, P2, Ti, rhog20, rhol, massfrac_gas, massfrac_liq)

                If icount > 1000 Then Throw New Exception("P1 did not converge in 1000 iterations.")
                icount += 1
            Loop

            P1TwoPhase = P1

        End Function

        Public Function P2TwoPhase(Wi As Double, Kv As Double, P1 As Double, rhog As Double, rhol As Double, k As Double, Pv As Double, Pc As Double, massfrac_gas As Double, massfrac_liq As Double) As Double
            Dim P2_high, P2_low, P2_mid, x_c As Double
            Dim icount As Integer

            x_c = x_choked(k, xT)
            P2_high = P1
            P2_low = P2_high - P2_high * x_c

            If P2_low > (P1 - FL ^ 2 * (P1 - F_F(Pv, Pc) * Pv)) Then
                P2_low = P1 - FL ^ 2 * (P1 - F_F(Pv, Pc) * Pv)
            End If

            If WTwoPhase(Kv, P1, P2_low, rhog, rhol, k, Pv, Pc, massfrac_gas, massfrac_liq) < Wi Then
                Throw New Exception("Valve capacity too small, increase Kv")
            Else
                Do While Math.Abs(P2_high - P2_low) > 0.001
                    P2_mid = (P2_high + P2_low) / 2
                    If WTwoPhase(Kv, P1, P2_mid, rhog, rhol, k, Pv, Pc, massfrac_gas, massfrac_liq) > Wi Then
                        P2_low = P2_mid
                    Else
                        P2_high = P2_mid
                    End If
                    If icount > 1000 Then Throw New Exception("P2 did not converge in 1000 iterations.")
                    icount += 1
                Loop
            End If
            P2TwoPhase = (P2_high + P2_low) / 2
        End Function

        Public Function P2Liquid(Wi As Double, Kv As Double, P1 As Double, rho As Double, Pv As Double, Pc As Double) As Double

            Dim P2_high, P2_low As Double

            P2_high = P1
            P2_low = P1 - FL ^ 2 * (P1 - F_F(Pv, Pc) * Pv)

            Dim Wic = Kv * FP * (rho * 999.1 * (P1 - P2_low)) ^ 0.5

            If Wic < Wi Then
                Throw New Exception("Valve capacity too small, increase Kv")
            Else
                P2Liquid = P1 - 1 / (999.1 * rho) * (Wi / (Kv * FP)) ^ 2
            End If

        End Function

        Public Function KvGas(Wi As Double, P1 As Double, P2 As Double, k As Double, rho As Double)
            Dim Y, x, x_c As Double

            x = x_ratio(P1, P2)
            x_c = x_choked(k, xT)

            If x > x_c Then
                x = x_c
            End If

            Y = Y_factor(x, k, xT)

            KvGas = Wi * 1 / (N6 * FP * Y) / (x * P1 * rho) ^ 0.5
        End Function

        Public Function WGas(Kv As Double, P1 As Double, P2 As Double, k As Double, rho As Double)
            Dim Y, x, x_c As Double

            x = x_ratio(P1, P2)
            x_c = x_choked(k, xT)

            If x > x_c Then
                x = x_c
            End If

            Y = Y_factor(x, k, xT)

            WGas = Kv * (N6 * FP * Y) * (x * P1 * rho) ^ 0.5
        End Function


        Public Function P2_Gas(Wi As Double, Kv As Double, P1 As Double, k As Double, rho As Double)
            Dim P2_high, P2_low, P2_mid, x_c As Double
            Dim icount As Integer

            x_c = x_choked(k, xT)
            P2_high = P1
            P2_low = P2_high - P2_high * x_c

            icount = 0
            If (Kv * N6 * FP * Y_factor(x_c, k, xT) * (x_c * P1 * rho) ^ 0.5) < Wi Then
                Throw New Exception("Valve capacity too small, increase Kv")
            Else
                Do While Math.Abs(P2_high - P2_low) > 0.001
                    P2_mid = (P2_high + P2_low) / 2
                    If WGas(Kv, P1, P2_mid, k, rho) > Wi Then
                        P2_low = P2_mid
                    Else
                        P2_high = P2_mid
                    End If
                    If icount > 1000 Then Throw New Exception("P2 did not converge in 1000 iterations.")
                    icount += 1
                Loop
            End If
            P2_Gas = (P2_high + P2_low) / 2
        End Function

        Public Sub CalculateKv()

            Dim Ti, P1, Hi, Wi, ei, P2, rho, rhog20, rhog, rhol, volf, k, v2, Cp_ig, Pv, Pc As Double
            Dim massfrac_liq, massfrac_gas As Double

            Dim ims As MaterialStream = Me.GetInletMaterialStream(0)
            Dim oms As MaterialStream = Me.GetOutletMaterialStream(0)

            Me.PropertyPackage.CurrentMaterialStream = ims
            Me.PropertyPackage.CurrentMaterialStream.Validate()

            Ti = ims.Phases(0).Properties.temperature.GetValueOrDefault
            P1 = ims.Phases(0).Properties.pressure.GetValueOrDefault
            Hi = ims.Phases(0).Properties.enthalpy.GetValueOrDefault
            Wi = ims.Phases(0).Properties.massflow.GetValueOrDefault
            ei = Hi * Wi
            rho = ims.Phases(0).Properties.density.GetValueOrDefault
            volf = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault

            P2 = oms.Phases(0).Properties.pressure.GetValueOrDefault

            If Me.CalcMode = CalculationMode.DeltaP Then
                P2 = P1 - Me.DeltaP.GetValueOrDefault
            ElseIf CalcMode = CalculationMode.OutletPressure Then
                P2 = Me.OutletPressure.GetValueOrDefault
            Else
                P2 = oms.Phases(0).Properties.pressure.GetValueOrDefault
            End If


            If CalcMode = CalculationMode.Kv_Steam Then
                If P2 > P1 / 2 Then
                    v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P2)
                    Kv = Wi * 3600 / 31.62 * (v2 / ((P1 - P2) / 100000.0)) ^ 0.5
                Else
                    v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P1 / 2)
                    Kv = Wi * 3600 / 31.62 * (2 * v2 / (P1 / 100000.0)) ^ 0.5
                End If
            Else
                If ims.Phases(2).Properties.molarfraction = 1 Or CalcMode = CalculationMode.Kv_Gas Then
                    ims.PropertyPackage.CurrentMaterialStream = ims
                    rho = ims.PropertyPackage.AUX_VAPDENS(Ti, P1)

                    Cp_ig = ims.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Ti) * ims.Phases(2).Properties.molecularWeight()
                    k = Cp_ig / (Cp_ig - 8.314)
                    Kv = KvGas(Wi * 3600, P1 / 100000.0, P2 / 100000.0, k, rho)
                ElseIf ims.Phases(1).Properties.molarfraction = 1 Or CalcMode = CalculationMode.Kv_Liquid Then
                    Pv = ims.PropertyPackage.AUX_PVAPM(Ti)
                    Pc = ims.PropertyPackage.AUX_PCM(PropertyPackages.Phase.Liquid)
                    rho = ims.Phases(1).Properties.density.GetValueOrDefault
                    Kv = KvLiquid(Wi * 3600, P1 / 100000.0, P2 / 100000.0, rho, Pv / 100000.0, Pc / 100000.0)
                ElseIf ims.Phases(2).Properties.molarfraction > 0 And ims.Phases(1).Properties.molarfraction > 0 Then
                    ims.PropertyPackage.CurrentMaterialStream = ims
                    rhog = ims.Phases(2).Properties.density.GetValueOrDefault
                    Cp_ig = ims.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Ti) * ims.Phases(2).Properties.molecularWeight()
                    k = Cp_ig / (Cp_ig - 8.314)
                    rhol = ims.Phases(1).Properties.density.GetValueOrDefault
                    Pc = ims.PropertyPackage.AUX_PCM(PropertyPackages.Phase.Liquid)
                    Pv = P1 'ims.PropertyPackage.AUX_PVAPM(PropertyPackages.Phase.Liquid, Ti)

                    massfrac_gas = ims.Phases(2).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                    massfrac_liq = ims.Phases(1).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault

                    Kv = KvTwoPhase(Wi * 3600, P1 / 100000.0, P2 / 100000.0, rhog, rhol, k, Pv / 100000.0, Pc / 100000.0, massfrac_gas, massfrac_liq)
                End If
            End If

            If EnableOpeningKvRelationship Then
                Try
                    Dim ExpContext As New Ciloci.Flee.ExpressionContext
                    ExpContext.Imports.AddType(GetType(System.Math))
                    ExpContext.Variables.Clear()
                    ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                    ExpContext.Variables.Add("OP", OpeningPct)

                    Dim Expr = ExpContext.CompileGeneric(Of Double)(PercentOpeningVersusPercentKvExpression)
                    Kv = Kv / (Expr.Evaluate() / 100)

                Catch ex As Exception
                    Throw New Exception("Invalid expression for Kv/Opening relationship.")
                End Try
            End If

            If FlowCoefficient = FlowCoefficientType.Cv Then
                'Cv = 1.16 Kv
                Kv = 1.16 * Kv
            End If

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The Valve works like a fixed pressure drop for the process, where 
                                the outlet material stream properties are calculated beginning 
                                from the principle that the expansion is an isenthalpic process.")

            IObj?.Paragraphs.Add("The outlet stream pressure is calculated from the inlet pressure 
                                and the pressure drop. The outlet stream temperature is found by 
                                doing a PH Flash. This way, in the majority of cases, the outlet 
                                temperature will be less than or equal to the inlet one.")

            If args Is Nothing Then
                If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                    Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                    Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If
            End If

            Dim Ti, Pi, Hi, Wi, ei, ein, T2, P2, H2, H2c, rho, volf, rhog20, P2ant, v2, Kvc, T2est As Double
            Dim Cp_ig, k, Pv, Pc, rhog, rhol, massfrac_gas, massfrac_liq As Double
            Dim icount As Integer

            Dim ims, oms As MaterialStream

            If args IsNot Nothing Then
                ims = args(0)
                oms = args(1)
            Else
                ims = Me.GetInletMaterialStream(0)
                oms = Me.GetOutletMaterialStream(0)
            End If

            Me.PropertyPackage.CurrentMaterialStream = ims
            Me.PropertyPackage.CurrentMaterialStream.Validate()
            Ti = ims.Phases(0).Properties.temperature.GetValueOrDefault
            Pi = ims.Phases(0).Properties.pressure.GetValueOrDefault
            Hi = ims.Phases(0).Properties.enthalpy.GetValueOrDefault
            Wi = ims.Phases(0).Properties.massflow.GetValueOrDefault
            ei = Hi * Wi
            ein = ei
            rho = ims.Phases(0).Properties.density.GetValueOrDefault
            volf = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault

            If oms.GetTemperature() < ims.GetTemperature() Then
                T2est = oms.GetTemperature()
            Else
                T2est = ims.GetTemperature()
            End If

            H2 = Hi '- Me.DeltaP.GetValueOrDefault / (rho_li * 1000)

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, W = {3} kg/s", Ti, Pi, Hi, Wi))

            Dim FC As Double 'flow coefficient

            If FlowCoefficient = FlowCoefficientType.Cv Then
                'Cv = 1.16 Kv
                'Kv = Cv / 1.16
                FC = Kv / 1.16
            Else
                FC = Kv
            End If

            If EnableOpeningKvRelationship Then
                IObj?.Paragraphs.Add("<h2>Opening/Kv[Cv] relationship</h2>")
                IObj?.Paragraphs.Add("When this feature is enabled, you can enter an expression that relates the valve stem opening with the maximum flow value (Kvmax).")
                IObj?.Paragraphs.Add("The relationship between control valve capacity and valve stem travel is known as the Flow Characteristic of the 
                                    Control Valve. Trim design of the valve affects how the control valve capacity changes as the valve moves through 
                                    its complete travel. Because of the variation in trim design, many valves are not linear in nature. Valve trims 
                                    are instead designed, or characterized, in order to meet the large variety of control application needs. Many 
                                    control loops have inherent non linearity's, which may be possible to compensate selecting the control valve trim.")
                IObj?.Paragraphs.Add("<img src='https://www.engineeringtoolbox.com/docs/documents/485/Control_Valve_Flow_Characteristics.gif'></img>")
                Select Case DefinedOpeningKvRelationShipType
                    Case OpeningKvRelationshipType.UserDefined
                        Try
                            Dim ExpContext As New Ciloci.Flee.ExpressionContext()
                            ExpContext.Imports.AddType(GetType(System.Math))
                            ExpContext.Variables.Clear()
                            ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                            ExpContext.Variables.Add("OP", OpeningPct)
                            IObj?.Paragraphs.Add("Current Opening (%): " & OpeningPct)
                            IObj?.Paragraphs.Add("Opening/Kv[Cv]max relationship expression: " & PercentOpeningVersusPercentKvExpression)
                            Dim Expr = ExpContext.CompileGeneric(Of Double)(PercentOpeningVersusPercentKvExpression)
                            Kvc = FC * Expr.Evaluate() / 100
                            IObj?.Paragraphs.Add("Calculated Kv[Cv]/Kv[Cv]max (%): " & Kvc / FC * 100)
                            IObj?.Paragraphs.Add("Calculated Kv: " & Kvc)
                        Catch ex As Exception
                            Throw New Exception("Invalid expression for Kv[Cv]/Opening relationship.")
                        End Try
                    Case OpeningKvRelationshipType.QuickOpening
                        IObj?.Paragraphs.Add("Current Opening (%): " & OpeningPct)
                        Kvc = (OpeningPct / 100.0) ^ 0.5 * FC
                        IObj?.Paragraphs.Add("Calculated Kv[Cv]/Kv[Cv]max (%): " & Kvc / FC * 100)
                        IObj?.Paragraphs.Add("Calculated Kv: " & Kvc)
                    Case OpeningKvRelationshipType.Linear
                        IObj?.Paragraphs.Add("Current Opening (%): " & OpeningPct)
                        Kvc = OpeningPct / 100.0 * FC
                        IObj?.Paragraphs.Add("Calculated Kv[Cv]/Kv[Cv]max (%): " & Kvc / FC * 100)
                        IObj?.Paragraphs.Add("Calculated Kv: " & Kvc)
                    Case OpeningKvRelationshipType.EqualPercentage
                        IObj?.Paragraphs.Add("Current Opening (%): " & OpeningPct)
                        Kvc = CharacteristicParameter ^ (OpeningPct / 100.0 - 1.0) * FC
                        IObj?.Paragraphs.Add("Calculated Kv[Cv]/Kv[Cv]max (%): " & Kvc / FC * 100)
                        IObj?.Paragraphs.Add("Calculated Kv: " & Kvc)
                    Case OpeningKvRelationshipType.DataTable
                        IObj?.Paragraphs.Add("Current Opening (%): " & OpeningPct)
                        Try
                            Dim factor = MathNet.Numerics.Interpolate.RationalWithoutPoles(OpeningKvRelDataTableX, OpeningKvRelDataTableX).Interpolate(OpeningPct) / 100.0
                            Kvc = factor * FC
                            IObj?.Paragraphs.Add("Calculated Kv[Cv]/Kv[Cv]max (%): " & Kvc / FC * 100)
                            IObj?.Paragraphs.Add("Calculated Kv: " & Kvc)
                        Catch ex As Exception
                            Throw New Exception("Error calculating Kv from tabulated data: " + ex.Message)
                        End Try
                End Select
            Else
                Kvc = FC
            End If

            'reference: https://www.samson.de/document/t00050en.pdf

            If CalcMode = CalculationMode.Kv_General Or CalcMode = CalculationMode.Kv_Steam Then
                IObj?.Paragraphs.Add("<h2>Kv Calculation Mode</h2>")
                IObj?.Paragraphs.Add("Kv flow equations in DWSIM are implemented as per ANSI/ISA-75.01.01 and IEC 60534-2-1 for turbulent flow.")
                IObj?.Paragraphs.Add("Kv for two-phase service is adapted from Masoneilan and is eqivalent to other vendor equations e.g. Valtek, Parcol and Warren Controls.")
                IObj?.Paragraphs.Add("See <a href='https://dam.bakerhughes.com/m/47616eb160214a1d/original/MN-Valve-Sizing-Handbook-GEA19540A-English-pdf.pdf' > Masoneilan Control Valve Sizing Handbook</a> for more information on general valve sizing.")
                IObj?.Paragraphs.Add("For more information on simplified steam service sizing, see <a href='https://www.samson.de/document/t00050en.pdf'>this document</a>.")
                IObj?.Paragraphs.Add("The folowwing default valve style modifiers are applied:")
                IObj?.Paragraphs.Add("<mi>x_T= 0.75</mi>")
                IObj?.Paragraphs.Add("<mi>F_L = 0.9</mi>")
                IObj?.Paragraphs.Add("<mi>F_P = 1.0</mi>")
                IObj?.Paragraphs.Add("<mi>F_s = 1.0</mi>")
                IObj?.Paragraphs.Add("<mi>F_i = 0.9</mi>")

                IObj?.Paragraphs.Add(String.Format("Kv = {0}", Kvc))
            End If

            If CalcMode = CalculationMode.Kv_Gas Then
                ims.PropertyPackage.CurrentMaterialStream = ims
                rhog = ims.PropertyPackage.AUX_VAPDENS(Ti, Pi)
                Cp_ig = ims.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Ti) * ims.Phases(2).Properties.molecularWeight.GetValueOrDefault()
                k = Cp_ig / (Cp_ig - 8.314)
                P2 = P2_Gas(Wi * 3600, Kvc, Pi / 100000.0, k, rhog) * 100000.0
                IObj?.Paragraphs.Add(String.Format("Calculated Outlet Pressure P2 = {0} Pa", P2))
            ElseIf CalcMode = CalculationMode.Kv_Liquid Then
                Pv = ims.PropertyPackage.AUX_PVAPM(Ti)
                Pc = ims.PropertyPackage.AUX_PCM(PropertyPackages.Phase.Liquid)
                rhol = ims.Phases(1).Properties.density.GetValueOrDefault
                P2 = 100000.0 * P2Liquid(Wi * 3600, Kvc, Pi / 100000.0, rhol, Pv / 100000.0, Pc / 100000.0)
                IObj?.Paragraphs.Add(String.Format("Calculated Outlet Pressure P2 = {0} Pa", P2))
            ElseIf CalcMode = CalculationMode.Kv_General Then
                ims.PropertyPackage.CurrentMaterialStream = ims
                rhog = ims.Phases(2).Properties.density.GetValueOrDefault
                Cp_ig = ims.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Ti) * ims.Phases(2).Properties.molecularWeight.GetValueOrDefault()
                k = Cp_ig / (Cp_ig - 8.314)
                rhol = ims.Phases(1).Properties.density.GetValueOrDefault
                Pc = ims.PropertyPackage.AUX_PCM(PropertyPackages.Phase.Liquid)
                Pv = Pi 'ims.PropertyPackage.AUX_PVAPM(PropertyPackages.Phase.Liquid, Ti)
                massfrac_gas = ims.Phases(2).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                massfrac_liq = ims.Phases(1).Properties.massflow.GetValueOrDefault / ims.Phases(0).Properties.massflow.GetValueOrDefault
                If massfrac_gas > 0.01 And massfrac_liq > 0.01 Then
                    P2 = 100000.0 * P2TwoPhase(Wi * 3600, Kvc, Pi / 100000.0, rhog, rhol, k, Pv / 100000.0, Pc / 100000.0, massfrac_gas, massfrac_liq)
                ElseIf massfrac_liq <= 0.01 Then
                    ims.PropertyPackage.CurrentMaterialStream = ims
                    rhog = ims.PropertyPackage.AUX_VAPDENS(Ti, Pi)
                    Cp_ig = ims.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Ti) * ims.Phases(2).Properties.molecularWeight.GetValueOrDefault()
                    k = Cp_ig / (Cp_ig - 8.314)
                    P2 = P2_Gas(Wi * 3600, Kvc, Pi / 100000.0, k, rhog) * 100000.0
                ElseIf massfrac_gas <= 0.01 Then
                    Pv = ims.PropertyPackage.AUX_PVAPM(Ti)
                    Pc = ims.PropertyPackage.AUX_PCM(PropertyPackages.Phase.Liquid)
                    rhol = ims.Phases(1).Properties.density.GetValueOrDefault
                    P2 = 100000.0 * P2Liquid(Wi * 3600, Kvc, Pi / 100000.0, rhol, Pv / 100000.0, Pc / 100000.0)
                End If
                IObj?.Paragraphs.Add(String.Format("Calculated Outlet Pressure P2 = {0} Pa", P2))
            ElseIf CalcMode = CalculationMode.Kv_Steam Then
                P2 = Pi * 0.7 / 100000.0
                icount = 0
                Do
                    v2 = 1 / ims.PropertyPackage.AUX_VAPDENS(Ti, P2)
                    P2ant = P2
                    P2 = Pi / 100000.0 - v2 * (31.62 * Kvc / (Wi * 3600)) ^ -2
                    icount += 1
                    If icount > 10000 Then Throw New Exception("P2 did not converge in 10000 iterations.")
                Loop Until Math.Abs(P2 - P2ant) < 0.0001
                P2 = P2 * 100000.0
                IObj?.Paragraphs.Add(String.Format("Calculated Outlet Pressure P2 = {0} Pa", P2))
            End If

            If Me.CalcMode = CalculationMode.DeltaP Then
                P2 = Pi - Me.DeltaP.GetValueOrDefault
                OutletPressure = P2
            ElseIf CalcMode = CalculationMode.OutletPressure Then
                P2 = Me.OutletPressure.GetValueOrDefault
                Me.DeltaP = Pi - P2
            Else
                DeltaP = Pi - P2
                OutletPressure = P2
            End If

            ActualKv = Kvc

            CheckSpec(P2, True, "outlet pressure")

            If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

            IObj?.Paragraphs.Add(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

            IObj?.Paragraphs.Add(String.Format("Inlet Stream Enthalpy = {0} kJ/kg", Hi))

            IObj?.SetCurrent()
            Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, T2est)
            T2 = tmp.CalculatedTemperature
            CheckSpec(T2, True, "outlet temperature")
            H2c = tmp.CalculatedEnthalpy
            CheckSpec(H2c, False, "outlet enthalpy")

            If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

            IObj?.Paragraphs.Add(String.Format("Outlet Stream Enthalpy = {0} kJ/kg", H2c))
            IObj?.Paragraphs.Add(String.Format("Calculated Outlet Temperature T2 = {0} K", T2))

            Houtlet = H2c
            Hinlet = Hi

            'Dim htol As Double = Me.PropertyPackage.Parameters("PP_PHFELT")
            'Dim herr As Double = Math.Abs((H2c - H2) / H2)

            'If herr > 0.01 Then Throw New Exception("The enthalpy of inlet and outlet streams doesn't match. Result is invalid.")

            Me.DeltaT = T2 - Ti
            Me.DeltaQ = 0

            OutletTemperature = T2

            If Not DebugMode Then

                With oms
                    .AtEquilibrium = False
                    .Phases(0).Properties.temperature = T2
                    .Phases(0).Properties.pressure = P2
                    .Phases(0).Properties.enthalpy = H2
                    .Phases(0).Properties.massflow = ims.Phases(0).Properties.massflow.GetValueOrDefault
                    .DefinedFlow = FlowSpec.Mass
                    Dim comp As BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = ims.Phases(0).Compounds(comp.Name).MoleFraction
                        comp.MassFraction = ims.Phases(0).Compounds(comp.Name).MassFraction
                        i += 1
                    Next
                    .SpecType = StreamSpec.Pressure_and_Enthalpy
                End With

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If

            IObj?.Close()

        End Sub

        Public Overrides Sub DeCalculate()

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de materia conectada a jusante
                With Me.GetOutletMaterialStream(0)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
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
                        Case 4
                            value = Kv
                        Case 5
                            value = OpeningPct
                        Case 6
                            value = CharacteristicParameter
                    End Select

                    Return value

                Else

                    If prop.Equals("Actual Flow Coefficient") Then
                        Return ActualKv
                    End If

                End If

            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            Select Case proptype
                Case PropertyType.RO
                    For i = 3 To 3
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                    proplist.Add("Actual Flow Coefficient")
                Case PropertyType.RW
                    For i = 0 To 6
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 6
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 6
                        proplist.Add("PROP_VA_" + CStr(i))
                    Next
                    proplist.Add("Actual Flow Coefficient")
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter

            If prop.Contains("_") Then

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
                    Case 4
                        Me.Kv = propval
                    Case 5
                        If propval >= 0 And propval <= 100 Then
                            Me.OpeningPct = propval
                        ElseIf propval < 0 Then
                            OpeningPct = 0
                        Else
                            OpeningPct = 100
                        End If
                    Case 6
                        CharacteristicParameter = propval
                End Select

            End If

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String

            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 = "NF" Then

                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim value As String = ""

                If prop.Contains("_") Then

                    Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                    Select Case propidx

                        Case 0, 4, 5
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

                Else

                    Return ""

                End If


            Else

                Return u0

            End If

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_Valve With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Valve With {.SimObject = Me}
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
            Return My.Resources.valve
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("VALVE_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("VALVE_Name")
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

            Dim istr, ostr As MaterialStream
            istr = Me.GetInletMaterialStream(0)
            ostr = Me.GetOutletMaterialStream(0)

            istr.PropertyPackage.CurrentMaterialStream = istr

            str.AppendLine("Adiabatic Valve: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Inlet conditions")
            str.AppendLine()
            str.AppendLine("    Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, istr.Phases(0).Properties.temperature.GetValueOrDefault).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, istr.Phases(0).Properties.pressure.GetValueOrDefault).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Mass flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.massflow, istr.Phases(0).Properties.massflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.massflow)
            str.AppendLine("    Mole flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, istr.Phases(0).Properties.molarflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.molarflow)
            str.AppendLine("    Volumetric flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, istr.Phases(0).Properties.volumetric_flow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.volumetricFlow)
            str.AppendLine("    Vapor fraction: " & istr.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine("    Compounds: " & istr.PropertyPackage.RET_VNAMES.ToArrayString)
            str.AppendLine("    Molar composition: " & istr.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString(ci))
            str.AppendLine()
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Calculation mode: " & CalcMode.ToString)
            Select Case Me.CalcMode
                Case CalculationMode.DeltaP
                    str.AppendLine("    Pressure decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure).ToString(numberformat, ci) & " " & su.pressure)
                Case Else
                    str.AppendLine("    Kv(max): " & Kv)
            End Select
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case Me.CalcMode
                Case CalculationMode.DeltaP
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure).ToString(numberformat, ci) & " " & su.pressure)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Pressure decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
                Case Else
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.OutletPressure).ToString(numberformat, ci) & " " & su.pressure)
                    str.AppendLine("    Pressure decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
            End Select
            str.AppendLine("    Inlet enthalpy: " & SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Hinlet).ToString(numberformat, ci) & " " & su.enthalpy)
            str.AppendLine("    Outlet enthalpy: " & SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Houtlet).ToString(numberformat, ci) & " " & su.enthalpy)
            str.AppendLine("    Temperature decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT).ToString(numberformat, ci) & " " & su.deltaT)

            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Adiabatic Valve '" & Me.GraphicObject?.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Calculation Mode",
                    CalcMode.ToString}))

            Select Case CalcMode
                Case CalculationMode.DeltaP
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Drop",
                            Me.DeltaP.GetValueOrDefault.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
                Case CalculationMode.OutletPressure
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.OutletPressure.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                Case Else
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Kv (max)",
                            Me.Kv.ToString(nf),
                            ""}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            Select Case CalcMode
                Case CalculationMode.DeltaP
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.OutletPressure.GetValueOrDefault.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                Case Else
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Drop",
                            Me.DeltaP.GetValueOrDefault.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Temperature Change",
                            Me.DeltaT.GetValueOrDefault.ConvertFromSI(su.deltaT).ToString(nf),
                            su.deltaT}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Inlet Enthalpy",
                            Me.Hinlet.ConvertFromSI(su.enthalpy).ToString(nf),
                            su.enthalpy}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Enthalpy",
                            Me.Houtlet.ConvertFromSI(su.enthalpy).ToString(nf),
                            su.enthalpy}))

            Return list

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Calculation Mode") Then
                Return "Select the calculation mode of this valve."
            ElseIf p.Equals("Pressure Drop") Then
                Return "If you chose 'Pressure Drop' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            ElseIf p.Equals("Outlet Pressure") Then
                Return "If you chose 'Outlet Pressure' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace

