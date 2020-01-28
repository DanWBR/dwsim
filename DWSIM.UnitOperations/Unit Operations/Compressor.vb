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


Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.PumpOps
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.MathOps.MathEx.Interpolation

Namespace UnitOperations

    <System.Serializable()> Public Class Compressor

        Inherits UnitOperations.UnitOpBaseClass

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.PressureChangers

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_ComprExpndr

        Public Enum CalculationMode
            OutletPressure = 0
            Delta_P = 1
            EnergyStream = 2
            PowerRequired = 3
            Head = 4
            Curves = 5
        End Enum

        Public Enum ProcessPathType
            Adiabatic = 0
            Polytropic = 1
        End Enum

        Public Property OutletTemperature As Double = 0.0#

        Public Property CalcMode() As CalculationMode = CalculationMode.OutletPressure

        Public Property ProcessPath As ProcessPathType = ProcessPathType.Adiabatic

        Public Property IgnorePhase() As Boolean

        Public Property PolytropicEfficiency() As Double = 75.0

        Public Property AdiabaticEfficiency() As Double = 75.0

        Public Property DeltaP() As Double = 0.0

        Public Property DeltaT() As Double = 0.0

        Public Property DeltaQ() As Double = 0.0

        Public Property POut() As Double = 101325.0

        Public Property AdiabaticCoefficient As Double = 0.0

        Public Property PolytropicCoefficient As Double = 0.0

        Public Property AdiabaticHead As Double = 0.0

        Public Property PolytropicHead As Double = 0.0

        'curves

        Public Property EquipType As String = ""

        Public Property CurvesDB As String = ""

        Public Property CurveFlow As Double

        Public Property CurveEff As Double

        Public Property CurveHead As Double

        Public Property CurvePower As Double

        Public Property Curves As New Dictionary(Of Integer, Dictionary(Of String, PumpOps.Curve))

        Public Property Speed As Integer = 1500

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Curves = New Dictionary(Of Integer, Dictionary(Of String, Curve))
            Try
                For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Curves").Elements.ToList
                    Dim dict As New Dictionary(Of String, PumpOps.Curve)
                    For Each xel2 In xel.Elements
                        Dim cv As New PumpOps.Curve()
                        cv.LoadData(xel2.Elements.ToList)
                        dict.Add(xel2.Name.ToString, cv)
                    Next
                    Curves.Add(Integer.Parse(xel.@RotationSpeed.ToString), dict)
                Next
            Catch ex As Exception
            End Try
            If Curves.Count = 0 Then Me.Curves.Add(Speed, CreateCurves())

            Dim eleff = data.Where(Function(x) x.Name = "EficienciaAdiabatica").FirstOrDefault
            If eleff IsNot Nothing Then
                AdiabaticEfficiency = eleff.Value.ToDoubleFromInvariant
            End If

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("Curves"))
                For Each kvp In Curves
                    Dim xel As XElement = New XElement("CurveSet", New XAttribute("RotationSpeed", kvp.Key))
                    .Item(.Count - 1).Add(xel)
                    For Each kvp2 In kvp.Value
                        xel.Add(New XElement(kvp2.Key.ToString, kvp2.Value.SaveData.ToArray()))
                    Next
                Next
            End With

            Return elements

        End Function

        Public Function CreateCurves() As Dictionary(Of String, PumpOps.Curve)

            Dim dict As New Dictionary(Of String, PumpOps.Curve)
            dict.Add("HEAD", New PumpOps.Curve(Guid.NewGuid().ToString, "HEAD", PumpOps.CurveType.Head))
            dict.Add("EFF", New PumpOps.Curve(Guid.NewGuid().ToString, "EFF", PumpOps.CurveType.Efficiency))
            dict.Add("POWER", New PumpOps.Curve(Guid.NewGuid().ToString, "POWER", PumpOps.CurveType.Power))

            Return dict

        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)
            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description
        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Compressor()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Compressor)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The compressor is used to provide energy to a vapor stream in the 
                            form of pressure. The ideal process is isentropic (constant 
                            entropy) and the non-idealities are considered according to the 
                            compressor efficiency, which is defined by the user.")

            IObj?.Paragraphs.Add("Calculation Method")

            IObj?.Paragraphs.Add("The compressor calculation is different for the two cases (when 
                                the provided delta-p or energy stream / power is 
                                used). In the first method, we have the following sequence:")

            IObj?.Paragraphs.Add("• Outlet pressure calculation:")

            IObj?.Paragraphs.Add("<m>P_{2}=P_{1}+\Delta P</m>")

            IObj?.Paragraphs.Add("• Outlet enthalpy: A PS Flash (Pressure-Entropy) is done to 
                              obtain the ideal process enthalpy change. The outlet real 
                              enthalpy is then calculated by:")

            IObj?.Paragraphs.Add("<m>H_{2}=H_{1}+\frac{\Delta H_{id}}{\eta\,W},</m>")

            IObj?.Paragraphs.Add("• Outlet temperature: PH Flash with <mi>P_{2}</mi> and <mi>H_{2}</mi>.")

            IObj?.Paragraphs.Add("In the second case (calculated outlet pressure), we have the 
                                following sequence:")

            IObj?.Paragraphs.Add("• Discharge pressure:")

            IObj?.Paragraphs.Add("<m>P_{2}=P_{1}[1+\frac{Pot}{\eta W}\frac{k-1}{k}\frac{MM}{8.314T_{1}}]^{[k/(k-1)]},</m>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>P_{2}</mi> outlet stream pressure")

            IObj?.Paragraphs.Add("<mi>P_{1}</mi> inlet stream pressure")

            IObj?.Paragraphs.Add("<mi>Pot</mi> compressor power")

            IObj?.Paragraphs.Add("<mi>W</mi> mass flow")

            IObj?.Paragraphs.Add("<mi>\eta</mi> compressor adiabatic efficiency")

            IObj?.Paragraphs.Add("<mi>k</mi> adiabatic coefficient <mi>(Cp_{gi}/Cv_{gi})</mi>")

            IObj?.Paragraphs.Add("<mi>MM</mi> gas molecular weight")

            IObj?.Paragraphs.Add("<mi>T_{1}</mi> inlet stream temperature")

            IObj?.Paragraphs.Add("The calculated outlet pressure using the above expression is used as a first estimate to calculate the power in an inner loop. The outlet pressure is updated is then updated until the calculated power matches the specified one.")

            IObj?.Paragraphs.Add("• Outlet enthalpy: A PS Flash (Pressure-Entropy) is done to 
                              obtain the ideal process enthalpy change. The outlet real 
                              enthalpy is then calculated by: ")

            IObj?.Paragraphs.Add("<m>H_{2}=H_{1}+\frac{\Delta H_{id}}{\eta\,W},</m>")

            IObj?.Paragraphs.Add("• Outlet temperature: PH Flash with <mi>P_{2}</mi> and <mi>H_{2}</mi>.")

            IObj?.Paragraphs.Add("Isentropic and Polytropic Coefficients are calculated from:")

            IObj?.Paragraphs.Add("<mi>n_i = \frac{\ln \left({P_2}/{P_1}\right) }{\ln\left(\rho_{2i}/\rho_1\right)} </mi>")

            IObj?.Paragraphs.Add("<mi>n_p = \frac{\ln \left({P_2}/{P_1}\right) }{\ln\left(\rho_{2}/\rho_1\right)} </mi>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>\rho_{2i}</mi> Outlet Gas Density calculated with Inlet Gas Entropy")

            If Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim Ti, Pi, Hi, Si, Wi, rho_vi, qvi, qli, ei, ein, T2, T2s, P2, P2i, Qloop, Qi, H2, H2s, cpig, cp, cv, mw, fx, fx0, fx00, P2i0, P2i00 As Double

            Dim msin, msout As MaterialStream, esin As Streams.EnergyStream

            msin = GetInletMaterialStream(0)
            msout = GetOutletMaterialStream(0)
            esin = GetInletEnergyStream(1)

            qli = msin.Phases(1).Properties.volumetric_flow.ToString

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            IObj?.Paragraphs.Add("Calculation Mode: " & CalcMode.ToString)

            If Not IgnorePhase And DebugMode Then AppendDebugLine("Checking if there is a liquid phase in the inlet stream...")

            If qli > 0 And Not Me.IgnorePhase Then Throw New Exception(FlowSheet.GetTranslatedString("ExisteumaPhaselquidan"))

            Select Case CalcMode

                Case CalculationMode.EnergyStream, CalculationMode.Head, CalculationMode.PowerRequired

Curves:             Me.PropertyPackage.CurrentMaterialStream = msin
                    Ti = msin.Phases(0).Properties.temperature
                    Pi = msin.Phases(0).Properties.pressure
                    rho_vi = msin.Phases(2).Properties.density
                    IObj?.SetCurrent()
                    cpig = Me.PropertyPackage.AUX_CPm(PhaseName.Vapor, Ti)
                    cp = msin.Phases(0).Properties.heatCapacityCp
                    cv = msin.Phases(0).Properties.heatCapacityCv
                    mw = msin.Phases(0).Properties.molecularWeight
                    qvi = msin.Phases(2).Properties.volumetric_flow
                    Hi = msin.Phases(0).Properties.enthalpy
                    Si = msin.Phases(0).Properties.entropy
                    Wi = msin.Phases(0).Properties.massflow
                    Qi = msin.Phases(0).Properties.molarflow
                    ei = Hi * Wi
                    ein = ei

                    IObj?.Paragraphs.Add("<h3>Input Variables</h3>")

                    IObj?.Paragraphs.Add(String.Format("<mi>W</mi>: {0} kg/s", Wi))
                    IObj?.Paragraphs.Add(String.Format("<mi>P_1</mi>: {0} Pa", Pi))
                    IObj?.Paragraphs.Add(String.Format("<mi>H_1</mi>: {0} kJ/kg", Hi))
                    IObj?.Paragraphs.Add(String.Format("<mi>S_1</mi>: {0} kJ/[kg.K]", Si))
                    IObj?.Paragraphs.Add(String.Format("<mi>\eta</mi>: {0} %", AdiabaticEfficiency))

                    If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Name))
                    If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, S = {3} kJ/[kg.K], W = {4} kg/s, cp = {5} kJ/[kg.K]", Ti, Pi, Hi, Si, Wi, cp))

                    Dim energystream As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)

                    Select Case Me.CalcMode
                        Case CalculationMode.EnergyStream
                            Me.DeltaQ = energystream.EnergyFlow
                            If DebugMode Then AppendDebugLine(String.Format("Power from energy stream: {0} kW", DeltaQ))
                        Case CalculationMode.PowerRequired
                            If DebugMode Then AppendDebugLine(String.Format("Power from definition: {0} kW", DeltaQ))
                        Case CalculationMode.Head, CalculationMode.Curves
                            If ProcessPath = ProcessPathType.Adiabatic Then
                                DeltaQ = AdiabaticHead / 1000 / Wi * 9.8
                            Else
                                DeltaQ = PolytropicHead / 1000 / Wi * 9.8
                            End If
                    End Select

                    CheckSpec(Me.DeltaQ, True, "power")

                    With esin
                        .EnergyFlow = Me.DeltaQ
                        .GraphicObject.Calculated = True
                    End With

                    Dim k As Double = cp / cv

                    If ProcessPath = ProcessPathType.Adiabatic Then
                        P2i = Pi * ((1 + DeltaQ * (Me.AdiabaticEfficiency / 100) / Wi * (k - 1) / k * mw / 8.314 / Ti)) ^ (k / (k - 1))
                    Else
                        P2i = Pi * ((1 + DeltaQ * (Me.PolytropicEfficiency / 100) / Wi * (k - 1) / k * mw / 8.314 / Ti)) ^ (k / (k - 1))
                    End If

                    Dim tmp As IFlashCalculationResult

                    Dim icnt As Integer = 0

                    'recalculate Q with P2i

                    Do

                        IObj?.SetCurrent()
                        tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEntropy, P2i, Si, 0)

                        If ProcessPath = ProcessPathType.Adiabatic Then
                            Qloop = Wi * (tmp.CalculatedEnthalpy - Hi) / (Me.AdiabaticEfficiency / 100)
                        Else
                            Qloop = Wi * (tmp.CalculatedEnthalpy - Hi) / (Me.PolytropicEfficiency / 100)
                        End If

                        If DebugMode Then AppendDebugLine(String.Format("Qi: {0}", Qi))

                        fx00 = fx0
                        fx0 = fx
                        fx = Qloop - DeltaQ

                        P2i00 = P2i0
                        P2i0 = P2i

                        If icnt <= 2 Then
                            P2i *= 1.01
                        Else
                            P2i = P2i - fx / ((fx - fx00) / (P2i - P2i00))
                        End If

                        If DebugMode Then AppendDebugLine(String.Format("P2i: {0}", P2i))

                        icnt += 1

                    Loop Until Math.Abs((DeltaQ - Qloop) / DeltaQ) < 0.001

                    P2 = P2i

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet pressure: {0} Pa", P2))

                    CheckSpec(P2, True, "outlet pressure")

                    DeltaP = P2 - Pi
                    POut = P2

                    CheckSpec(Si, False, "inlet entropy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PS flash to calculate ideal outlet enthalpy... P = {0} Pa, S = {1} kJ/[kg.K]", P2, Si))

                    IObj?.SetCurrent()
                    tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEntropy, P2, Si, 0)
                    T2s = tmp.CalculatedTemperature
                    H2s = tmp.CalculatedEnthalpy

                    IObj?.Paragraphs.Add("<h3>Results</h3>")

                    IObj?.Paragraphs.Add("<mi>S_{2,id}</mi>: " & String.Format("{0} kJ/[kg.K]", tmp.CalculatedEntropy))
                    IObj?.Paragraphs.Add("<mi>T_{2,id}</mi>: " & String.Format("{0} K", tmp.CalculatedTemperature))
                    IObj?.Paragraphs.Add("<mi>H_{2,id}</mi>: " & String.Format("{0} kJ/kg", tmp.CalculatedEnthalpy))

                    If DebugMode Then AppendDebugLine(String.Format("Calculated ideal outlet enthalpy Hid = {0} kJ/kg", tmp.CalculatedEnthalpy))

                    If ProcessPath = ProcessPathType.Adiabatic Then
                        H2 = Hi + (H2s - Hi) / (Me.AdiabaticEfficiency / 100)
                    Else
                        H2 = Hi + (H2s - Hi) / (Me.PolytropicEfficiency / 100)
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("Calculated real outlet enthalpy Hr = {0} kJ/kg", H2))

                    CheckSpec(H2, False, "outlet enthalpy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    IObj?.SetCurrent()
                    tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)

                    T2 = tmp.CalculatedTemperature

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    CheckSpec(T2, True, "outlet temperature")

                    Me.DeltaT = T2 - Ti

                    OutletTemperature = T2

                    IObj?.Paragraphs.Add(String.Format("<mi>P_2</mi>: {0} Pa", P2))
                    IObj?.Paragraphs.Add(String.Format("<mi>H_2</mi>: {0} kJ/kg", H2))
                    IObj?.Paragraphs.Add(String.Format("<mi>S_2</mi>: {0} kJ/[kg.K]", tmp.CalculatedEntropy))
                    IObj?.Paragraphs.Add(String.Format("<mi>T_2</mi>: {0} K", T2))

                    Dim rho1, rho2, rho2i, n_isent, n_poly, fce As Double
                    Dim tms As MaterialStream = msin.Clone()

                    rho1 = msin.GetPhase("Mixture").Properties.density.GetValueOrDefault

                    tms.PropertyPackage = PropertyPackage
                    PropertyPackage.CurrentMaterialStream = tms
                    tms.Phases(0).Properties.temperature = T2s
                    tms.Phases(0).Properties.pressure = P2
                    tms.Calculate()

                    rho2i = tms.GetPhase("Mixture").Properties.density.GetValueOrDefault

                    tms.PropertyPackage = PropertyPackage
                    PropertyPackage.CurrentMaterialStream = tms
                    tms.Phases(0).Properties.temperature = T2
                    tms.Phases(0).Properties.pressure = P2
                    tms.Calculate()

                    rho2 = tms.GetPhase("Mixture").Properties.density.GetValueOrDefault

                    ' volume exponent (isent)

                    n_isent = Math.Log(P2 / Pi) / Math.Log(rho2i / rho1)

                    ' volume exponent (polyt)

                    n_poly = Math.Log(P2 / Pi) / Math.Log(rho2 / rho1)

                    ' efficiencies

                    fce = ((P2 / Pi) ^ ((n_poly - 1) / n_poly) - 1) * ((n_poly / (n_poly - 1)) * (n_isent - 1) / n_isent) / ((P2 / Pi) ^ ((n_isent - 1) / n_isent) - 1)

                    If ProcessPath = ProcessPathType.Adiabatic Then

                        PolytropicEfficiency = fce * AdiabaticEfficiency

                    Else

                        AdiabaticEfficiency = PolytropicEfficiency / fce

                    End If

                    AdiabaticCoefficient = n_isent

                    PolytropicCoefficient = n_poly

                    Dim Wic, Wpc As Double

                    Wic = DeltaQ / (AdiabaticEfficiency / 100)

                    Wpc = DeltaQ / (PolytropicEfficiency / 100)

                    If CalcMode = CalculationMode.Head And ProcessPath = ProcessPathType.Adiabatic Then

                        PolytropicHead = Wpc * 1000 * Wi / 9.8 ' m

                    ElseIf CalcMode = CalculationMode.Head And ProcessPath = ProcessPathType.Polytropic Then

                        AdiabaticHead = Wic * 1000 * Wi / 9.8 ' m

                    Else

                        AdiabaticHead = Wic * 1000 * Wi / 9.8 ' m
                        PolytropicHead = Wpc * 1000 * Wi / 9.8 ' m

                    End If

                    IObj?.Paragraphs.Add(String.Format("<mi>n_i</mi>: {0} ", n_isent))

                    IObj?.Paragraphs.Add(String.Format("<mi>n_p</mi>: {0} ", n_poly))

                    IObj?.Paragraphs.Add(String.Format("<mi>\eta_i</mi>: {0} ", AdiabaticEfficiency / 100))

                    IObj?.Paragraphs.Add(String.Format("<mi>\eta_p</mi>: {0} ", PolytropicEfficiency / 100))

                    If Not DebugMode Then

                        'Atribuir valores a corrente de materia conectada a jusante
                        With msout
                            .Phases(0).Properties.temperature = T2
                            .Phases(0).Properties.pressure = P2
                            .Phases(0).Properties.enthalpy = H2
                            Dim comp As BaseClasses.Compound
                            For Each comp In .Phases(0).Compounds.Values
                                comp.MoleFraction = msin.Phases(0).Compounds(comp.Name).MoleFraction
                                comp.MassFraction = msin.Phases(0).Compounds(comp.Name).MassFraction
                            Next
                            .Phases(0).Properties.massflow = msin.Phases(0).Properties.massflow
                        End With

                    End If

                Case CalculationMode.Delta_P, CalculationMode.OutletPressure

                    Me.PropertyPackage.CurrentMaterialStream = msin
                    Ti = msin.Phases(0).Properties.temperature
                    Pi = msin.Phases(0).Properties.pressure
                    rho_vi = msin.Phases(2).Properties.density
                    qvi = msin.Phases(2).Properties.volumetric_flow
                    Hi = msin.Phases(0).Properties.enthalpy
                    Si = msin.Phases(0).Properties.entropy
                    Wi = msin.Phases(0).Properties.massflow
                    Qi = msin.Phases(0).Properties.molarflow
                    mw = msin.Phases(0).Properties.molecularWeight
                    ei = Hi * Wi
                    ein = ei

                    IObj?.Paragraphs.Add("<h3>Input Variables</h3>")

                    IObj?.Paragraphs.Add(String.Format("<mi>W</mi>: {0} kg/s", Wi))
                    IObj?.Paragraphs.Add(String.Format("<mi>P_1</mi>: {0} Pa", Pi))
                    IObj?.Paragraphs.Add(String.Format("<mi>H_1</mi>: {0} kJ/kg", Hi))
                    IObj?.Paragraphs.Add(String.Format("<mi>S_1</mi>: {0} kJ/[kg.K]", Si))
                    IObj?.Paragraphs.Add(String.Format("<mi>\eta</mi>: {0} %", AdiabaticEfficiency))

                    If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Name))
                    If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, S = {3} kJ/[kg.K], W = {4} kg/s", Ti, Pi, Hi, Si, Wi, cp))

                    Me.PropertyPackage.CurrentMaterialStream = msin

                    Select Case Me.CalcMode
                        Case CalculationMode.Delta_P
                            P2 = Pi + Me.DeltaP
                            POut = P2
                        Case CalculationMode.OutletPressure
                            P2 = Me.POut
                            DeltaP = P2 - Pi
                    End Select

                    CheckSpec(Si, False, "inlet entropy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PS flash to calculate ideal outlet enthalpy... P = {0} Pa, S = {1} kJ/[kg.K]", P2, Si))

                    IObj?.SetCurrent()
                    Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEntropy, P2, Si, 0)
                    T2 = tmp.CalculatedTemperature
                    T2s = T2
                    H2 = tmp.CalculatedEnthalpy
                    H2s = H2

                    IObj?.Paragraphs.Add("<h3>Results</h3>")

                    IObj?.Paragraphs.Add("<mi>S_{2,id}</mi>: " & String.Format("{0} kJ/[kg.K]", tmp.CalculatedEntropy))
                    IObj?.Paragraphs.Add("<mi>T_{2,id}</mi>: " & String.Format("{0} K", T2))
                    IObj?.Paragraphs.Add("<mi>H_{2,id}</mi>: " & String.Format("{0} kJ/kg", H2))

                    If DebugMode Then AppendDebugLine(String.Format("Calculated ideal outlet enthalpy Hid = {0} kJ/kg", tmp.CalculatedEnthalpy))

                    CheckSpec(T2, True, "outlet temperature")
                    CheckSpec(H2, False, "outlet enthalpy")

                    If ProcessPath = ProcessPathType.Adiabatic Then

                        Me.DeltaQ = Wi * (H2s - Hi) / (Me.AdiabaticEfficiency / 100)

                    Else

                        Me.DeltaQ = Wi * (H2s - Hi) / (Me.PolytropicEfficiency / 100)

                    End If

                    If DebugMode Then AppendDebugLine(String.Format("Calculated real compressor power = {0} kW", DeltaQ))

                    CheckSpec(DeltaQ, True, "power")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, Hi + Me.DeltaQ / Wi))

                    IObj?.SetCurrent()
                    PropertyPackage.CurrentMaterialStream = msin
                    tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, Hi + Me.DeltaQ / Wi, T2)
                    T2 = tmp.CalculatedTemperature
                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    CheckSpec(T2, True, "outlet temperature")

                    H2 = Hi + Me.DeltaQ / Wi

                    OutletTemperature = T2

                    IObj?.Paragraphs.Add(String.Format("<mi>P_2</mi>: {0} Pa", P2))
                    IObj?.Paragraphs.Add(String.Format("<mi>H_2</mi>: {0} kJ/kg", H2))
                    IObj?.Paragraphs.Add(String.Format("<mi>S_2</mi>: {0} kJ/[kg.K]", tmp.CalculatedEntropy))
                    IObj?.Paragraphs.Add(String.Format("<mi>T_2</mi>: {0} K", T2))

                    Dim rho1, rho2, rho2i, n_isent, n_poly, CFi, CFp, Wisent, Wpoly, Wic, Wpc, fce As Double
                    Dim tms As MaterialStream = msin.Clone()

                    rho1 = msin.GetPhase("Mixture").Properties.density.GetValueOrDefault

                    tms.PropertyPackage = PropertyPackage
                    PropertyPackage.CurrentMaterialStream = tms
                    tms.Phases(0).Properties.temperature = T2s
                    tms.Phases(0).Properties.pressure = P2
                    tms.Calculate()

                    rho2i = tms.GetPhase("Mixture").Properties.density.GetValueOrDefault

                    tms.PropertyPackage = PropertyPackage
                    PropertyPackage.CurrentMaterialStream = tms
                    tms.Phases(0).Properties.temperature = T2
                    tms.Phases(0).Properties.pressure = P2
                    tms.Calculate()

                    rho2 = tms.GetPhase("Mixture").Properties.density.GetValueOrDefault

                    ' volume exponent (isent)

                    n_isent = Math.Log(P2 / Pi) / Math.Log(rho2i / rho1)

                    ' volume exponent (polyt)

                    n_poly = Math.Log(P2 / Pi) / Math.Log(rho2 / rho1)

                    fce = ((P2 / Pi) ^ ((n_poly - 1) / n_poly) - 1) * ((n_poly / (n_poly - 1)) * (n_isent - 1) / n_isent) / ((P2 / Pi) ^ ((n_isent - 1) / n_isent) - 1)

                    ' real work

                    If ProcessPath = ProcessPathType.Adiabatic Then

                        PolytropicEfficiency = AdiabaticEfficiency * fce

                    Else

                        AdiabaticEfficiency = PolytropicEfficiency / fce

                    End If

                    Wic = DeltaQ / (AdiabaticEfficiency / 100)

                    Wpc = DeltaQ / (PolytropicEfficiency / 100)

                    ' heads

                    AdiabaticHead = Wic * 1000 * Wi / 9.8 ' m

                    PolytropicHead = Wpc * 1000 * Wi / 9.8 ' m

                    AdiabaticCoefficient = n_isent

                    PolytropicCoefficient = n_poly

                    IObj?.Paragraphs.Add(String.Format("<mi>n_i</mi>: {0} ", n_isent))

                    IObj?.Paragraphs.Add(String.Format("<mi>n_p</mi>: {0} ", n_poly))

                    IObj?.Paragraphs.Add(String.Format("<mi>\eta_i</mi>: {0} ", AdiabaticEfficiency / 100))

                    IObj?.Paragraphs.Add(String.Format("<mi>\eta_p</mi>: {0} ", PolytropicEfficiency / 100))

                    If Not DebugMode Then

                        'Atribuir valores a corrente de materia conectada a jusante
                        With msout
                            .Phases(0).Properties.temperature = T2
                            .Phases(0).Properties.pressure = P2
                            .Phases(0).Properties.enthalpy = H2
                            Dim comp As BaseClasses.Compound
                            For Each comp In .Phases(0).Compounds.Values
                                comp.MoleFraction = msin.Phases(0).Compounds(comp.Name).MoleFraction
                                comp.MassFraction = msin.Phases(0).Compounds(comp.Name).MassFraction
                            Next
                            .Phases(0).Properties.massflow = msin.Phases(0).Properties.massflow
                        End With

                        'energy stream - update energy flow value (kW)
                        With esin
                            .EnergyFlow = Me.DeltaQ
                            .GraphicObject.Calculated = True
                        End With

                    End If

                Case CalculationMode.Curves

                    Dim chead, ceff, cpower As PumpOps.Curve

                    If DebugMode Then AppendDebugLine(String.Format("Creating curves..."))

                    If Me.Curves.Count = 0 Then Me.Curves.Add(Speed, CreateCurves())

                    Dim LHeadSpeed, LHead, LPowerSpeed, LPower, LEffSpeed, LEff As New List(Of Double)

                    For Each datapair In Me.Curves

                        chead = datapair.Value("HEAD")
                        ceff = datapair.Value("EFF")
                        cpower = datapair.Value("POWER")

                        Dim xhead, yhead, xeff, yeff, xpower, ypower As New ArrayList

                        Dim qint As Double

                        If chead.xunit.Contains("@ P,T") Then
                            'actual flow
                            qint = msin.Phases(0).Properties.volumetric_flow
                        Else
                            ' molar flow
                            qint = msin.Phases(0).Properties.molarflow
                        End If

                        Dim i As Integer

                        For i = 0 To chead.x.Count - 1
                            If Double.TryParse(chead.x(i), New Double) And Double.TryParse(chead.y(i), New Double) Then
                                xhead.Add(SystemsOfUnits.Converter.ConvertToSI(chead.xunit.Replace(" @ P,T", ""), chead.x(i)))
                                yhead.Add(SystemsOfUnits.Converter.ConvertToSI(chead.yunit, chead.y(i)))
                            End If
                        Next
                        For i = 0 To cpower.x.Count - 1
                            If Double.TryParse(cpower.x(i), New Double) And Double.TryParse(cpower.y(i), New Double) Then
                                xpower.Add(SystemsOfUnits.Converter.ConvertToSI(cpower.xunit.Replace(" @ P,T", ""), cpower.x(i)))
                                ypower.Add(SystemsOfUnits.Converter.ConvertToSI(cpower.yunit, chead.y(i)))
                            End If
                        Next
                        For i = 0 To ceff.x.Count - 1
                            If Double.TryParse(ceff.x(i), New Double) And Double.TryParse(ceff.y(i), New Double) Then
                                xeff.Add(SystemsOfUnits.Converter.ConvertToSI(ceff.xunit.Replace(" @ P,T", ""), ceff.x(i)))
                                If ceff.yunit = "%" Then
                                    yeff.Add(ceff.y(i) / 100)
                                Else
                                    yeff.Add(ceff.y(i))
                                End If
                            End If
                        Next

                        'get operating points
                        Dim head, eff, power As Double

                        If datapair.Value("HEAD").Enabled And datapair.Value("HEAD").x.Count > 0 Then
                            head = Interpolation.Interpolate(xhead.ToArray(GetType(Double)), yhead.ToArray(GetType(Double)), qint)
                            LHeadSpeed.Add(datapair.Key)
                            LHead.Add(head)
                        End If

                        If datapair.Value("POWER").Enabled And datapair.Value("POWER").x.Count > 0 Then
                            power = Interpolation.Interpolate(xpower.ToArray(GetType(Double)), ypower.ToArray(GetType(Double)), qint)
                            LPowerSpeed.Add(datapair.Key)
                            LPower.Add(power)
                        End If

                        If datapair.Value("EFF").Enabled And datapair.Value("EFF").x.Count > 0 Then
                            eff = Interpolation.Interpolate(xeff.ToArray(GetType(Double)), yeff.ToArray(GetType(Double)), qint)
                            LEffSpeed.Add(datapair.Key)
                            LEff.Add(eff)
                        End If

                    Next

                    Dim ires As Double

                    If LHead.Count > 0 Then
                        ' head has priority over power
                        ires = Interpolation.Interpolate(LHeadSpeed.ToArray, LHead.ToArray(), Speed)
                        Me.CurvePower = Double.NegativeInfinity
                        Me.CurveHead = ires
                    Else
                        'power
                        ires = Interpolation.Interpolate(LPowerSpeed.ToArray, LPower.ToArray(), Speed)
                        Me.CurveHead = Double.NegativeInfinity
                        Me.CurvePower = ires
                    End If

                    If LEff.Count > 0 Then
                        'efficiency
                        ires = Interpolation.Interpolate(LEffSpeed.ToArray, LEff.ToArray(), Speed)
                        Me.CurveEff = ires * 100
                    Else
                        Me.CurveEff = Double.NegativeInfinity
                    End If

                    If CurvePower = Double.NegativeInfinity Then
                        If ProcessPath = ProcessPathType.Adiabatic Then
                            AdiabaticHead = CurveHead
                        Else
                            PolytropicHead = CurveHead
                        End If
                    Else
                        If ProcessPath = ProcessPathType.Adiabatic Then
                            AdiabaticHead = CurvePower * 1000 * Wi / 9.8
                        Else
                            PolytropicHead = CurvePower * 1000 * Wi / 9.8
                        End If
                    End If

                    If Not CurveEff = Double.NegativeInfinity Then
                        If ProcessPath = ProcessPathType.Adiabatic Then
                            AdiabaticEfficiency = CurveEff
                        Else
                            PolytropicEfficiency = CurveEff
                        End If
                    End If

                    GoTo Curves

            End Select

            If DebugMode Then AppendDebugLine("Calculation finished successfully.")

            IObj?.Close()

        End Sub

        Public Overrides Sub DeCalculate()

            'Zerar valores da corrente de materia conectada a jusante
            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                Dim msj As MaterialStream = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                With msj
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(0).Properties.massfraction = 1
                    Dim comp As BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.molarflow = Nothing
                End With

            End If

            'energy stream - update energy flow value (kW)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name), Streams.EnergyStream)
                    .EnergyFlow = Nothing
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

                If prop.Contains("PROP_") Then

                    Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                    Select Case propidx

                        Case 0
                            'PROP_CO_0	Pressure Increase (Head)
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP)
                        Case 1
                            'PROP_CO_1(Efficiency)
                            value = Me.AdiabaticEfficiency
                        Case 2
                            'PROP_CO_2(Delta - T)
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT)
                        Case 3
                            'PROP_CO_3	Power Required
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ)
                        Case 4
                            'PROP_CO_4	Pressure Out
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.POut)

                    End Select

                    Return value

                Else

                    Select Case prop
                        Case "AdiabaticHead"
                            Return SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.AdiabaticHead)
                        Case "PolytropicHead"
                            Return SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.PolytropicHead)
                        Case "AdiabaticCoefficient"
                            Return AdiabaticCoefficient
                        Case "PolytropicCoefficient"
                            Return PolytropicCoefficient
                        Case "PolytropicEfficiency"
                            Return PolytropicEfficiency
                        Case "RotationSpeed"
                            Return Speed
                    End Select

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
                    For i = 2 To 3
                        proplist.Add("PROP_CO_" + CStr(i))
                    Next
                    proplist.Add("AdiabaticCoefficient")
                    proplist.Add("PolytropicCoefficient")
                    proplist.Add("RotationSpeed")
                Case PropertyType.RW
                    For i = 0 To 4
                        proplist.Add("PROP_CO_" + CStr(i))
                    Next
                    proplist.Add("PolytropicEfficiency")
                    proplist.Add("AdiabaticCoefficient")
                    proplist.Add("PolytropicCoefficient")
                    proplist.Add("AdiabaticHead")
                    proplist.Add("PolytropicHead")
                    proplist.Add("RotationSpeed")
                Case PropertyType.WR
                    For i = 0 To 1
                        proplist.Add("PROP_CO_" + CStr(i))
                    Next
                    proplist.Add("PROP_CO_4")
                    proplist.Add("AdiabaticHead")
                    proplist.Add("PolytropicHead")
                    proplist.Add("RotationSpeed")
                Case PropertyType.ALL
                    For i = 0 To 4
                        proplist.Add("PROP_CO_" + CStr(i))
                    Next
                    proplist.Add("PolytropicEfficiency")
                    proplist.Add("AdiabaticCoefficient")
                    proplist.Add("PolytropicCoefficient")
                    proplist.Add("AdiabaticHead")
                    proplist.Add("PolytropicHead")
                    proplist.Add("RotationSpeed")
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter

            If prop.Contains("PROP_") Then

                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx
                    Case 0
                        'PROP_CO_0	Pressure Increase (Head)
                        Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                    Case 1
                        'PROP_CO_1(Efficiency)
                        Me.AdiabaticEfficiency = propval
                    Case 4
                        'PROP_CO_4(Pressure Out)
                        Me.POut = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                End Select

            Else

                Select Case prop
                    Case "AdiabaticHead"
                        AdiabaticHead = SystemsOfUnits.Converter.ConvertToSI(su.distance, propval)
                    Case "PolytropicHead"
                        PolytropicHead = SystemsOfUnits.Converter.ConvertToSI(su.distance, propval)
                    Case "PolytropicEfficiency"
                        PolytropicEfficiency = propval
                    Case "RotationSpeed"
                        Speed = propval
                End Select

            End If

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

                If prop.Contains("PROP_CO") Then

                    Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                    Select Case propidx

                        Case 0
                            'PROP_CO_0	Pressure Increase (Head)
                            value = su.deltaP
                        Case 1
                            'PROP_CO_1(Efficiency)
                            value = "%"
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

                Else

                    If prop.Contains("Head") Then

                        Return su.distance

                    ElseIf prop.Contains("Efficiency") Then

                        Return "%"

                    ElseIf prop.Contains("Speed") Then

                        Return "rpm"

                    Else

                        Return ""

                    End If

                End If

            End If

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_ComprExpndr With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ComprExpndr With {.SimObject = Me}
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
            Return My.Resources.uo_compr_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("COMP_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("COMP_Name")
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

            str.AppendLine("Compressor: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Inlet Conditions")
            str.AppendLine()
            str.AppendLine("    Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, istr.Phases(0).Properties.temperature).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, istr.Phases(0).Properties.pressure).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Mass Flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.massflow, istr.Phases(0).Properties.massflow).ToString(numberformat, ci) & " " & su.massflow)
            str.AppendLine("    Volumetric Flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, istr.Phases(0).Properties.volumetric_flow).ToString(numberformat, ci) & " " & su.volumetricFlow)
            str.AppendLine("    Vapor Fraction: " & istr.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine("    Compounds: " & istr.PropertyPackage.RET_VNAMES.ToArrayString)
            str.AppendLine("    Molar Composition: " & istr.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString(ci))
            str.AppendLine()
            str.AppendLine("Calculation Parameters")
            str.AppendLine()
            str.AppendLine("    Calculation Mode: " & CalcMode.ToString)
            str.AppendLine("    Thermodynamic Path: " & ProcessPath.ToString)
            Select Case CalcMode
                Case CalculationMode.Delta_P
                    str.AppendLine("    Pressure Increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Convert.ToDouble(DeltaP)).ToString(numberformat, ci) & " " & su.deltaP)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Outlet Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Convert.ToDouble(POut)).ToString(numberformat, ci) & " " & su.pressure)
                Case CalculationMode.PowerRequired, CalculationMode.EnergyStream
                    str.AppendLine("   Power Required: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Convert.ToDouble(DeltaQ)).ToString(numberformat, ci) & " " & su.heatflow)
                Case CalculationMode.Head
                    Select Case ProcessPath
                        Case ProcessPathType.Adiabatic
                            str.AppendLine("    Specified Head: " & SystemsOfUnits.Converter.ConvertFromSI(su.distance, Convert.ToDouble(AdiabaticHead)).ToString(numberformat, ci) & " " & su.distance)
                        Case ProcessPathType.Polytropic
                            str.AppendLine("    Specified Head: " & SystemsOfUnits.Converter.ConvertFromSI(su.distance, Convert.ToDouble(PolytropicHead)).ToString(numberformat, ci) & " " & su.distance)
                    End Select
                Case CalculationMode.Curves
                    str.AppendLine("    Rotation Speed: " & Convert.ToDouble(Speed).ToString(numberformat, ci))
            End Select
            str.AppendLine("    Adiabatic Efficiency: " & Convert.ToDouble(AdiabaticEfficiency).ToString(numberformat, ci))
            str.AppendLine("    Polytropic Efficiency: " & Convert.ToDouble(PolytropicEfficiency).ToString(numberformat, ci))
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            str.AppendLine("    Outlet Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Convert.ToDouble(POut)).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Pressure Increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Convert.ToDouble(DeltaP)).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine("    Adiabatic Coefficient: " & Convert.ToDouble(AdiabaticCoefficient).ToString(numberformat, ci))
            str.AppendLine("    Polytropic Coefficient: " & Convert.ToDouble(PolytropicCoefficient).ToString(numberformat, ci))
            str.AppendLine("    Temperature Change: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, DeltaT).ToString(numberformat, ci) & " " & su.deltaT)
            str.AppendLine("    Power Generated: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Convert.ToDouble(DeltaQ)).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine("    Adiabatic Head: " & SystemsOfUnits.Converter.ConvertFromSI(su.distance, Convert.ToDouble(AdiabaticHead)).ToString(numberformat, ci) & " " & su.distance)
            str.AppendLine("    Polytropic Head: " & SystemsOfUnits.Converter.ConvertFromSI(su.distance, Convert.ToDouble(PolytropicHead)).ToString(numberformat, ci) & " " & su.distance)

            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Compressor '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Calculation Mode",
                    CalcMode.ToString}))

            Select Case CalcMode
                Case CalculationMode.Delta_P
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Increase",
                            Me.DeltaP.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
                Case CalculationMode.OutletPressure
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.POut.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                Case CalculationMode.PowerRequired, CalculationMode.EnergyStream
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Power Required",
                            Me.DeltaQ.ConvertFromSI(su.heatflow).ToString(nf),
                            su.heatflow}))
                Case CalculationMode.Head
                    If ProcessPath = ProcessPathType.Adiabatic Then
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Compressor Head",
                            Me.AdiabaticHead.ConvertFromSI(su.distance).ToString(nf),
                            su.distance}))
                    Else
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Compressor Head",
                            Me.PolytropicHead.ConvertFromSI(su.distance).ToString(nf),
                            su.distance}))
                    End If
                Case CalculationMode.Curves
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                                    New String() {"Rotation Speed",
                                    Me.Speed.ToString(nf),
                                    "rpm"}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Thermodynamic Path",
                    ProcessPath.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Adiabatic Efficiency",
                            Me.AdiabaticEfficiency.ToString(nf),
                            "%"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Polytropic Efficiency",
                            Me.PolytropicEfficiency.ToString(nf),
                            "%"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Outlet Pressure",
                    Me.POut.ConvertFromSI(su.pressure).ToString(nf),
                    su.pressure}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Increase",
                            Me.DeltaP.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Adiabatic Coefficient",
                            Me.AdiabaticCoefficient.ToString(nf),
                            ""}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Polytropic Coefficient",
                            Me.PolytropicCoefficient.ToString(nf),
                            ""}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Temperature Change",
                            Me.DeltaT.ConvertFromSI(su.deltaT).ToString(nf),
                            su.deltaT}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Power Required",
                            Me.DeltaQ.ConvertFromSI(su.heatflow).ToString(nf),
                            su.heatflow}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Adiabatic Head",
                            Me.AdiabaticHead.ConvertFromSI(su.distance).ToString(nf),
                            su.distance}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Polytropic Head",
                            Me.PolytropicHead.ConvertFromSI(su.distance).ToString(nf),
                            su.distance}))

            Return list

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Calculation Mode") Then
                Return "Select the variable to specify for the calculation of the Compressor."
            ElseIf p.Equals("Pressure Increase") Then
                Return "If you chose the 'Pressure Variation' calculation mode, enter the desired value for the pressure increase."
            ElseIf p.Equals("Outlet Pressure") Then
                Return "If you chose the 'Outlet Pressure' calculation mode, enter the desired outlet pressure."
            ElseIf p.Equals("Power Required") Then
                Return "If you chose the 'Power Required' calculation mode, enter the desired required compressor power."
            ElseIf p.Equals("Adiabatic Efficiency (%)") Then
                Return "Enter the isentropic efficiency of the compressor, if the Thermodynamic Path is Adiabatic."
            ElseIf p.Equals("Polytropic Efficiency (%)") Then
                Return "Enter the polytropic efficiency of the compressor, if the Thermodynamic Path is Polytropic."
            ElseIf p.Equals("Adiabatic Head") Then
                Return "If you chose the 'Known Head' calculation mode and the thermo path is Adiabatic, enter the compressor's Adiabatic Head."
            ElseIf p.Equals("Polytropic Head") Then
                Return "If you chose the 'Known Head' calculation mode and the thermo path is Polytropic, enter the compressor's Polytropic Head."
            ElseIf p.Equals("Thermodynamic Path") Then
                Return "Select the Thermodynamic Path according to the available data."
            ElseIf p.Equals("Rotation Speed") Then
                Return "Enter the Rotation Speed of the Equipment in rpm."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace