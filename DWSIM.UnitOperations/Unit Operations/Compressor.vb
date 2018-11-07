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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums

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
        End Enum

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)
        Protected m_POut As Nullable(Of Double) = 101325.0#
        Protected m_cmode As CalculationMode = CalculationMode.Delta_P

        Protected m_eta_a As Nullable(Of Double) = 75.0#
        Protected m_eta_p As Nullable(Of Double) = Nothing

        Protected m_ignorephase As Boolean = True

        Public Property OutletTemperature As Double = 0.0#

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

            IObj?.Paragraphs.Add("• Power required by the compressor:")

            IObj?.Paragraphs.Add("<m>Pot=\frac{W(H_{2_{id}}-H_{1})}{\eta},</m>")

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

            If Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Dim Ti, Pi, Hi, Si, Wi, rho_vi, qvi, qli, ei, ein, T2, P2, P2i, Qi, H2, cpig, cp, cv, mw, fx, fx0, fx00, P2i0, P2i00 As Double

            Dim msin, msout As MaterialStream, esin As Streams.EnergyStream

            msin = GetInletMaterialStream(0)
            msout = GetOutletMaterialStream(0)
            esin = GetInletEnergyStream(1)

            qli = msin.Phases(1).Properties.volumetric_flow.GetValueOrDefault.ToString

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            IObj?.Paragraphs.Add("Calculation Mode: " & CalcMode.ToString)

            If Not IgnorePhase And DebugMode Then AppendDebugLine("Checking if there is a liquid phase in the inlet stream...")

            If qli > 0 And Not Me.IgnorePhase Then Throw New Exception(FlowSheet.GetTranslatedString("ExisteumaPhaselquidan"))

            If FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name).GraphicObject.Active Then

                If Me.CalcMode = CalculationMode.Delta_P Or Me.CalcMode = CalculationMode.OutletPressure Then GoTo fix

                Me.PropertyPackage.CurrentMaterialStream = msin
                Ti = msin.Phases(0).Properties.temperature.GetValueOrDefault.ToString
                Pi = msin.Phases(0).Properties.pressure.GetValueOrDefault.ToString
                rho_vi = msin.Phases(2).Properties.density.GetValueOrDefault.ToString
                IObj?.SetCurrent()
                cpig = Me.PropertyPackage.AUX_CPm(PhaseName.Vapor, Ti)
                cp = msin.Phases(0).Properties.heatCapacityCp.GetValueOrDefault
                cv = msin.Phases(0).Properties.heatCapacityCv.GetValueOrDefault
                mw = msin.Phases(0).Properties.molecularWeight.GetValueOrDefault
                qvi = msin.Phases(2).Properties.volumetric_flow.GetValueOrDefault.ToString
                Hi = msin.Phases(0).Properties.enthalpy.GetValueOrDefault.ToString
                Si = msin.Phases(0).Properties.entropy.GetValueOrDefault.ToString
                Wi = msin.Phases(0).Properties.massflow.GetValueOrDefault.ToString
                ei = Hi * Wi
                ein = ei

                IObj?.Paragraphs.Add("<h3>Input Variables</h3>")

                IObj?.Paragraphs.Add(String.Format("<mi>W</mi>: {0} kg/s", Wi))
                IObj?.Paragraphs.Add(String.Format("<mi>P_1</mi>: {0} Pa", Pi))
                IObj?.Paragraphs.Add(String.Format("<mi>H_1</mi>: {0} kJ/kg", Hi))
                IObj?.Paragraphs.Add(String.Format("<mi>S_1</mi>: {0} kJ/[kg.K]", Si))
                IObj?.Paragraphs.Add(String.Format("<mi>\eta</mi>: {0} %", EficienciaAdiabatica.GetValueOrDefault))

                If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Name))
                If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, S = {3} kJ/[kg.K], W = {4} kg/s, cp = {5} kJ/[kg.K]", Ti, Pi, Hi, Si, Wi, cp))

                Dim energystream As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)

                Select Case Me.CalcMode
                    Case CalculationMode.EnergyStream
                        With energystream
                            Me.DeltaQ = .EnergyFlow.GetValueOrDefault 'Wi * (H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100)
                        End With
                        If DebugMode Then AppendDebugLine(String.Format("Power from energy stream: {0} kW", DeltaQ))
                    Case CalculationMode.PowerRequired
                        If DebugMode Then AppendDebugLine(String.Format("Power from definition: {0} kW", DeltaQ))
                End Select

                CheckSpec(Me.DeltaQ, True, "power")

                With esin
                    .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                    .GraphicObject.Calculated = True
                End With

                Dim k As Double = cp / cv

                P2i = Pi * ((1 + DeltaQ.GetValueOrDefault * (Me.EficienciaAdiabatica.GetValueOrDefault / 100) / Wi * (k - 1) / k * mw / 8.314 / Ti)) ^ (k / (k - 1))

                Dim tmp As IFlashCalculationResult

                Dim icnt As Integer = 0

                'recalculate Q with P2i

                Do

                    IObj?.SetCurrent()
                    tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEntropy, P2i, Si, 0)
                    Qi = Wi * (tmp.CalculatedEnthalpy - Hi) / (Me.EficienciaAdiabatica.GetValueOrDefault / 100)

                    If DebugMode Then AppendDebugLine(String.Format("Qi: {0}", Qi))

                    fx00 = fx0
                    fx0 = fx
                    fx = Qi - DeltaQ

                    P2i00 = P2i0
                    P2i0 = P2i

                    If icnt <= 2 Then
                        P2i *= 1.01
                    Else
                        P2i = P2i - fx / ((fx - fx00) / (P2i - P2i00))
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("P2i: {0}", P2i))

                    icnt += 1

                Loop Until Math.Abs((DeltaQ.GetValueOrDefault - Qi) / DeltaQ.GetValueOrDefault) < 0.001

                P2 = P2i

                If DebugMode Then AppendDebugLine(String.Format("Calculated outlet pressure: {0} Pa", P2))

                CheckSpec(P2, True, "outlet pressure")

                DeltaP = P2 - Pi
                POut = P2

                CheckSpec(Si, False, "inlet entropy")

                If DebugMode Then AppendDebugLine(String.Format("Doing a PS flash to calculate ideal outlet enthalpy... P = {0} Pa, S = {1} kJ/[kg.K]", P2, Si))

                IObj?.SetCurrent()
                tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEntropy, P2, Si, 0)

                IObj?.Paragraphs.Add("<h3>Results</h3>")

                IObj?.Paragraphs.Add("<mi>S_{2,id}</mi>: " & String.Format("{0} kJ/[kg.K]", tmp.CalculatedEntropy))
                IObj?.Paragraphs.Add("<mi>T_{2,id}</mi>: " & String.Format("{0} K", tmp.CalculatedTemperature))
                IObj?.Paragraphs.Add("<mi>H_{2,id}</mi>: " & String.Format("{0} kJ/kg", tmp.CalculatedEnthalpy))

                If DebugMode Then AppendDebugLine(String.Format("Calculated ideal outlet enthalpy Hid = {0} kJ/kg", tmp.CalculatedEnthalpy))

                H2 = Hi + (tmp.CalculatedEnthalpy - Hi) / (Me.EficienciaAdiabatica.GetValueOrDefault / 100)

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
                        .Phases(0).Properties.massflow = msin.Phases(0).Properties.massflow.GetValueOrDefault
                    End With

                End If

            Else

fix:            Me.PropertyPackage.CurrentMaterialStream = msin
                Ti = msin.Phases(0).Properties.temperature.GetValueOrDefault.ToString
                Pi = msin.Phases(0).Properties.pressure.GetValueOrDefault.ToString
                rho_vi = msin.Phases(2).Properties.density.GetValueOrDefault.ToString
                qvi = msin.Phases(2).Properties.volumetric_flow.GetValueOrDefault.ToString
                Hi = msin.Phases(0).Properties.enthalpy.GetValueOrDefault.ToString
                Si = msin.Phases(0).Properties.entropy.GetValueOrDefault.ToString
                Wi = msin.Phases(0).Properties.massflow.GetValueOrDefault.ToString
                ei = Hi * Wi
                ein = ei

                IObj?.Paragraphs.Add("<h3>Input Variables</h3>")

                IObj?.Paragraphs.Add(String.Format("<mi>W</mi>: {0} kg/s", Wi))
                IObj?.Paragraphs.Add(String.Format("<mi>P_1</mi>: {0} Pa", Pi))
                IObj?.Paragraphs.Add(String.Format("<mi>H_1</mi>: {0} kJ/kg", Hi))
                IObj?.Paragraphs.Add(String.Format("<mi>S_1</mi>: {0} kJ/[kg.K]", Si))
                IObj?.Paragraphs.Add(String.Format("<mi>\eta</mi>: {0} %", EficienciaAdiabatica.GetValueOrDefault))

                If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Name))
                If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, S = {3} kJ/[kg.K], W = {4} kg/s", Ti, Pi, Hi, Si, Wi, cp))

                Me.PropertyPackage.CurrentMaterialStream = msin

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

                IObj?.SetCurrent()
                Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEntropy, P2, Si, 0)
                T2 = tmp.CalculatedTemperature
                H2 = tmp.CalculatedEnthalpy

                IObj?.Paragraphs.Add("<h3>Results</h3>")

                IObj?.Paragraphs.Add("<mi>S_{2,id}</mi>: " & String.Format("{0} kJ/[kg.K]", tmp.CalculatedEntropy))
                IObj?.Paragraphs.Add("<mi>T_{2,id}</mi>: " & String.Format("{0} K", T2))
                IObj?.Paragraphs.Add("<mi>H_{2,id}</mi>: " & String.Format("{0} kJ/kg", H2))

                If DebugMode Then AppendDebugLine(String.Format("Calculated ideal outlet enthalpy Hid = {0} kJ/kg", tmp.CalculatedEnthalpy))

                CheckSpec(T2, True, "outlet temperature")
                CheckSpec(H2, False, "outlet enthalpy")

                Me.DeltaQ = Wi * (H2 - Hi) / (Me.EficienciaAdiabatica.GetValueOrDefault / 100)

                If DebugMode Then AppendDebugLine(String.Format("Calculated real compressor power = {0} kW", DeltaQ))

                CheckSpec(DeltaQ, True, "power")

                If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, Hi + Me.DeltaQ.GetValueOrDefault / Wi))

                IObj?.SetCurrent()
                tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, Hi + Me.DeltaQ.GetValueOrDefault / Wi, T2)
                T2 = tmp.CalculatedTemperature
                Me.DeltaT = T2 - Ti

                If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                CheckSpec(T2, True, "outlet temperature")

                H2 = Hi + Me.DeltaQ.GetValueOrDefault / Wi

                OutletTemperature = T2

                IObj?.Paragraphs.Add(String.Format("<mi>P_2</mi>: {0} Pa", P2))
                IObj?.Paragraphs.Add(String.Format("<mi>H_2</mi>: {0} kJ/kg", H2))
                IObj?.Paragraphs.Add(String.Format("<mi>S_2</mi>: {0} kJ/[kg.K]", tmp.CalculatedEntropy))
                IObj?.Paragraphs.Add(String.Format("<mi>T_2</mi>: {0} K", T2))

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
                        .Phases(0).Properties.massflow = msin.Phases(0).Properties.massflow.GetValueOrDefault
                    End With

                    'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
                    With esin
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                End If

            End If

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

            'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
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

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

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
            str.AppendLine("Inlet conditions")
            str.AppendLine()
            str.AppendLine("    Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, istr.Phases(0).Properties.temperature.GetValueOrDefault).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, istr.Phases(0).Properties.pressure.GetValueOrDefault).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Mass flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.massflow, istr.Phases(0).Properties.massflow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.massflow)
            str.AppendLine("    Volumetric flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, istr.Phases(0).Properties.volumetric_flow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.volumetricFlow)
            str.AppendLine("    Vapor fraction: " & istr.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine("    Compounds: " & istr.PropertyPackage.RET_VNAMES.ToArrayString)
            str.AppendLine("    Molar composition: " & istr.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString(ci))
            str.AppendLine()
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Calculation mode: " & CalcMode.ToString)
            Select Case CalcMode
                Case CalculationMode.Delta_P
                    str.AppendLine("    Pressure increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Convert.ToDouble(DeltaP.GetValueOrDefault)).ToString(numberformat, ci) & " " & su.deltaP)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Convert.ToDouble(POut.GetValueOrDefault)).ToString(numberformat, ci) & " " & su.pressure)
                Case CalculationMode.PowerRequired, CalculationMode.EnergyStream
                    str.AppendLine("   Power Required: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Convert.ToDouble(DeltaQ.GetValueOrDefault)).ToString(numberformat, ci) & " " & su.heatflow)
            End Select
            str.AppendLine("    Efficiency: " & Convert.ToDouble(EficienciaAdiabatica).ToString(numberformat, ci))
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case CalcMode
                Case CalculationMode.Delta_P
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Convert.ToDouble(POut.GetValueOrDefault)).ToString(numberformat, ci) & " " & su.pressure)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Pressure increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Convert.ToDouble(DeltaP.GetValueOrDefault)).ToString(numberformat, ci) & " " & su.deltaP)
            End Select
            str.AppendLine("    Temperature increase/decrease: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, DeltaT.GetValueOrDefault).ToString(numberformat, ci) & " " & su.deltaT)
            str.AppendLine("    Power required: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Convert.ToDouble(DeltaQ.GetValueOrDefault)).ToString(numberformat, ci) & " " & su.heatflow)

            Return str.ToString

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
            ElseIf p.Equals("Efficiency (%)") Then
                Return "Enter the isentropic efficiency of the compressor. 100% efficiency means a totally isentropic process."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace