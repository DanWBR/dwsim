'    Centrifugal Pump Calculation Routines 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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
Imports DWSIM.MathOps.MathEx.Interpolation

Namespace UnitOperations.Auxiliary.PumpOps

    Public Enum CurveType
        Head
        Power
        NPSHr
        Efficiency
        SystemHead
        None
    End Enum

    Public Class CurveSet

        Implements Interfaces.ICustomXMLSerialization

        Public Property Name As String = "MyCurveSet"

        Public Property Description As String = "MyCurveSetDescription"

        Property ImpellerDiameter As Double = 200.0

        Property ImpellerSpeed As Double = 1450.0

        Property ImpellerDiameterUnit As String = "mm"

        Property CurveHead As New Curve

        Property CurvePower As New Curve

        Property CurveEfficiency As New Curve

        Property CurveNPSHr As New Curve

        Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me)
        End Function

        Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
        End Function

    End Class

    <System.Serializable()> Public Class Curve

        Implements Interfaces.ICustomXMLSerialization

        Public Property X As New List(Of Double)
        Public Property Y As New List(Of Double)

        Protected _xunit As String = ""
        Protected _yunit As String = ""
        Protected _type As CurveType
        Protected _id As String = ""
        Protected _name As String = ""
        Protected _enabled As Boolean = False

        Sub New(ByVal id As String, ByVal name As String, ByVal x As List(Of Double), ByVal xunit As String, ByVal y As List(Of Double), ByVal yunit As String, ByVal type As CurveType)
            Me.New()
            Me.ID = id
            Me.Name = name
            Me.x = x
            Me.y = y
            Me.xunit = xunit
            Me.yunit = yunit
            Me.CvType = type
        End Sub

        Sub New(ByVal id As String, ByVal name As String, ByVal type As CurveType)
            Me.New()
            Me.ID = id
            Me.Name = name
            Me.CvType = type
        End Sub

        Sub New()
            _X = New List(Of Double)
            _Y = New List(Of Double)
        End Sub

        Public Property Enabled() As Boolean
            Get
                Return _enabled
            End Get
            Set(ByVal value As Boolean)
                _enabled = value
            End Set
        End Property

        Public Property Name() As String
            Get
                Return _name
            End Get
            Set(ByVal value As String)
                _name = value
            End Set
        End Property

        Public Property ID() As String
            Get
                Return _id
            End Get
            Set(ByVal value As String)
                _id = value
            End Set
        End Property

        Public Property CvType() As CurveType
            Get
                Return _type
            End Get
            Set(ByVal value As CurveType)
                _type = value
            End Set
        End Property

        Public Property yunit() As String
            Get
                Return _yunit
            End Get
            Set(ByVal value As String)
                _yunit = value
            End Set
        End Property

        Public Property xunit() As String
            Get
                Return _xunit
            End Get
            Set(ByVal value As String)
                _xunit = value
            End Set
        End Property

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            Try
                Dim xel As XElement = (From xmlprop In data Select xmlprop Where xmlprop.Name = "x").FirstOrDefault
                Dim val As ArrayList = XMLSerializer.XMLSerializer.StringToArray(xel.Value, System.Globalization.CultureInfo.InvariantCulture)
                X = val.ToDoubleList()
                xel = (From xmlprop In data Select xmlprop Where xmlprop.Name = "y").FirstOrDefault
                val = XMLSerializer.XMLSerializer.StringToArray(xel.Value, System.Globalization.CultureInfo.InvariantCulture)
                Y = val.ToDoubleList()
            Catch ex As Exception
            End Try

            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

    End Class

End Namespace

Namespace UnitOperations

    <System.Serializable()> Public Class Pump

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.PressureChangers

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_Pump

        Public Enum CalculationMode
            Delta_P = 0
            OutletPressure = 1
            EnergyStream = 2
            Curves = 3
            Power = 4
        End Enum

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)

        Protected m_eta As Nullable(Of Double) = 75
        Protected m_npsh As Nullable(Of Double)
        Protected m_pout As Double = 0

        Protected m_ignorephase As Boolean = True

        Protected m_FixOnDeltaP As Boolean = True
        Protected m_cmode As CalculationMode = CalculationMode.Delta_P

        Property PumpType As String = ""

        Protected _curvehead As Double
        Protected _curveeff As Double
        Protected _curvenpsh As Double
        Protected _curvepower As Double
        Protected _curvesyshead As Double
        Protected _curveflow As Double

        Property OutletTemperature As Double = 298.15#

        Public Property PumpCurveSet As New PumpOps.CurveSet

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Pump()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Pump)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Curves").Elements.ToList
                Dim cv As New PumpOps.Curve()
                cv.LoadData(xel.Elements.ToList)
                Select Case cv.Name
                    Case "POWER"
                        PumpCurveSet.CurvePower = cv
                    Case "EFF"
                        PumpCurveSet.CurveEfficiency = cv
                    Case "NPSH"
                        PumpCurveSet.CurveNPSHr = cv
                    Case "HEAD"
                        PumpCurveSet.CurveHead = cv
                End Select
            Next

            Dim xid = data.Where(Function(d) d.Name = "ImpellerDiameter").FirstOrDefault()
            If xid IsNot Nothing Then
                PumpCurveSet.ImpellerDiameter = xid.Value.ToDoubleFromInvariant()
            End If
            Dim xir = data.Where(Function(d) d.Name = "ImpellerSpeed").FirstOrDefault()
            If xir IsNot Nothing Then
                PumpCurveSet.ImpellerSpeed = xir.Value.ToDoubleFromInvariant()
            End If
            Dim xidu = data.Where(Function(d) d.Name = "DiameterUnit").FirstOrDefault()
            If xidu IsNot Nothing Then
                PumpCurveSet.ImpellerDiameterUnit = xidu.Value
            End If

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        Public Property CurveFlow() As Double
            Get
                Return _curveflow
            End Get
            Set(ByVal value As Double)
                _curveflow = value
            End Set
        End Property

        Public Property CurveSysHead() As Double
            Get
                Return _curvesyshead
            End Get
            Set(ByVal value As Double)
                _curvesyshead = value
            End Set
        End Property

        Public Property CurvePower() As Double
            Get
                Return _curvepower
            End Get
            Set(ByVal value As Double)
                _curvepower = value
            End Set
        End Property

        Public Property CurveNPSHr() As Double
            Get
                Return _curvenpsh
            End Get
            Set(ByVal value As Double)
                _curvenpsh = value
            End Set
        End Property

        Public Property CurveEff() As Double
            Get
                Return _curveeff
            End Get
            Set(ByVal value As Double)
                _curveeff = value
            End Set
        End Property

        Public Property CurveHead() As Double
            Get
                Return _curvehead
            End Get
            Set(ByVal value As Double)
                _curvehead = value
            End Set
        End Property

        Public Property Pout() As Double
            Get
                Return m_pout
            End Get
            Set(ByVal value As Double)
                m_pout = value
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

        Public Property FixOnDeltaP() As Boolean
            Get
                Return m_FixOnDeltaP
            End Get
            Set(ByVal value As Boolean)
                m_FixOnDeltaP = value
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

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Property NPSH() As Nullable(Of Double)
            Get
                Return m_npsh
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_npsh = value
            End Set
        End Property

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

        Public Property Head As Double

        Public Overrides Sub RunDynamicModel()

            Calculate()

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The calculation method for the pump is different for the two 
                            cases (when the provided delta-p or the potency of the energy 
                            stream is used). In the first method, we have the following 
                            sequence:")

            IObj?.Paragraphs.Add("• Outlet stream enthalpy:")

            IObj?.Paragraphs.Add("<m>H_{2}=H_{1}+\frac{\Delta P}{\rho},</m>")

            IObj?.Paragraphs.Add("• Pump discharge pressure:")

            IObj?.Paragraphs.Add("<m>P_{2}=P_{1}+\Delta P</m>")

            IObj?.Paragraphs.Add("• Pump required power:")

            IObj?.Paragraphs.Add("<m>Pot=\frac{W(H_{2}-H_{1})}{\eta},</m>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>Pot</mi> pump power")

            IObj?.Paragraphs.Add("<mi>W</mi> mass flow")

            IObj?.Paragraphs.Add("<mi>H_{2}</mi> outlet stream specific enthalpy")

            IObj?.Paragraphs.Add("<mi>H_{1}</mi> inlet stream specific enthalpy")

            IObj?.Paragraphs.Add("<mi>\eta</mi> pump efficiency")

            IObj?.Paragraphs.Add("• Outlet temperature: PH Flash (with P2 and H2).")

            IObj?.Paragraphs.Add("In the second case (calculated outlet pressure), we have the 
                                following sequence:")

            IObj?.Paragraphs.Add("• Outlet stream enthalpy:")

            IObj?.Paragraphs.Add("<m>H_{2}=H_{1}+\frac{Pot\,\eta}{W},</m>")

            IObj?.Paragraphs.Add("• <mi>\Delta P</mi>:")

            IObj?.Paragraphs.Add("<m>\Delta P=\rho(H_{2}-H_{1}),</m>")

            IObj?.Paragraphs.Add("• Discharge pressure:")

            IObj?.Paragraphs.Add("<m>P_{2}=P_{1}+\Delta P</m>")

            IObj?.Paragraphs.Add("• Outlet temperature: PH Flash.")

            If args Is Nothing Then
                If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                    'Call function to calculate flowsheet
                    Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                    'Call function to calculate flowsheet
                    Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If
            End If

            Dim msin, msout As MaterialStream, esin As Streams.EnergyStream

            If args Is Nothing Then
                msin = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
                msout = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                esin = GetInletEnergyStream(1)
            Else
                msin = args(0)
                msout = args(1)
                esin = args(2)
            End If

            If msin.Phases(2).Properties.molarfraction.GetValueOrDefault() > 0.001 Then
                FlowSheet.ShowMessage(GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("Vapor phase detected in pump inlet"), IFlowsheet.MessageType.Warning)
            End If

            Me.PropertyPackage.CurrentMaterialStream = msin

            Me.PropertyPackage.CurrentMaterialStream.Validate()

            Dim Ti, Pi, Hi, Wi, rho_li, qli, ei, ein, T2, P2, H2 As Double

            Ti = msin.Phases(0).Properties.temperature.GetValueOrDefault
            Pi = msin.Phases(0).Properties.pressure.GetValueOrDefault
            rho_li = msin.Phases(1).Properties.density.GetValueOrDefault
            qli = msin.Phases(1).Properties.volumetric_flow.GetValueOrDefault
            Hi = msin.Phases(0).Properties.enthalpy.GetValueOrDefault
            Wi = msin.Phases(0).Properties.massflow.GetValueOrDefault
            ei = Hi * Wi
            ein = ei

            If qli = 0.0 Then
                DeltaT = 0.0
                DeltaQ = 0.0
                If CalcMode <> CalculationMode.EnergyStream And esin IsNot Nothing Then
                    esin.EnergyFlow = 0.0
                    If args Is Nothing Then esin.GraphicObject.Calculated = True
                End If
                If CalcMode <> CalculationMode.Delta_P Then
                    DeltaP = Pout - Pi
                End If
                If CalcMode <> CalculationMode.OutletPressure Then
                    Pout = Pi + DeltaP
                End If
                If Not DebugMode Then
                    With msout
                        .Phases(0).Properties.temperature = Ti
                        .Phases(0).Properties.pressure = Pout
                        .Phases(0).Properties.enthalpy = Hi
                        Dim comp As BaseClasses.Compound
                        Dim i As Integer = 0
                        For Each comp In .Phases(0).Compounds.Values
                            comp.MoleFraction = msin.Phases(0).Compounds(comp.Name).MoleFraction
                            comp.MassFraction = msin.Phases(0).Compounds(comp.Name).MassFraction
                            i += 1
                        Next
                        .Phases(0).Properties.massflow = msin.Phases(0).Properties.massflow.GetValueOrDefault
                        .DefinedFlow = FlowSpec.Mass
                        .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
                    End With
                Else
                    AppendDebugLine("Calculation finished successfully.")
                End If
                Exit Sub
            End If

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, W = {3} kg/s, Liquid Flow = {4} m3/s", Ti, Pi, Hi, Wi, qli))

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            IObj?.Paragraphs.Add("Calculation mode: " & CalcMode.ToString)

            IObj?.Paragraphs.Add("<h3>Input Variables</h3>")

            IObj?.Paragraphs.Add(String.Format("<mi>W</mi>: {0} kg/s", Wi))
            IObj?.Paragraphs.Add(String.Format("<mi>P_1</mi>: {0} Pa", Pi))
            IObj?.Paragraphs.Add(String.Format("<mi>H_1</mi>: {0} kJ/kg", Hi))
            IObj?.Paragraphs.Add(String.Format("<mi>S_1</mi>: {0} K", T2))
            IObj?.Paragraphs.Add(String.Format("<mi>\eta</mi>: {0} %", Eficiencia.GetValueOrDefault))

            Select Case Me.CalcMode

                Case CalculationMode.Curves

                    Dim cv As New SystemsOfUnits.Converter

                    Dim cnpsh, chead, ceff, cpower As PumpOps.Curve

                    cnpsh = Me.PumpCurveSet.CurveNPSHr
                    chead = Me.PumpCurveSet.CurveHead
                    ceff = Me.PumpCurveSet.CurveEfficiency
                    cpower = Me.PumpCurveSet.CurvePower

                    Dim xhead, yhead, xnpsh, ynpsh, xeff, yeff, xpower, ypower, xsystem, ysystem As New ArrayList

                    Dim i As Integer

                    For i = 0 To chead.x.Count - 1
                        If Double.TryParse(chead.x(i), New Double) And Double.TryParse(chead.y(i), New Double) Then
                            xhead.Add(SystemsOfUnits.Converter.ConvertToSI(chead.xunit, chead.x(i)))
                            yhead.Add(SystemsOfUnits.Converter.ConvertToSI(chead.yunit, chead.y(i)))
                        End If
                    Next
                    For i = 0 To cnpsh.x.Count - 1
                        If Double.TryParse(cnpsh.x(i), New Double) And Double.TryParse(cnpsh.y(i), New Double) Then
                            xnpsh.Add(SystemsOfUnits.Converter.ConvertToSI(cnpsh.xunit, cnpsh.x(i)))
                            ynpsh.Add(SystemsOfUnits.Converter.ConvertToSI(cnpsh.yunit, cnpsh.y(i)))
                        End If
                    Next
                    For i = 0 To ceff.x.Count - 1
                        If Double.TryParse(ceff.x(i), New Double) And Double.TryParse(ceff.y(i), New Double) Then
                            xeff.Add(SystemsOfUnits.Converter.ConvertToSI(ceff.xunit, ceff.x(i)))
                            If ceff.yunit = "%" Then
                                yeff.Add(ceff.y(i) / 100)
                            Else
                                yeff.Add(ceff.y(i))
                            End If
                        End If
                    Next
                    For i = 0 To cpower.x.Count - 1
                        If Double.TryParse(cpower.x(i), New Double) And Double.TryParse(cpower.y(i), New Double) Then
                            xpower.Add(SystemsOfUnits.Converter.ConvertToSI(cpower.xunit, cpower.x(i)))
                            ypower.Add(SystemsOfUnits.Converter.ConvertToSI(cpower.yunit, cpower.y(i)))
                        End If
                    Next

                    Dim w() As Double

                    If DebugMode Then AppendDebugLine(String.Format("Getting operating point..."))

                    'get operating points
                    Dim head, npshr, eff, power, syshead As Double

                    'head
                    ReDim w(xhead.Count)
                    ratinterpolation.buildfloaterhormannrationalinterpolant(xhead.ToArray(GetType(Double)), xhead.Count, 0.5, w)
                    head = polinterpolation.barycentricinterpolation(xhead.ToArray(GetType(Double)), yhead.ToArray(GetType(Double)), w, xhead.Count, qli)

                    If DebugMode Then AppendDebugLine(String.Format("Head: {0} m", head))

                    Me.CurveHead = head

                    'npshr
                    If Me.PumpCurveSet.CurveNPSHr.Enabled Then
                        ReDim w(xnpsh.Count)
                        ratinterpolation.buildfloaterhormannrationalinterpolant(xnpsh.ToArray(GetType(Double)), xnpsh.Count, 0.5, w)
                        npshr = polinterpolation.barycentricinterpolation(xnpsh.ToArray(GetType(Double)), ynpsh.ToArray(GetType(Double)), w, xnpsh.Count, qli)
                    Else
                        npshr = 0
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("NPSHr: {0} m", npshr))

                    Me.CurveNPSHr = npshr

                    'efficiency
                    If Me.PumpCurveSet.CurveEfficiency.Enabled Then
                        ReDim w(xeff.Count)
                        ratinterpolation.buildfloaterhormannrationalinterpolant(xeff.ToArray(GetType(Double)), xeff.Count, 0.5, w)
                        eff = polinterpolation.barycentricinterpolation(xeff.ToArray(GetType(Double)), yeff.ToArray(GetType(Double)), w, xeff.Count, qli)
                    Else
                        eff = Me.Eficiencia.GetValueOrDefault / 100
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("Efficiency: {0} %", eff * 100))

                    Me.CurveEff = eff * 100

                    If DebugMode Then AppendDebugLine(String.Format("System Head: {0} m", syshead))

                    'we need -> head, power, eff, to calculate P2, H2, T2

                    P2 = Pi + head * 9.81 * rho_li
                    CheckSpec(P2, True, "outlet pressure")

                    Me.DeltaP = P2 - Pi

                    Pout = P2

                    If DebugMode Then AppendDebugLine(String.Format("Outlet Pressure: {0} Pa", P2))

                    Dim tmp As IFlashCalculationResult

                    'power
                    If Me.PumpCurveSet.CurvePower.Enabled Then
                        ReDim w(xpower.Count)
                        ratinterpolation.buildfloaterhormannrationalinterpolant(xpower.ToArray(GetType(Double)), xpower.Count, 0.5, w)
                        power = polinterpolation.barycentricinterpolation(xpower.ToArray(GetType(Double)), ypower.ToArray(GetType(Double)), w, xpower.Count, qli)
                        H2 = Hi + power * eff / Wi
                    Else
                        power = Wi * 9.81 * syshead / eff / 1000
                        H2 = Hi + power * eff / Wi
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("Power: {0} kW", power))

                    CheckSpec(power, True, "power")

                    Me.CurvePower = power

                    Me.DeltaQ = power

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)
                    T2 = tmp.CalculatedTemperature

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    CheckSpec(T2, True, "outlet temperature")

                    Me.DeltaT = T2 - Ti

                    Me.Eficiencia = eff * 100

                    Try
                        IObj?.SetCurrent()
                        Dim Pbub = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.TemperatureVaporFraction, Ti, 0.0#, Pi).CalculatedPressure
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                    Me.CurveFlow = qli

                    If esin IsNot Nothing Then
                        'energy stream - update energy flow value (kW)
                        With esin
                            .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                            .GraphicObject.Calculated = True
                        End With
                    End If

                Case CalculationMode.EnergyStream

                    Dim tmp As IFlashCalculationResult

                    'Corrente de EnergyFlow - pegar valor do DH
                    If esin IsNot Nothing Then
                        With esin
                            Me.DeltaQ = .EnergyFlow.GetValueOrDefault 'Wi * (H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100)
                        End With
                    End If

                    H2 = Hi + Me.DeltaQ.GetValueOrDefault * (Me.Eficiencia.GetValueOrDefault / 100) / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    P2 = Pi + (H2 - Hi) * rho_li * 1000
                    CheckSpec(P2, True, "outlet pressure")

                    DeltaP = P2 - Pi

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    IObj?.SetCurrent()
                    If P2 / Pi > 10 Then
                        Dim P2it As Double = Pi + DeltaP / 10.0
                        While P2it <= P2
                            tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2it, H2, Ti)
                            T2 = tmp.CalculatedTemperature.GetValueOrDefault
                            P2it += DeltaP / 10.0
                        End While
                    Else
                        tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)
                        T2 = tmp.CalculatedTemperature.GetValueOrDefault
                    End If
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    Dim Pbub As Double '= Me.PropertyPackage.DW_CalcPVAP_ISOL(Ti)

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... T = {0} K, VF = 0", Ti))

                    Pout = P2

                    Try
                        IObj?.SetCurrent()
                        Pbub = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.TemperatureVaporFraction, Ti, 0.0#, Pi).CalculatedPressure
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                Case CalculationMode.Power

                    Dim tmp As IFlashCalculationResult

                    If esin IsNot Nothing Then
                        'Corrente de EnergyFlow - pegar valor do DH
                        With esin
                            .EnergyFlow = Me.DeltaQ
                        End With
                    End If

                    H2 = Hi + Me.DeltaQ.GetValueOrDefault * (Me.Eficiencia.GetValueOrDefault / 100) / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    P2 = Pi + (H2 - Hi) * rho_li * 1000
                    CheckSpec(P2, True, "outlet pressure")

                    DeltaP = P2 - Pi

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    IObj?.SetCurrent()
                    If P2 / Pi > 10 Then
                        Dim P2it As Double = Pi + DeltaP / 10.0
                        While P2it <= P2
                            tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2it, H2, Ti)
                            T2 = tmp.CalculatedTemperature.GetValueOrDefault
                            P2it += DeltaP / 10.0
                        End While
                    Else
                        tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)
                        T2 = tmp.CalculatedTemperature.GetValueOrDefault
                    End If
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... P = {0} Pa, VF = 0", P2))

                    Try
                        IObj?.SetCurrent()
                        Dim Pbub = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.TemperatureVaporFraction, Ti, 0.0#, Pi).CalculatedPressure
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                Case CalculationMode.Delta_P

                    Me.PropertyPackage.CurrentMaterialStream = msin
                    P2 = Pi + Me.DeltaP.GetValueOrDefault
                    CheckSpec(P2, True, "outlet pressure")

                    Pout = P2

                    Me.DeltaQ = (P2 - Pi) / rho_li / 1000 / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    H2 = Hi + Me.DeltaQ / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    IObj?.SetCurrent()
                    If P2 / Pi > 10 Then
                        Dim H2it As Double = Hi + (H2 - Hi) / 10.0
                        T2 = Ti
                        While H2it <= H2
                            Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2it, T2)
                            T2 = tmp.CalculatedTemperature.GetValueOrDefault
                            H2it += (H2 - Hi) / 10.0
                        End While
                    Else
                        Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)
                        T2 = tmp.CalculatedTemperature.GetValueOrDefault
                    End If
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... P = {0} Pa, VF = 0", P2))

                    Try
                        IObj?.SetCurrent()
                        Dim Pbub = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.TemperatureVaporFraction, Ti, 0.0#, Pi).CalculatedPressure
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                    If esin IsNot Nothing Then
                        'energy stream - update energy flow value (kW)
                        With esin
                            .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                            If args Is Nothing Then .GraphicObject.Calculated = True
                        End With
                    End If

                Case CalculationMode.OutletPressure

                    Me.PropertyPackage.CurrentMaterialStream = msin

                    P2 = Me.Pout
                    CheckSpec(P2, True, "outlet pressure")

                    Me.DeltaP = P2 - Pi

                    Me.DeltaQ = (P2 - Pi) / rho_li / 1000 / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    H2 = Hi + Me.DeltaQ / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    IObj?.SetCurrent()
                    If P2 / Pi > 10 Then
                        Dim H2it As Double = Hi + (H2 - Hi) / 10.0
                        T2 = Ti
                        While H2it <= H2
                            Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2it, T2)
                            T2 = tmp.CalculatedTemperature.GetValueOrDefault
                            H2it += (H2 - Hi) / 10.0
                        End While
                    Else
                        Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P2, H2, Ti)
                        T2 = tmp.CalculatedTemperature.GetValueOrDefault
                    End If
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... P = {0} Pa, VF = 0", P2))

                    Try
                        IObj?.SetCurrent()
                        Dim Pbub = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.TemperatureVaporFraction, Ti, 0.0#, Pi).CalculatedPressure
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                    If esin IsNot Nothing Then
                        'energy stream - update energy flow value (kW)
                        With esin
                            .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                            If args Is Nothing Then .GraphicObject.Calculated = True
                        End With
                    End If

            End Select

            Head = (P2 - Pi) / (9.81 * rho_li)

            OutletTemperature = T2

            IObj?.Paragraphs.Add("<h3>Results</h3>")

            IObj?.Paragraphs.Add(String.Format("<mi>P_2</mi>: {0} Pa", P2))
            IObj?.Paragraphs.Add(String.Format("<mi>T_2</mi>: {0} Pa", T2))
            IObj?.Paragraphs.Add(String.Format("<mi>H_2</mi>: {0} kJ/kg", H2))

            If Not DebugMode Then

                'Atribuir valores a corrente de materia conectada a jusante
                With msout
                    .AtEquilibrium = False
                    .Phases(0).Properties.temperature = T2
                    .Phases(0).Properties.pressure = P2
                    .Phases(0).Properties.enthalpy = H2
                    Dim comp As BaseClasses.Compound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = msin.Phases(0).Compounds(comp.Name).MoleFraction
                        comp.MassFraction = msin.Phases(0).Compounds(comp.Name).MassFraction
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = msin.Phases(0).Properties.massflow.GetValueOrDefault
                    .DefinedFlow = FlowSpec.Mass
                    .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
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

            'energy stream - update energy flow value (kW)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With Me.GetEnergyStream
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
                        'PROP_PU_0	Pressure Increase (Head)
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                    Case 1
                        'PROP_PU_1(Efficiency)
                        value = Me.Eficiencia.GetValueOrDefault
                    Case 2
                        'PROP_PU_2(Delta - T)
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
                    Case 3
                        'PROP_PU_3	Power Required
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault)
                    Case 4
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.NPSH.GetValueOrDefault)
                    Case 5
                        value = Pout.ConvertFromSI(su.pressure)
                    Case 6
                        value = Head.ConvertFromSI(su.distance)
                End Select

                Return value

            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            For i = 0 To 6
                proplist.Add("PROP_PU_" + CStr(i))
            Next
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
                    'PROP_PU_0	Pressure Increase (Head)
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 1
                    'PROP_PU_1(Efficiency)
                    Me.Eficiencia = propval
                Case 3
                    Me.DeltaQ = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
                Case 5
                    Me.Pout = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
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
                        'PROP_PU_0	Pressure Increase (Head)
                        value = su.deltaP
                    Case 1
                        'PROP_PU_1(Efficiency)
                        value = ""
                    Case 2
                        'PROP_PU_2(Delta - T)
                        value = su.deltaT
                    Case 3
                        'PROP_PU_3	Power Required
                        value = su.heatflow
                    Case 4
                        value = su.distance
                    Case 5
                        value = su.pressure
                    Case 6
                        value = su.distance
                End Select

                Return value
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_Pump With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Pump With {.SimObject = Me}
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
            Return My.Resources.pump
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("PUMP_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("PUMP_Name")
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

            str.AppendLine("Pump: " & Me.GraphicObject.Tag)
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
            Select Case Me.CalcMode
                Case CalculationMode.Delta_P
                    str.AppendLine("    Pressure increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Pout).ToString(numberformat, ci) & " " & su.pressure)
                Case CalculationMode.Power
                    str.AppendLine("    Power: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ).ToString(numberformat, ci) & " " & su.heatflow)
            End Select
            str.AppendLine("    Efficiency: " & Me.Eficiencia.GetValueOrDefault.ToString(numberformat, ci))
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case Me.CalcMode
                Case CalculationMode.Delta_P
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Pout).ToString(numberformat, ci) & " " & su.pressure)
                    str.AppendLine("    Power: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ).ToString(numberformat, ci) & " " & su.heatflow)
                Case CalculationMode.OutletPressure
                    str.AppendLine("    Pressure increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
                    str.AppendLine("    Power: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ).ToString(numberformat, ci) & " " & su.heatflow)
                Case CalculationMode.Power
                    str.AppendLine("    Outlet pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Pout).ToString(numberformat, ci) & " " & su.pressure)
                    str.AppendLine("    Pressure increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP).ToString(numberformat, ci) & " " & su.deltaP)
            End Select
            str.AppendLine("    Temperature increase: " & SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT).ToString(numberformat, ci) & " " & su.deltaT)
            str.AppendLine("    Available NPSH: " & SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.NPSH).ToString(numberformat, ci) & " " & su.distance)
            str.AppendLine("    Head: " & SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.Head).ToString(numberformat, ci) & " " & su.distance)

            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Adiabatic Pump '" & Me.GraphicObject?.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Calculation Mode",
                    CalcMode.ToString}))

            Select Case CalcMode
                Case CalculationMode.Delta_P
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Increase",
                            Me.DeltaP.GetValueOrDefault.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
                Case CalculationMode.OutletPressure
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.Pout.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                Case CalculationMode.EnergyStream
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Power Required",
                            Me.DeltaQ.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                            su.heatflow}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Adiabatic Efficiency",
                            Me.Eficiencia.GetValueOrDefault.ToString(nf),
                            "%"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            Select Case CalcMode
                Case CalculationMode.Delta_P
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.Pout.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Power Required",
                            Me.DeltaQ.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                            su.heatflow}))
                Case CalculationMode.OutletPressure
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Increase",
                            Me.DeltaP.GetValueOrDefault.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Power Required",
                            Me.DeltaQ.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                            su.heatflow}))
                Case CalculationMode.EnergyStream
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Pressure",
                            Me.Pout.ConvertFromSI(su.pressure).ToString(nf),
                            su.pressure}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Pressure Increase",
                            Me.DeltaP.GetValueOrDefault.ConvertFromSI(su.deltaP).ToString(nf),
                            su.deltaP}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Temperature Change",
                            Me.DeltaT.GetValueOrDefault.ConvertFromSI(su.deltaT).ToString(nf),
                            su.deltaT}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Available NPSH",
                            NPSH.GetValueOrDefault.ConvertFromSI(su.distance).ToString(nf),
                            su.distance}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Head",
                            Me.Head.ConvertFromSI(su.distance).ToString(nf),
                            su.distance}))
            Return list

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Calculation Mode") Then
                Return "Select the calculation mode of the pump. This will define which variables you need to specify."
            ElseIf p.Equals("Pressure Increase") Then
                Return "If you chose 'Pressure Increase' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            ElseIf p.Equals("Outlet Pressure") Then
                Return "If you chose 'Outlet Pressure' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            ElseIf p.Equals("Power") Then
                Return "If you chose 'Power' as the calculation mode, enter the desired value."
            ElseIf p.Equals("Efficiency (%)") Then
                Return "Enter the desired efficiency of the pumping process. This defines how much energy flow is actually added or removed to/from the inlet stream."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace

