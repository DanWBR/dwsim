Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.UnitOperations.Streams
Imports DWSIM.Inspector
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.ExtensionMethods
Imports SkiaSharp
Imports s = DWSIM.UI.Shared.Common
Imports Eto.Forms
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports DWSIM.SharedClasses
Imports System.IO
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Thermodynamics

Namespace UnitOperations

    <System.Serializable()> Public Class AirCooler2

        Inherits DWSIM.UnitOperations.UnitOperations.UnitOpBaseClass

        Implements DWSIM.Interfaces.IExternalUnitOperation

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_AirCooler

        Private calc As DWSIM.Thermodynamics.CalculatorInterface.Calculator

        Private rpp As DWSIM.Thermodynamics.PropertyPackages.RaoultPropertyPackage

        Private _name = "Air Cooler 2"
        Private _desc = "Air Cooler 2 Model"

        Public Property UseGlobalWeather As Boolean = False

        Public Enum CalcMode
            SpecifyOutletTemperature = 0
            SpecifyGeometry = 1
            SpecifyUA = 2
        End Enum

        Public Property CalculationMode As CalcMode = CalcMode.SpecifyOutletTemperature

        Public Property PressureDrop As Double = 0.0

        Public Property OutletTemperature As Double = 300

        Public Property OverallUA As Double = 500 'W/K

        Public Property ReferenceFanSpeed As Double = 100.0 'rpm

        Public Property ReferenceAirFlow As Double = 1.0 'm3/s

        Public Property ActualFanSpeed As Double = 100.0 'rpm

        Public Property ActualAirFlow As Double = 1.0 'm3/s

        Public Property AirInletTemperature As Double = 273.15 + 25.0

        Public Property AirPressure As Double = 101325.0

        Public Property AirOutletTemperature As Double = 273.15 + 25.0

        Public Property ElectricalPowerConversionFactor As Double = 1.0 'kW/rpm

        Public Property HeatLoad As Double = 0.0

        Public Property ElectricalPowerLoad As Double = 0.0

        'tube internal diameter in mm
        Public Property Tube_Di As Double = 50.0

        'tube external diameter in mm
        Public Property Tube_De As Double = 60.0

        'tube length in m
        Public Property Tube_Length As Double = 5.0

        'tube pitch in mm
        Public Property Tube_Pitch As Double = 40.0

        'tube fouling in K.m2/W
        Public Property Tube_Fouling As Double = 0.0#

        'number of tube passes per shell, integer
        Public Property Tube_PassesPerShell As Integer = 1

        'number of tubes per shell, integer
        Public Property Tube_NumberPerShell As Integer = 160

        'tube material roughness in mm
        Public Property Tube_Roughness As Double = 0.000045 * 1000

        'tube thermal conductivity
        Public Property Tube_ThermalConductivity As Double = 70.0#

        Public Property MaxHeatExchange As Double = 0.0

        Public Property ExchangerEfficiency As Double = 0.0

        Public Property LMTD As Double = 0.0
        Public Property LMTD_F As Double = 0.0


        Public Overrides Function GetDisplayName() As String
            Return _name
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return _desc
        End Function

        Private ImagePath As String = ""
        Private Image As SKImage

        Public Overrides Property ComponentName As String = _name

        Public Overrides Property ComponentDescription As String = _desc

        Private ReadOnly Property IExternalUnitOperation_Name As String = _name Implements IExternalUnitOperation.Name

        Public ReadOnly Property Prefix As String = "AC-" Implements IExternalUnitOperation.Prefix

        Public ReadOnly Property Description As String = _desc Implements IExternalUnitOperation.Description

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Exchangers

        Public Overrides ReadOnly Property MobileCompatible As Boolean = False

        Public Function ReturnInstance(typename As String) As Object Implements IExternalUnitOperation.ReturnInstance

            Return New AirCooler2()

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_AirCooler With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_AirCooler With {.SimObject = Me}
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
                    If f.InvokeRequired Then f.BeginInvoke(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.icons8_fan
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Sub Draw(g As Object) Implements IExternalUnitOperation.Draw

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            If Image Is Nothing Then

                ImagePath = System.IO.Path.GetTempFileName()
                My.Resources.icons8_fan.Save(ImagePath)

                Using streamBG = New FileStream(ImagePath, FileMode.Open)
                    Using bitmap = SKBitmap.Decode(streamBG)
                        Image = SKImage.FromBitmap(bitmap)
                    End Using
                End Using

                Try
                    File.Delete(ImagePath)
                Catch ex As Exception
                End Try

            End If

            Using p As New SKPaint With {.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                canvas.DrawImage(Image, New SKRect(GraphicObject.X, GraphicObject.Y, GraphicObject.X + GraphicObject.Width, GraphicObject.Y + GraphicObject.Height), p)
            End Using

        End Sub

        Public Sub CreateConnectors() Implements IExternalUnitOperation.CreateConnectors

            Dim myIC1 As New ConnectionPoint

            Dim w, h, x, y As Double
            w = GraphicObject.Width
            h = GraphicObject.Height
            x = GraphicObject.X
            y = GraphicObject.Y

            myIC1.Position = New Point(x, y)
            myIC1.Type = ConType.ConIn
            myIC1.Direction = ConDir.Right

            Dim myIC2 As New ConnectionPoint

            myIC2.Position = New Point(x + 0.5 * w, y + h)
            myIC2.Type = ConType.ConEn
            myIC2.Direction = ConDir.Up

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(x + w, y)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right

            With GraphicObject.InputConnectors
                If .Count = 2 Then
                    .Item(0).Position = New Point(x, y + h / 2)
                    .Item(1).Position = New Point(x + 0.5 * w, y + h)
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                End If
                .Item(0).ConnectorName = "Fluid Inlet"
                .Item(1).ConnectorName = "Power Inlet"
            End With

            With GraphicObject.OutputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x + w, y + h / 2)
                Else
                    .Add(myOC1)
                End If
                .Item(0).ConnectorName = "Fluid Outlet"
            End With

            Me.GraphicObject.EnergyConnector.Active = False

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New AirCooler2()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of AirCooler2)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        Public Sub New(ByVal Name As String, ByVal Description As String)

            MyBase.CreateNew()
            Me.ComponentName = Name
            Me.ComponentDescription = Description

        End Sub

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub PerformPostCalcValidation()

        End Sub

        Public Overrides Sub Calculate(Optional args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The Air Cooler model is a subset of the Shell and Tube HX model, with air being the 'shell side' fluid.")

            IObj?.Paragraphs.Add("Input Parameters")

            IObj?.Paragraphs.Add("The Air Cooler model in DWSIM has three calculation modes: ")

            IObj?.Paragraphs.Add("1. Specify Outlet Temperature: you must provide the 
                              fluid outlet temperature and DWSIM will calculate Overall UA and Heat Exchanged.")

            IObj?.Paragraphs.Add("2. Specify Tube Geometry: in this mode you must provide 
                          the tube geometry and DWSIM will calculate output 
                          temperatures, pressure drop at the tubes, overall 
                          HTC (U), LMTD, and exchange area. This calculation mode uses a 
                          simplified version of Tinker's method for Shell and Tube 
                          exchanger calculations, with a modification for the outside heat transfer coefficient (convection). ")

            IObj?.Paragraphs.Add("3. Specify Overall UA: in this mode you must provide the Overall UA and DWSIM will 
                          calculate the Heat Exchanged and Outlet Temperature.")

            IObj?.Paragraphs.Add("You can provide the pressure drop for the hot fluid in the exchanger for modes 1 and 3 only.")

            IObj?.Paragraphs.Add("Calculation Mode")

            IObj?.Paragraphs.Add("The heat exchanger in DWSIM is calculated using the simple  convection heat equation:")

            IObj?.Paragraphs.Add("<m>Q=UA\Delta T_{ml},</m>")

            IObj?.Paragraphs.Add("where: Q = heat exchanged, A = heat transfer area (external 
                            surface) and <mi>\Delta T_{ml}</mi> = Logarithmic Mean Temperature 
                            Difference (LMTD). We also remember that:")

            IObj?.Paragraphs.Add("<m>Q=m\Delta H,</m>")

            IObj?.Paragraphs.Add("where: <mi>Q</mi> = heat transferred from/to the fluid and <mi>\Delta H</mi> = outlet-inlet enthalpy difference.")

            IObj?.Paragraphs.Add("<h2>Inlet Streams</h2>")

            Dim su = FlowSheet.FlowsheetOptions.SelectedUnitSystem

            Dim Ti1, Ti2, w1, w2, A, Tc1, Th1, Wc, Wh, P1, P2, Th2, Tc2, U As Double
            Dim Pc1, Ph1, Pc2, Ph2, DeltaHc, DeltaHh, H1, H2, Hc1, Hh1, Hc2, Hh2, CPC, CPH As Double
            Dim StIn0, StIn1, StOut0, StOut1, StInCold, StInHot, StOutHot, StOutCold As MaterialStream
            Dim coldidx As Integer = 0

            'Validate unitop status.
            Me.Validate()

            StIn0 = Me.GetInletMaterialStream(0)
            StOut0 = Me.GetOutletMaterialStream(0)

            If calc Is Nothing Then
                calc = New Thermodynamics.CalculatorInterface.Calculator()
                calc.Initialize()
                rpp = New Thermodynamics.PropertyPackages.RaoultPropertyPackage()
            End If

            StIn1 = calc.CreateMaterialStream({"Air"}, {1.0})

            If UseGlobalWeather Then

                StIn1.Phases(0).Properties.temperature = FlowSheet.FlowsheetOptions.CurrentWeather.Temperature_C + 273.15
                StIn1.Phases(0).Properties.pressure = FlowSheet.FlowsheetOptions.CurrentWeather.AtmosphericPressure_Pa

            Else

                StIn1.Phases(0).Properties.temperature = AirInletTemperature
                StIn1.Phases(0).Properties.pressure = AirPressure

            End If

            ActualAirFlow = ReferenceAirFlow / ReferenceFanSpeed * ActualFanSpeed

            StIn1.Phases(0).Properties.massflow = Nothing
            StIn1.Phases(0).Properties.molarflow = Nothing
            StIn1.SetVolumetricFlow(ActualAirFlow)

            StOut1 = StIn1.Clone()

            StIn1.PropertyPackage = rpp
            StOut1.PropertyPackage = rpp

            IObj?.SetCurrent()
            StIn1.Calculate()

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

            IObj?.Paragraphs.Add(String.Format("<h3>Cold Stream: {0}</h3>", "Ambient Air"))

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
            IObj?.Paragraphs.Add("<m>Q_{max,cold}=W_{cold}(H_{cold,in}-H_{cold,h})</m>")

            IObj?.Paragraphs.Add("where")
            IObj?.Paragraphs.Add("<mi>H_{hot,in}</mi> is the hot stream inlet enthalpy")
            IObj?.Paragraphs.Add("<mi>H_{hot,c}</mi> is the hot stream enthalpy at cold stream inlet temperature")
            IObj?.Paragraphs.Add("<mi>H_{cold,in}</mi> is the cold stream inlet enthalpy ")
            IObj?.Paragraphs.Add("<mi>H_{cold,h}</mi> is the cold stream enthalpy at hot stream inlet temperature")

            Pc2 = Pc1
            Ph2 = Ph1 - PressureDrop

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

            IObj?.Paragraphs.Add("<mi>Q_{cold}</mi> = " & DeltaHc & " kW")

            MaxHeatExchange = Math.Min(DeltaHc, DeltaHh) 'kW

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

            IObj?.Paragraphs.Add("Calculating heat exchanged...")

            IObj?.Paragraphs.Add(String.Format("Calculation mode: {0}", [Enum].GetName(CalculationMode.GetType, CalculationMode)))

            Select Case CalculationMode

                Case CalcMode.SpecifyOutletTemperature

                    Th2 = OutletTemperature

                    StInHot.PropertyPackage.CurrentMaterialStream = StInHot
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PT flash to calculate hot stream outlet enthalpy... P = {0} Pa, T = K", Ph2, Th2))
                    IObj?.SetCurrent()
                    Dim tmp = StInHot.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, Ph2, Th2, 0.0#)
                    Hh2 = tmp.CalculatedEnthalpy
                    HeatLoad = -Wh * (Hh2 - Hh1)

                    DeltaHc = HeatLoad / Wc
                    Hc2 = Hc1 + DeltaHc
                    StInCold.PropertyPackage.CurrentMaterialStream = StInCold
                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate cold stream outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", Pc2, Hc2))
                    IObj?.SetCurrent()
                    tmp = StInCold.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pc2, Hc2, Tc1)
                    Tc2 = tmp.CalculatedTemperature
                    If DebugMode Then AppendDebugLine(String.Format("Calculated cold stream outlet temperature T2 = {0} K", Tc2))

                    LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))

                    OverallUA = HeatLoad / LMTD * 1000

                    AirOutletTemperature = Tc2

                    If HeatLoad > MaxHeatExchange Then
                        Throw New Exception("Invalid Outlet Temperature (Heat Exchanged higher than the theoretical maximum)")
                    End If

                Case CalcMode.SpecifyGeometry

                    'Shell and Tube HX calculation using Tinker's method.

                    IObj?.Paragraphs.Add("Shell and Tube HX calculation uses Tinker's method, more details <a href='http://essel.com.br/cursos/03_trocadores.htm'>on this link</a>, Chapter 5 (Capitulo 5).")

                    Dim Tc2_ant, Th2_ant As Double
                    Dim U_ant, fx, Fant, F As Double
                    Dim DTm, Tcm, Thm, R, Sf, P As Double

                    'initial estimates for R and P to calculate outlet temperatures

                    R = 0.4
                    P = 0.6

                    'hot
                    Th2 = P * (Tc1 - Th1) + Th1
                    Tc2 = Tc1 - R * (Th2 - Th1)

                    If Th2 <= Tc2 Then Th2 = Tc2 * 1.2

                    Pc2 = Pc1
                    Ph2 = Ph1
                    F = 1.0#
                    U = 500.0#

                    IObj?.Paragraphs.Add("<h3>Initial Estimates</h3>")

                    IObj?.Paragraphs.Add("<mi>T_{c,out}</mi> = " & Tc2 & " K")
                    IObj?.Paragraphs.Add("<mi>T_{h,out}</mi> = " & Th2 & " K")
                    IObj?.Paragraphs.Add("<mi>U</mi> = " & U & " W/[m2.K]")

                    Dim rhoc, muc, kc, rhoh, muh, kh, rt, di, de, L, hi, nt, vt, Ret, Prt As Double

                    Dim icnt As Integer = 0

                    Do

                        IObj?.Paragraphs.Add("<h4>Convergence Loop #" & icnt & "</h4>")

                        LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))

                        'hot
                        R = (Tc1 - Tc2) / (Th2 - Th1)
                        P = (Th2 - Th1) / (Tc1 - Th1)

                        Fant = F

                        If R <> 1.0# Then
                            Dim alpha As Double
                            alpha = ((1 - R * P) / (1 - P))
                            Sf = (alpha - 1) / (alpha - R)
                            F = (R ^ 2 + 1) ^ 0.5 * Math.Log((1 - Sf) / (1 - R * Sf)) / ((R - 1) * Math.Log((2 - Sf * (R + 1 - (R ^ 2 + 1) ^ 0.5)) / (2 - Sf * (R + 1 + (R ^ 2 + 1) ^ 0.5))))
                        Else
                            Sf = P
                            F = Sf * 2 ^ 0.5 / ((1 - Sf) * Math.Log((2 * (1 - Sf) + Sf * 2 ^ 0.5) / (2 * (1 - Sf) - Sf * 2 ^ 0.5)))
                        End If
                        If Double.IsNaN(F) Then
                            F = Fant
                        End If
                        DTm = F * LMTD

                        IObj?.Paragraphs.Add("<mi>\Delta T_{ml}</mi> = " & LMTD & " K")

                        '3
                        Tcm = (Tc2 - Tc1) / 2 + Tc1
                        Thm = (Th1 - Th2) / 2 + Th2


                        IObj?.Paragraphs.Add("<mi>F</mi> = " & F)
                        IObj?.Paragraphs.Add("<mi>\Delta T_m</mi> = " & DTm & " K")
                        IObj?.Paragraphs.Add("<mi>T_{c,m}</mi> = " & Tcm & " K")
                        IObj?.Paragraphs.Add("<mi>T_{h,m}</mi> = " & Thm & " K")

                        '4, 5

                        StInCold.PropertyPackage.CurrentMaterialStream = StInCold
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
                                IObj?.SetCurrent()
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
                                IObj?.SetCurrent()
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
                                IObj?.SetCurrent()
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid1)
                            End If
                            If tms.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                            Else
                                IObj?.SetCurrent()
                                .DW_ZerarPhaseProps(PropertyPackages.Phase.Vapor)
                            End If
                            If tms.Phases(2).Properties.molarfraction.GetValueOrDefault >= 0 And tms.Phases(2).Properties.molarfraction.GetValueOrDefault <= 1 Then
                                IObj?.SetCurrent()
                                .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid)
                            Else
                                IObj?.SetCurrent()
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
                        Dim pitch As Double, n As Integer

                        rt = Tube_Fouling
                        de = Tube_De / 1000
                        di = Tube_Di / 1000
                        L = Tube_Length
                        pitch = Tube_Pitch / 1000
                        n = Tube_NumberPerShell
                        nt = n / Tube_PassesPerShell
                        A = n * Math.PI * de * (L - 2 * de)

                        'If pitch < de Then Throw New Exception("Invalid input: tube spacing (pitch) is smaller than the tube's external diameter.")

                        'hot
                        vt = Wh / (rhoh * nt * Math.PI * di ^ 2 / 4)
                        Ret = rhoh * vt * di / muh
                        Prt = muh * CPH / kh * 1000

                        IObj?.Paragraphs.Add("<mi>Re_{tube}</mi> = " & Ret)
                        IObj?.Paragraphs.Add("<mi>Pr_{tube}</mi> = " & Prt)

                        'calcular DeltaP

                        Dim dpt As Double
                        'tube
                        dpt = 0.0#
                        Dim fric As Double = 0
                        Dim epsilon As Double = Tube_Roughness / 1000
                        If Ret > 3250 Then
                            Dim a1 = Math.Log(((epsilon / di) ^ 1.1096) / 2.8257 + (7.149 / Ret) ^ 0.8961) / Math.Log(10.0#)
                            Dim b1 = -2 * Math.Log((epsilon / di) / 3.7065 - 5.0452 * a1 / Ret) / Math.Log(10.0#)
                            fric = (1 / b1) ^ 2
                        Else
                            fric = 64 / Ret
                        End If

                        'hot
                        dpt = fric * L * Tube_PassesPerShell / di * vt ^ 2 / 2 * rhoh

                        IObj?.Paragraphs.Add("<mi>\Delta P_{tube}</mi> = " & dpt & " Pa")

                        'tube heat transfer coeff
                        hi = UnitOperations.Pipe.hint_petukhov(kh, di, fric, Ret, Prt)

                        IObj?.Paragraphs.Add("<mi>h_{int,tube}</mi> = " & hi & " W/[m2.K]")

                        'air heat transfer coeff
                        Dim he As Double = 0.0

                        'shell internal diameter
                        Dim Dsi, Dsf, nsc As Double

                        nsc = 1.1 * n ^ 0.5

                        Dsf = (nsc - 1) * pitch + de
                        Dsi = Dsf / 1.075

                        Dim Gsf, Np, Fp, Ss, Ssf, Cb, Ca, Res, Prs, aa, bb, cc, xx, yy, Nh, Y As Double
                        xx = Dsi / L
                        yy = pitch / de
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

                        Fp = 1 / (0.8 + Np * (Dsi / pitch) ^ 0.5)
                        Cb = 0.97
                        Ca = Cb * (pitch - de) / pitch
                        Ss = Ca * L * Dsf
                        Ssf = Ss / Fp

                        Gsf = Wc / Ssf
                        Res = Gsf * de / muc
                        Prs = muc * CPC / kc * 1000

                        IObj?.Paragraphs.Add("<mi>Re_{ext}</mi> = " & Res)
                        IObj?.Paragraphs.Add("<mi>Pr_{ext}</mi> = " & Prs)

                        'ho*Do/k = 0.287 * (Do * G / mu) ^ 0.61 * (cp * mu / k) ^ 0.33 * Fa

                        Dim G = ActualAirFlow / A * rhoc

                        he = (1 / de) * kc * 0.287 * (de * G / muc) ^ 0.61 * (CPC * 1000 * muc / kc) ^ 0.33

                        he = UnitOperations.Pipe.hext_holman(kc, de, Res, Prs)

                        IObj?.Paragraphs.Add("<mi>h_{ext}</mi> = " & he & " W/[m2.K]")

                        'global HTC (U)
                        Dim kt As Double = Tube_ThermalConductivity
                        Dim f1, f2, f3, f4, f5 As Double
                        f1 = de / (hi * di)
                        f2 = rt * de / di
                        f3 = de / (2 * kt) * Math.Log(de / di)
                        f4 = 0.0
                        f5 = 1 / he

                        U_ant = U
                        U = f1 + f2 + f3 + f4 + f5
                        U = 1 / U

                        HeatLoad = U * A * F * LMTD / 1000
                        If HeatLoad > MaxHeatExchange Then HeatLoad = MaxHeatExchange

                        'hot
                        DeltaHc = HeatLoad / Wc
                        DeltaHh = -HeatLoad / Wh

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

                        Ph2 = Ph1 - dpt

                        OverallUA = U * A

                        IObj?.Paragraphs.Add("<mi>Q</mi> = " & HeatLoad & " kW")
                        IObj?.Paragraphs.Add("<mi>U</mi> = " & U & " W/[m2.K]")
                        IObj?.Paragraphs.Add("<mi>A</mi> = " & A & " m2")
                        IObj?.Paragraphs.Add("<mi>UA</mi> = " & OverallUA & " W/K")

                        IObj?.Paragraphs.Add("<mi>T_{c,out}</mi> = " & Tc2 & " K")
                        IObj?.Paragraphs.Add("<mi>T_{h,out}</mi> = " & Th2 & " K")

                        Me.LMTD_F = F
                        fx = Math.Abs((Th2 - Th2_ant) ^ 2 + (Tc2 - Tc2_ant) ^ 2)

                        IObj?.Paragraphs.Add("Temperature error = " & fx)

                        FlowSheet.CheckStatus()

                        icnt += 1

                        PressureDrop = dpt

                        AirOutletTemperature = Tc2

                        OutletTemperature = Th2

                    Loop Until fx < 0.01 Or icnt > 100

                Case CalcMode.SpecifyUA

                    Dim Qi, Q_old, PIc1, PIc2, PIh1, PIh2 As Double
                    Dim NTUh, NTUc, WWh, WWc, RRh, RRc, PPh, PPc As Double
                    Dim tmp As IFlashCalculationResult
                    Dim count As Integer
                    Qi = MaxHeatExchange
                    Q_old = 10000000000.0

                    If DebugMode Then AppendDebugLine(String.Format("Start with Max Heat Exchange Q = {0} KW", Qi))

                    Do

                        If DebugMode Then AppendDebugLine(String.Format("======================================================"))
                        If DebugMode Then AppendDebugLine(String.Format("Iteration loop: {0}", count))

                        Hc2 = Qi / Wc + Hc1
                        Hh2 = Hh1 - Qi / Wh
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

                        If Math.Abs((Qi - Q_old) / Q_old) < 0.001 Or count > 100 Then Exit Do

                        WWc = Wc * (Hc2 - Hc1) / (Tc2 - Tc1) * 1000 'Heat Capacity Rate cold side
                        WWh = Wh * (Hh2 - Hh1) / (Th2 - Th1) * 1000 'Heat Capacity Rate hot side
                        NTUc = OverallUA / WWc 'Numbers of transfer units - cold side
                        NTUh = OverallUA / WWh 'Numbers of transfer units - hot side
                        RRc = WWc / WWh 'Heat capacity ratio cold side
                        RRh = WWh / WWc 'Heat capacity ratio hot side

                        If DebugMode Then AppendDebugLine(String.Format("Calculating heat exchanger"))
                        If DebugMode Then AppendDebugLine(String.Format("Number of Transfer Units - NTU_cold :{0}  NTU_hot: {1}", NTUc, NTUh))
                        If DebugMode Then AppendDebugLine(String.Format("Heat Capacity Rates - W_cold :{0}  W_hot: {1}", WWc, WWh))
                        If DebugMode Then AppendDebugLine(String.Format("Heat Capacity Ratios - R_cold :{0}  R_hot: {1}", RRc, RRh))

                        PPc = (1 - Math.Exp((RRc - 1) * NTUc)) / (1 - RRc * Math.Exp((RRc - 1) * NTUc))
                        PPh = (1 - Math.Exp((RRh - 1) * NTUh)) / (1 - RRh * Math.Exp((RRh - 1) * NTUh))

                        If DebugMode Then AppendDebugLine(String.Format("Dimensionless Temp Change - P_cold :{0}  P_hot: {1}", PPc, PPh))

                        If Double.IsNaN(PPc) Then PPc = 0
                        If Double.IsNaN(PPh) Then PPh = 0
                        Tc2 = Tc1 + PPc * (Th1 - Tc1)
                        Th2 = Th1 - PPh * (Th1 - Tc1)
                        If DebugMode Then AppendDebugLine(String.Format("Outlet Temperatures - Tc2 :{0} K  Th2: {1} K", Tc2, Th2))

                        If (Th1 - Tc2) / (Th2 - Tc1) = 1 Then
                            LMTD = ((Th1 - Tc2) + (Th2 - Tc1)) / 2
                        Else
                            LMTD = ((Th1 - Tc2) - (Th2 - Tc1)) / Math.Log((Th1 - Tc2) / (Th2 - Tc1))
                        End If

                        Q_old = Qi
                        If LMTD > 0 Then
                            Qi = OverallUA * LMTD / 1000
                        Else
                            Qi = Wh * (Hh1 - Hh2)
                            LMTD = Qi / OverallUA * 1000
                        End If

                        If DebugMode Then
                            AppendDebugLine(String.Format("Logarithmic Temperature Difference :{0} K", LMTD))
                            AppendDebugLine(String.Format("Heat Exchange Q = {0} KW", Qi))
                        End If

                        count += 1

                    Loop

                    If HeatLoad > MaxHeatExchange Then
                        Throw New Exception("Invalid Outlet Temperature (Heat Exchanged higher than the theoretical maximum)")
                    End If

                    AirOutletTemperature = Tc2
                    OutletTemperature = Th2

                    HeatLoad = Qi

                    If count > 100 Then Throw New Exception("Reached maximum number of iterations! Final Q change: " & Qi - Q_old & " kW ; " & Math.Abs((Qi - Q_old) / Q_old * 100) & " % ")

                    PIc1 = (1 + StInCold.Phases(1).Properties.molarfraction.GetValueOrDefault) * (1 + StInCold.Phases(2).Properties.molarfraction.GetValueOrDefault) * (1 + StInCold.Phases(7).Properties.molarfraction.GetValueOrDefault)
                    PIh1 = (1 + StInHot.Phases(1).Properties.molarfraction.GetValueOrDefault) * (1 + StInHot.Phases(2).Properties.molarfraction.GetValueOrDefault) * (1 + StInHot.Phases(7).Properties.molarfraction.GetValueOrDefault)

                    If (PIc1 = 2 And PIc2 > 2) Or (PIc1 > 2 And PIc2 = 2) Then FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": Phase change in cold stream detected! Heat exchange result is an aproximation.", IFlowsheet.MessageType.Warning)
                    If (PIh1 = 2 And PIh2 > 2) Or (PIh1 > 2 And PIh2 = 2) Then FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": Phase change in hot stream detected! Heat exchange result is an aproximation.", IFlowsheet.MessageType.Warning)

            End Select

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add("<mi>T_{c,out}</mi> = " & Tc2 & " K")
            IObj?.Paragraphs.Add("<mi>T_{h,out}</mi> = " & Th2 & " K")
            IObj?.Paragraphs.Add("<mi>P_{c,out}</mi> = " & Pc2 & " Pa")
            IObj?.Paragraphs.Add("<mi>P_{h,out}</mi> = " & Ph2 & " Pa")

            IObj?.Paragraphs.Add("<mi>Q</mi> = " & HeatLoad & " kW")

            IObj?.Paragraphs.Add("<mi>U</mi> = " & U & " W/[m2.K]")

            IObj?.Paragraphs.Add("<mi>\Delta T_{ml}</mi> = " & LMTD & " K")

            ExchangerEfficiency = HeatLoad / MaxHeatExchange * 100

            IObj?.Paragraphs.Add("<mi>Q/Q_{max}</mi> = " & ExchangerEfficiency & " %")

            'ElectricalPowerConversionFactor = HeatLoad / ActualFanSpeed

            If Not DebugMode Then

                ElectricalPowerLoad = ElectricalPowerConversionFactor * ActualFanSpeed

                GetInletEnergyStream(1).EnergyFlow = ElectricalPowerLoad

                'Define new calculated properties.
                StOutHot.Phases(0).Properties.temperature = Th2
                StOutCold.Phases(0).Properties.temperature = Tc2
                StOutHot.Phases(0).Properties.pressure = Ph2
                StOutCold.Phases(0).Properties.pressure = Pc2
                StOutHot.Phases(0).Properties.enthalpy = Hh2
                StOutCold.Phases(0).Properties.enthalpy = Hc2

                If Th2 < Tc1 Or Tc2 > Th1 Then
                    FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": Temperature Cross", IFlowsheet.MessageType.Warning)
                End If

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If



        End Sub

        Public Overrides Sub DeCalculate()

            Dim es As EnergyStream
            Dim cp As IConnectionPoint
            For Each cp In Me.GraphicObject.OutputConnectors
                If cp.IsAttached Then
                    es = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                    es.EnergyFlow = Nothing
                End If
            Next

        End Sub

        Public Sub PopulateEditorPanel(ctner As Object) Implements IExternalUnitOperation.PopulateEditorPanel

            Dim container As DynamicLayout = ctner

            Dim su = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim cmodes As String() = {"Specify Outlet Temperature", "Specify Tube Geometry", "Specify Overall UA"}

            s.CreateAndAddDropDownRow(container, "Calculation Mode", cmodes.ToList, CalculationMode, Sub(dd, e)
                                                                                                         CalculationMode = dd.SelectedIndex
                                                                                                     End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddLabelRow(container, "Main Properties")

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Pressure Drop ({0})", su.deltaP),
                                 PressureDrop.ConvertFromSI(su.deltaP),
                                 Sub(tb, e)
                                     PressureDrop = tb.Text.ParseExpressionToDouble.ConvertToSI(su.deltaP)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Fluid Outlet Temperature ({0})", su.temperature),
                                 OutletTemperature.ConvertFromSI(su.temperature),
                                 Sub(tb, e)
                                     OutletTemperature = tb.Text.ParseExpressionToDouble.ConvertToSI(su.temperature)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            Dim tbx = s.CreateAndAddTextBoxRow(container, nf, String.Format("Heat Load ({0})", su.heatflow),
                                 HeatLoad.ConvertFromSI(su.heatflow),
                                 Sub(tb, e)
                                 End Sub, Sub() FlowSheet.RequestCalculation())
            tbx.ReadOnly = True

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Overall UA ({0})", "W/K"),
                                 OverallUA,
                                 Sub(tb, e)
                                     OverallUA = tb.Text.ParseExpressionToDouble
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Air Inlet Temperature ({0})", su.temperature),
                                 AirInletTemperature.ConvertFromSI(su.temperature),
                                 Sub(tb, e)
                                     AirInletTemperature = tb.Text.ParseExpressionToDouble.ConvertToSI(su.temperature)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Air Inlet Pressure ({0})", su.pressure),
                                 AirPressure.ConvertFromSI(su.temperature),
                                 Sub(tb, e)
                                     AirPressure = tb.Text.ParseExpressionToDouble.ConvertToSI(su.pressure)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            Dim tbx2 = s.CreateAndAddTextBoxRow(container, nf, String.Format("Air Outlet Temperature ({0})", su.temperature),
                                 AirOutletTemperature.ConvertFromSI(su.temperature),
                                 Sub(tb, e)
                                     AirOutletTemperature = tb.Text.ParseExpressionToDouble.ConvertToSI(su.temperature)
                                 End Sub, Sub() FlowSheet.RequestCalculation())
            tbx2.ReadOnly = True

            Dim tbx3 = s.CreateAndAddTextBoxRow(container, nf, String.Format("Heat Load ({0})", su.heatflow),
                                 HeatLoad.ConvertFromSI(su.heatflow),
                                 Sub(tb, e)
                                     HeatLoad = tb.Text.ParseExpressionToDouble.ConvertToSI(su.heatflow)
                                 End Sub, Sub() FlowSheet.RequestCalculation())
            tbx3.ReadOnly = True

            Dim tbx4 = s.CreateAndAddTextBoxRow(container, nf, String.Format("Maximum Heat Load ({0})", su.heatflow),
                                 MaxHeatExchange.ConvertFromSI(su.heatflow),
                                 Sub(tb, e)
                                     MaxHeatExchange = tb.Text.ParseExpressionToDouble.ConvertToSI(su.heatflow)
                                 End Sub, Sub() FlowSheet.RequestCalculation())
            tbx4.ReadOnly = True

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Exchanger Efficiency ({0})", "%"),
                                 ExchangerEfficiency,
                                 Sub(tb, e)
                                     ExchangerEfficiency = tb.Text.ParseExpressionToDouble
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddLabelRow(container, "Fan Properties")

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Reference Fan Speed ({0})", "rpm"),
                                 ReferenceFanSpeed,
                                 Sub(tb, e)
                                     ReferenceFanSpeed = tb.Text.ParseExpressionToDouble
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Reference Air Flow ({0})", su.volumetricFlow),
                                 ReferenceAirFlow.ConvertFromSI(su.volumetricFlow),
                                 Sub(tb, e)
                                     ReferenceAirFlow = tb.Text.ParseExpressionToDouble.ConvertToSI(su.volumetricFlow)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Actual Fan Speed ({0})", "rpm"),
                                 ActualFanSpeed,
                                 Sub(tb, e)
                                     ActualFanSpeed = tb.Text.ParseExpressionToDouble
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Actual Air Flow ({0})", su.volumetricFlow),
                                 ActualAirFlow.ConvertFromSI(su.volumetricFlow),
                                 Sub(tb, e)
                                     ActualAirFlow = tb.Text.ParseExpressionToDouble.ConvertToSI(su.volumetricFlow)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Electrical Power Conversion Factor ({0})", "kW/rpm"),
                                 ElectricalPowerConversionFactor,
                                 Sub(tb, e)
                                     ElectricalPowerConversionFactor = tb.Text.ParseExpressionToDouble
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            Dim tbx5 = s.CreateAndAddTextBoxRow(container, nf, String.Format("Electrical Power Load ({0})", su.heatflow),
                                 ElectricalPowerLoad.ConvertFromSI(su.heatflow),
                                 Sub(tb, e)
                                     ElectricalPowerLoad = tb.Text.ParseExpressionToDouble.ConvertToSI(su.heatflow)
                                 End Sub, Sub() FlowSheet.RequestCalculation())
            tbx5.ReadOnly = True

            s.CreateAndAddLabelRow(container, "Tube Properties")

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube Internal Diameter ({0})", su.diameter),
                                 Tube_Di.ConvertFromSI(su.diameter),
                                 Sub(tb, e)
                                     Tube_Di = tb.Text.ParseExpressionToDouble.ConvertToSI(su.diameter)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube External Diameter ({0})", su.diameter),
                                 Tube_De.ConvertFromSI(su.diameter),
                                 Sub(tb, e)
                                     Tube_De = tb.Text.ParseExpressionToDouble.ConvertToSI(su.diameter)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube Length ({0})", su.distance),
                                 Tube_Length.ConvertFromSI(su.distance),
                                 Sub(tb, e)
                                     Tube_Length = tb.Text.ParseExpressionToDouble.ConvertToSI(su.distance)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube Pitch ({0})", su.thickness),
                                 Tube_Pitch.ConvertFromSI(su.thickness),
                                 Sub(tb, e)
                                     Tube_Length = tb.Text.ParseExpressionToDouble.ConvertToSI(su.thickness)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube Fouling Factor ({0})", su.foulingfactor),
                                 Tube_Fouling.ConvertFromSI(su.foulingfactor),
                                 Sub(tb, e)
                                     Tube_Fouling = tb.Text.ParseExpressionToDouble.ConvertToSI(su.foulingfactor)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube Roughness ({0})", su.distance),
                                 Tube_Roughness.ConvertFromSI(su.distance),
                                 Sub(tb, e)
                                     Tube_Roughness = tb.Text.ParseExpressionToDouble.ConvertToSI(su.distance)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube Thermal Conductivity ({0})", su.thermalConductivity),
                                 Tube_ThermalConductivity.ConvertFromSI(su.thermalConductivity),
                                 Sub(tb, e)
                                     Tube_ThermalConductivity = tb.Text.ParseExpressionToDouble.ConvertToSI(su.thermalConductivity)
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Number of Tubes"),
                                 Tube_NumberPerShell,
                                 Sub(tb, e)
                                     Tube_NumberPerShell = tb.Text.ParseExpressionToDouble
                                 End Sub, Sub() FlowSheet.RequestCalculation())

            s.CreateAndAddTextBoxRow(container, nf, String.Format("Tube Passes"),
                                 Tube_PassesPerShell,
                                 Sub(tb, e)
                                     Tube_PassesPerShell = tb.Text.ParseExpressionToDouble
                                 End Sub, Sub() FlowSheet.RequestCalculation())

        End Sub

        Private Sub CallSolverIfNeeded()
            If GlobalSettings.Settings.CallSolverOnEditorPropertyChanged Then
                FlowSheet.RequestCalculation()
            End If
        End Sub

        Public Overrides Function GetPropertyDescription(p As String) As String
            Return p
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI

            Select Case prop
                Case "Pressure Drop"
                    Return PressureDrop.ConvertFromSI(su.deltaP)
                Case "Outlet Temperature"
                    Return OutletTemperature.ConvertFromSI(su.temperature)
                Case "Overall UA"
                    Return OverallUA
                Case "Reference Fan Speed"
                    Return ReferenceFanSpeed
                Case "Reference Air Flow"
                    Return ReferenceAirFlow.ConvertFromSI(su.volumetricFlow)
                Case "Actual Fan Speed"
                    Return ActualFanSpeed
                Case "Actual Air Flow"
                    Return ActualAirFlow.ConvertFromSI(su.volumetricFlow)
                Case "Air Inlet Temperature"
                    Return AirInletTemperature.ConvertFromSI(su.temperature)
                Case "Air Pressure"
                    Return AirPressure.ConvertFromSI(su.pressure)
                Case "Air Outlet Temperature"
                    Return AirOutletTemperature.ConvertFromSI(su.temperature)
                Case "Electrical Power Conversion Factor"
                    Return ElectricalPowerConversionFactor
                Case "Heat Load"
                    Return HeatLoad.ConvertFromSI(su.heatflow)
                Case "Electrical Power Load"
                    Return ElectricalPowerLoad.ConvertFromSI(su.heatflow)
                Case "Tube Internal Diameter"
                    Return Tube_Di.ConvertFromSI(su.diameter)
                Case "Tube External Diameter"
                    Return Tube_De.ConvertFromSI(su.diameter)
                Case "Tube Length"
                    Return Tube_Length.ConvertFromSI(su.distance)
                Case "Tube Pitch"
                    Return Tube_Pitch.ConvertFromSI(su.thickness)
                Case "Tube Fouling"
                    Return Tube_Fouling.ConvertFromSI(su.foulingfactor)
                Case "Tube Passes"
                    Return Tube_PassesPerShell
                Case "Tube Number"
                    Return Tube_NumberPerShell
                Case "Tube Roughness"
                    Return Tube_Roughness.ConvertFromSI(su.distance)
                Case "Tube Thermal Conductivity"
                    Return Tube_ThermalConductivity.ConvertFromSI(su.thermalConductivity)
                Case "Max Heat Exchange"
                    Return MaxHeatExchange.ConvertFromSI(su.heatflow)
                Case "Exchanger Efficiency"
                    Return ExchangerEfficiency
                Case Else
                    Return 0
            End Select

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()

            Dim list As New List(Of String)

            list.Add("Pressure Drop")
            list.Add("Outlet Temperature")
            list.Add("Overall UA")
            list.Add("Reference Fan Speed")
            list.Add("Reference Air Flow")
            list.Add("Actual Fan Speed")
            list.Add("Actual Air Flow")
            list.Add("Air Inlet Temperature")
            list.Add("Air Pressure")
            list.Add("Air Outlet Temperature")
            list.Add("Electrical Power Conversion Factor")
            list.Add("Heat Load")
            list.Add("Electrical Power Load")
            list.Add("Tube Internal Diameter")
            list.Add("Tube External Diameter")
            list.Add("Tube Length")
            list.Add("Tube Pitch")
            list.Add("Tube Fouling")
            list.Add("Tube Passes")
            list.Add("Tube Number")
            list.Add("Tube Roughness")
            list.Add("Tube Thermal Conductivity")
            list.Add("Max Heat Exchange")
            list.Add("Exchanger Efficiency")

            Return list.ToArray

        End Function

        Public Overloads Overrides Function GetDefaultProperties() As String()

            Dim list As New List(Of String)

            list.Add("Outlet Temperature")
            list.Add("Overall UA")
            list.Add("Heat Load")
            list.Add("Electrical Power Load")
            list.Add("Actual Air Flow")
            list.Add("Air Inlet Temperature")
            list.Add("Air Outlet Temperature")
            list.Add("Max Heat Exchange")
            list.Add("Exchanger Efficiency")

            Return list.ToArray

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If su Is Nothing Then su = New SystemsOfUnits.SI

            Select Case prop
                Case "Pressure Drop"
                    PressureDrop = propval.ConvertToSI(su.deltaP)
                Case "Outlet Temperature"
                    OutletTemperature = propval.ConvertToSI(su.temperature)
                Case "Overall UA"
                    OverallUA = propval
                Case "Reference Fan Speed"
                    ReferenceFanSpeed = propval
                Case "Reference Air Flow"
                    ReferenceAirFlow = propval.ConvertToSI(su.volumetricFlow)
                Case "Actual Fan Speed"
                    ActualFanSpeed = propval
                Case "Actual Air Flow"
                    ActualAirFlow = propval.ConvertToSI(su.volumetricFlow)
                Case "Air Inlet Temperature"
                    AirInletTemperature = propval.ConvertToSI(su.temperature)
                Case "Air Pressure"
                    AirPressure = propval.ConvertToSI(su.pressure)
                Case "Air Outlet Temperature"
                    AirOutletTemperature = propval.ConvertToSI(su.temperature)
                Case "Electrical Power Conversion Factor"
                    ElectricalPowerConversionFactor = propval
                Case "Heat Load"
                    HeatLoad = propval.ConvertToSI(su.heatflow)
                Case "Electrical Power Load"
                    ElectricalPowerLoad = propval.ConvertToSI(su.heatflow)
                Case "Tube Internal Diameter"
                    Tube_Di = propval.ConvertToSI(su.diameter)
                Case "Tube External Diameter"
                    Tube_De = propval.ConvertToSI(su.diameter)
                Case "Tube Length"
                    Tube_Length = propval.ConvertToSI(su.distance)
                Case "Tube Pitch"
                    Tube_Pitch = propval.ConvertToSI(su.thickness)
                Case "Tube Fouling"
                    Tube_Fouling = propval.ConvertToSI(su.foulingfactor)
                Case "Tube Passes"
                    Tube_PassesPerShell = propval
                Case "Tube Number"
                    Tube_NumberPerShell = propval
                Case "Tube Roughness"
                    Tube_Roughness = propval.ConvertToSI(su.distance)
                Case "Tube Thermal Conductivity"
                    Tube_ThermalConductivity = propval.ConvertToSI(su.thermalConductivity)
                Case "Max Heat Exchange"
                    MaxHeatExchange = propval.ConvertToSI(su.heatflow)
                Case "Exchanger Efficiency"
                    ExchangerEfficiency = propval
            End Select

            Return True

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String

            If su Is Nothing Then su = New SystemsOfUnits.SI

            Select Case prop
                Case "Pressure Drop"
                    Return su.deltaP
                Case "Outlet Temperature"
                    Return su.temperature
                Case "Overall UA"
                    Return "W/K"
                Case "Reference Fan Speed"
                    Return "rpm"
                Case "Reference Air Flow"
                    Return su.volumetricFlow
                Case "Actual Fan Speed"
                    Return "rpm"
                Case "Actual Air Flow"
                    Return su.volumetricFlow
                Case "Air Inlet Temperature"
                    Return su.temperature
                Case "Air Pressure"
                    Return su.pressure
                Case "Air Outlet Temperature"
                    Return su.temperature
                Case "Electrical Power Conversion Factor"
                    Return ""
                Case "Heat Load"
                    Return su.heatflow
                Case "Electrical Power Load"
                    Return su.heatflow
                Case "Tube Internal Diameter"
                    Return su.diameter
                Case "Tube External Diameter"
                    Return su.diameter
                Case "Tube Length"
                    Return su.distance
                Case "Tube Pitch"
                    Return su.thickness
                Case "Tube Fouling"
                    Return su.foulingfactor
                Case "Tube Passes"
                    Return ""
                Case "Tube Number"
                    Return ""
                Case "Tube Roughness"
                    Return su.distance
                Case "Tube Thermal Conductivity"
                    Return su.thermalConductivity
                Case "Max Heat Exchange"
                    Return su.heatflow
                Case "Exchanger Efficiency"
                    Return "%"
                Case Else
                    Return ""
            End Select

        End Function

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            Dim istr, ostr As MaterialStream
            istr = Me.GetInletMaterialStream(0)
            ostr = Me.GetOutletMaterialStream(0)

            istr.PropertyPackage.CurrentMaterialStream = istr

            str.AppendLine("Air Cooler: " & Me.GraphicObject.Tag)
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
            str.AppendLine("    Molar composition: " & istr.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).ToArrayString())
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            str.AppendLine("    Pressure drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.PressureDrop).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine("    Fluid outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Air outlet temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.AirOutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
            str.AppendLine("    Heat load: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.HeatLoad).ToString(numberformat, ci) & " " & su.heatflow)

            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Air Cooler '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {"Calculation Mode",
                    CalculationMode.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Pressure Drop",
                    Me.PressureDrop.ConvertFromSI(su.deltaP).ToString(nf),
                    su.deltaP}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Fluid Outlet Temperature",
                    Me.OutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                    su.temperature}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Air Outlet Temperature",
                    Me.AirOutletTemperature.ConvertFromSI(su.temperature).ToString(nf),
                    su.temperature}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Heat Load",
                    Me.HeatLoad.ConvertFromSI(su.heatflow).ToString(nf),
                    su.heatflow}))

            Return list

        End Function


    End Class

End Namespace
