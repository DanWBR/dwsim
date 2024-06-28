Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations
Imports SkiaSharp
Imports Eto.Forms
Imports DWSIM.UI.Shared.Common
Imports System.Globalization

Namespace UnitOperations

    Public Class WindTurbine

        Inherits CleanEnergyUnitOpBase

        <Xml.Serialization.XmlIgnore> Public f As EditingForm_WindTurbine

        Private ImagePath As String = ""

        Private Image As SKImage

        Private calc As DWSIM.Thermodynamics.CalculatorInterface.Calculator

        Private rpp As DWSIM.Thermodynamics.PropertyPackages.RaoultPropertyPackage

        Public Overrides Property Prefix As String = "WT-"

        Public UserDefinedWindSpeed As Double = 10.0

        Public UserDefinedAirTemperature As Double = 298.15

        Public UserDefinedAirPressure As Double = 101325.0

        Public UserDefinedRelativeHumidity As Double = 30.0

        Public ActualWindSpeed As Double = 10.0

        Public ActualAirTemperature As Double = 298.15

        Public ActualAirPressure As Double = 101325.0

        Public ActualRelativeHumidity As Double = 30.0

        Public Property DiskArea As Double = 10.0

        Public Property RotorDiameter As Double = 0.0

        Public Property Efficiency As Double = 80.0

        Public Property NumberOfTurbines As Integer = 1

        Public Property AirDensity As Double = 0.0

        Public Property GeneratedPower As Double = 0.0

        Public Property MaximumTheoreticalPower As Double = 0.0

        Public Overrides Function GetDisplayName() As String
            Return "Wind Turbine"
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return "Wind Turbine"
        End Function

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub Draw(g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            If Image Is Nothing Then

                ImagePath = SharedClasses.Utility.GetTempFileName()
                My.Resources.icons8_wind_turbine.Save(ImagePath)

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

        Public Overrides Sub CreateConnectors()

            Dim w, h, x, y As Double
            w = GraphicObject.Width
            h = GraphicObject.Height
            x = GraphicObject.X
            y = GraphicObject.Y

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(x + w, y + h / 2.0)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right
            myOC1.Type = ConType.ConEn

            With GraphicObject.OutputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x + w, y + h / 2.0)
                Else
                    .Add(myOC1)
                End If
                .Item(0).ConnectorName = "Power Outlet"
            End With

            Me.GraphicObject.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub PopulateEditorPanel(ctner As Object)


            Dim container As DynamicLayout = ctner

            Dim su = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            container.CreateAndAddCheckBoxRow("Use Global Weather Conditions", Not UseUserDefinedWeather,
                                        Sub(chk, e)
                                            UseUserDefinedWeather = Not chk.Checked
                                        End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Wind Speed ({0})", su.velocity), UserDefinedWindSpeed,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     UserDefinedWindSpeed = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.velocity)
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Air Temperature ({0})", su.temperature), UserDefinedAirTemperature,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     UserDefinedAirTemperature = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.temperature)
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Air Pressure ({0})", su.pressure), UserDefinedAirPressure,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     UserDefinedAirPressure = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.pressure)
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Relative Humidity ({0})", "%"), UserDefinedRelativeHumidity,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     UserDefinedRelativeHumidity = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

            container.CreateAndAddEmptySpace()

            container.CreateAndAddTextBoxRow(nf, String.Format("Disk Area ({0})", su.area), DiskArea,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     DiskArea = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.area)
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Efficiency ({0})", "%"), Efficiency,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     Efficiency = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, "Number of Units", NumberOfTurbines,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     NumberOfTurbines = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

        End Sub

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As CultureInfo, nf As String) As String

            Dim sb As New Text.StringBuilder()

            sb.AppendLine(String.Format("Number of Units: {0}", NumberOfTurbines))

            sb.AppendLine()
            sb.AppendLine(String.Format("Using Global Weather: {0}", Not UseUserDefinedWeather))
            sb.AppendLine(String.Format("Air Temperature: {0} {1}", ActualAirTemperature.ConvertFromSI(su.temperature).ToString(nf), su.temperature))
            sb.AppendLine(String.Format("Air Pressure: {0} {1}", ActualAirPressure.ConvertFromSI(su.pressure).ToString(nf), su.pressure))
            sb.AppendLine(String.Format("Relative Humidity (%): {0}", ActualRelativeHumidity.ToString(nf)))

            sb.AppendLine()
            sb.AppendLine(String.Format("Disk Area: {0} {1}", DiskArea.ConvertFromSI(su.area).ToString(nf), su.area))
            sb.AppendLine(String.Format("Efficiency: {0}", Efficiency.ToString(nf)))
            sb.AppendLine()
            sb.AppendLine(String.Format("Calculated Air Density: {0} {1}", AirDensity.ConvertFromSI(su.density).ToString(nf), su.density))
            sb.AppendLine(String.Format("Maximum Theoretical Power: {0} {1}", MaximumTheoreticalPower.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow))
            sb.AppendLine(String.Format("Generated Power: {0} {1}", GeneratedPower.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow))

            Return sb.ToString()

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_WindTurbine With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_WindTurbine With {.SimObject = Me}
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
                    If f.InvokeRequired Then f.BeginInvoke(Sub() f.UpdateInfo()) Else f.UpdateInfo()
                End If
            End If

        End Sub

        Public Overrides Sub CloseEditForm()

            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If

        End Sub

        Public Overrides Function ReturnInstance(typename As String) As Object

            Return New WindTurbine

        End Function

        Public Overrides Function GetIconBitmap() As Object

            Return My.Resources.icons8_wind_turbine

        End Function

        Public Overrides Function CloneXML() As Object

            Dim obj As ICustomXMLSerialization = New WindTurbine()
            obj.LoadData(Me.SaveData)
            Return obj

        End Function

        Public Overrides Function CloneJSON() As Object

            Throw New NotImplementedException()

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

        Public Overrides Sub Calculate(Optional args As Object = Nothing)

            Dim esout = GetOutletEnergyStream(0)

            Dim ws, at, ap, rh As Double

            If UseUserDefinedWeather Then

                ws = UserDefinedWindSpeed
                at = UserDefinedAirTemperature
                rh = UserDefinedRelativeHumidity
                ap = UserDefinedAirPressure

            Else

                ws = FlowSheet.FlowsheetOptions.CurrentWeather.WindSpeed_km_h / 3.6
                at = FlowSheet.FlowsheetOptions.CurrentWeather.Temperature_C + 273.15
                rh = FlowSheet.FlowsheetOptions.CurrentWeather.RelativeHumidity_pct
                ap = FlowSheet.FlowsheetOptions.CurrentWeather.AtmosphericPressure_Pa

            End If

            ActualAirPressure = ap
            ActualAirTemperature = at
            ActualRelativeHumidity = rh
            ActualWindSpeed = ws

            'calculate air density

            If calc Is Nothing Then
                calc = New Thermodynamics.CalculatorInterface.Calculator()
                calc.Initialize()
                rpp = New Thermodynamics.PropertyPackages.RaoultPropertyPackage()
            End If

            Dim airstr = calc.CreateMaterialStream({"Air", "Water"}, {1.0, 1.0})
            airstr.SetMassFlow(1.0)
            airstr.SetTemperature(at)
            airstr.SetPressure(ap)
            airstr.SetFlowsheet(FlowSheet)
            airstr.PropertyPackage = rpp
            rpp.CurrentMaterialStream = airstr

            airstr.Calculate()

            Dim wc = airstr.Phases(2).Compounds("Water").MoleFraction.GetValueOrDefault()

            'add relative humidity
            wc = wc * rh / 100.0

            airstr = calc.CreateMaterialStream({"Air", "Water"}, {1.0 - wc, wc})
            airstr.SetMassFlow(1.0)
            airstr.SetTemperature(at)
            airstr.SetPressure(ap)
            airstr.SetFlowsheet(FlowSheet)
            airstr.PropertyPackage = rpp
            rpp.CurrentMaterialStream = airstr

            airstr.Calculate()

            AirDensity = airstr.Phases(2).Properties.density.GetValueOrDefault()

            airstr.Dispose()
            airstr = Nothing

            If RotorDiameter <> 0.0 Then
                DiskArea = Math.PI * RotorDiameter ^ 2 / 4
            Else
                RotorDiameter = (DiskArea * 4 / Math.PI) ^ 0.5
            End If

            MaximumTheoreticalPower = NumberOfTurbines * 8.0 / 27.0 * AirDensity * ws ^ 3 * DiskArea / 1000.0 ' kW

            GeneratedPower = MaximumTheoreticalPower * Efficiency / 100.0

            esout.EnergyFlow = GeneratedPower

        End Sub

        Public Overrides Function GetProperties(proptype As PropertyType) As String()

            Select Case proptype
                Case PropertyType.ALL, PropertyType.RW, PropertyType.RO
                    Return New String() {"Efficiency", "User-Defined Wind Speed", "Actual Wind Speed", "User-Defined Air Temperature", "Actual Air Temperature",
                        "User-Defined Air Pressure", "Actual Air Pressure", "User-Defined Relative Humidity", "Actual Relative Humidity",
                        "Disk Area", "Rotor Diameter", "Number of Units", "Generated Power", "Maximum Theoretical Power", "Calculated Air Density"}
                Case PropertyType.WR
                    Return New String() {"Efficiency", "User-Defined Wind Speed", "User-Defined Air Temperature",
                        "User-Defined Air Pressure", "User-Defined Relative Humidity", "Rotor Diameter", "Number of Units"}
            End Select

        End Function

        Public Overrides Function GetPropertyValue(prop As String, Optional su As IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    Return Efficiency
                Case "User-Defined Wind Speed"
                    Return UserDefinedWindSpeed.ConvertFromSI(su.velocity)
                Case "Actual Wind Speed"
                    Return ActualWindSpeed.ConvertFromSI(su.velocity)
                Case "User-Defined Air Temperature"
                    Return UserDefinedAirTemperature.ConvertFromSI(su.temperature)
                Case "Actual Air Temperature"
                    Return ActualAirTemperature.ConvertFromSI(su.temperature)
                Case "User-Defined Air Pressure"
                    Return UserDefinedAirPressure.ConvertFromSI(su.pressure)
                Case "Actual Air Pressure"
                    Return ActualAirPressure.ConvertFromSI(su.pressure)
                Case "User-Defined Relative Humidity"
                    Return UserDefinedRelativeHumidity
                Case "Actual Relative Humidity"
                    Return ActualRelativeHumidity
                Case "Disk Area"
                    Return DiskArea.ConvertFromSI(su.area)
                Case "Rotor Diameter"
                    Return RotorDiameter.ConvertFromSI(su.distance)
                Case "Number of Units"
                    Return NumberOfTurbines
                Case "Generated Power"
                    Return GeneratedPower.ConvertFromSI(su.heatflow)
                Case "Maximum Theoretical Power"
                    Return MaximumTheoreticalPower.ConvertFromSI(su.heatflow)
                Case "Calculated Air Density"
                    Return AirDensity.ConvertFromSI(su.density)
            End Select

        End Function

        Public Overrides Function GetPropertyUnit(prop As String, Optional su As IUnitsOfMeasure = Nothing) As String

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    Return "%"
                Case "User-Defined Wind Speed"
                    Return (su.velocity)
                Case "Actual Wind Speed"
                    Return (su.velocity)
                Case "User-Defined Air Temperature"
                    Return (su.temperature)
                Case "Actual Air Temperature"
                    Return (su.temperature)
                Case "User-Defined Air Pressure"
                    Return (su.pressure)
                Case "Actual Air Pressure"
                    Return (su.pressure)
                Case "User-Defined Relative Humidity"
                    Return "%"
                Case "Actual Relative Humidity"
                    Return "%"
                Case "Disk Area"
                    Return (su.area)
                Case "Rotor Diameter"
                    Return (su.distance)
                Case "Number of Units"
                    Return ""
                Case "Generated Power"
                    Return (su.heatflow)
                Case "Maximum Theoretical Power"
                    Return (su.heatflow)
                Case "Calculated Air Density"
                    Return (su.density)
            End Select

        End Function

        Public Overrides Function SetPropertyValue(prop As String, propval As Object, Optional su As IUnitsOfMeasure = Nothing) As Boolean

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI
            Select Case prop
                Case "Efficiency"
                    Efficiency = Convert.ToDouble(propval)
                Case "User-Defined Wind Speed"
                    UserDefinedWindSpeed = Convert.ToDouble(propval).ConvertFromSI(su.velocity)
                Case "User-Defined Air Temperature"
                    UserDefinedAirTemperature = Convert.ToDouble(propval).ConvertFromSI(su.temperature)
                Case "User-Defined Air Pressure"
                    UserDefinedAirPressure = Convert.ToDouble(propval).ConvertFromSI(su.pressure)
                Case "User-Defined Relative Humidity"
                    UserDefinedRelativeHumidity = Convert.ToDouble(propval)
                Case "Disk Area"
                    DiskArea = Convert.ToDouble(propval).ConvertFromSI(su.area)
                    RotorDiameter = (DiskArea * 4 / Math.PI) ^ 0.5
                Case "Rotor Diameter"
                    RotorDiameter = Convert.ToDouble(propval).ConvertFromSI(su.distance)
                    DiskArea = Math.PI * RotorDiameter ^ 2 / 4
                Case "Number of Units"
                    NumberOfTurbines = Convert.ToDouble(propval)
            End Select

            Return True

        End Function

    End Class

End Namespace