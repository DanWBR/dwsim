Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations
Imports SkiaSharp

Namespace UnitOperations

    Public Class WindTurbine

        Inherits CleanEnergyUnitOpBase

        Private ImagePath As String = ""

        Private Image As SKImage

        Private calc As DWSIM.Thermodynamics.CalculatorInterface.Calculator

        Private rpp As DWSIM.Thermodynamics.PropertyPackages.RaoultPropertyPackage

        Public Overrides Property Prefix As String = "WT-"

        Public UserDefinedWindSpeed As Double = 10.0

        Public UserDefinedAirTemperature As Double = 298.15

        Public UserDefinedAirPressure As Double = 101325.0

        Public UserDefinedRelativeHumidity As Double = 30.0

        Public Property MinimumWindSpeed As Double = 3.0

        Public Property MaximumWindSpeed As Double = 25.0

        Public Property DiskArea As Double = 10.0

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
            myOC1.Position = New Point(x + w, y / 2.0)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right
            myOC1.Type = ConType.ConEn

            With GraphicObject.OutputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x + w, y / 2.0)
                Else
                    .Add(myOC1)
                End If
                .Item(0).ConnectorName = "Power Outlet"
            End With

            Me.GraphicObject.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub PopulateEditorPanel(ctner As Object)

        End Sub

        Public Overrides Sub DisplayEditForm()

        End Sub

        Public Overrides Sub UpdateEditForm()

        End Sub

        Public Overrides Sub CloseEditForm()

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
            airstr.PropertyPackage = rpp
            rpp.CurrentMaterialStream = airstr

            airstr.Calculate()

            AirDensity = airstr.Phases(2).Properties.density.GetValueOrDefault()

            airstr.Dispose()
            airstr = Nothing

            MaximumTheoreticalPower = 8.0 / 27.0 * AirDensity * ws ^ 3 * DiskArea / 1000.0 ' kW

            GeneratedPower = MaximumTheoreticalPower * Efficiency / 100.0

            esout.EnergyFlow = GeneratedPower

        End Sub
    End Class

End Namespace