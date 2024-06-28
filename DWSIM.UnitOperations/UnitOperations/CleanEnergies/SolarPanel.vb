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

    Public Class SolarPanel

        Inherits CleanEnergyUnitOpBase

        Private ImagePath As String = ""

        Private Image As SKImage

        <Xml.Serialization.XmlIgnore> Public f As EditingForm_SolarPanel

        Public Overrides Property Prefix As String = "SP-"

        Public Property PanelArea As Double = 1

        Public Property PanelEfficiency As Double = 15

        Public Property NumberOfPanels As Integer = 1

        Public Property GeneratedPower As Double = 0.0

        Public Property SolarIrradiation_kW_m2 As Double = 1.0

        Public Property ActualSolarIrradiation_kW_m2 As Double = 1.0

        Public Overrides Function GetDisplayName() As String
            Return "Solar Panel"
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return "Solar Panel"
        End Function

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub Draw(g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            If Image Is Nothing Then

                ImagePath = SharedClasses.Utility.GetTempFileName()
                My.Resources.icons8_solar_panel.Save(ImagePath)

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
            myOC1.Position = New Point(x + w, y + w / 2.0)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right
            myOC1.Type = ConType.ConEn

            With GraphicObject.OutputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x + w, y + w / 2.0)
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

            container.CreateAndAddTextBoxRow(nf, String.Format("Solar Irradiation ({0})", "kW/m2"), SolarIrradiation_kW_m2,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     SolarIrradiation_kW_m2 = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

            container.CreateAndAddEmptySpace()

            container.CreateAndAddTextBoxRow(nf, String.Format("Panel Area ({0})", su.area), PanelArea,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     PanelArea = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.area)
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Efficiency ({0})", "%"), PanelEfficiency,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     PanelEfficiency = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, "Number of Units", NumberOfPanels,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     NumberOfPanels = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

        End Sub

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As CultureInfo, nf As String) As String

            Dim sb As New Text.StringBuilder()

            sb.AppendLine(String.Format("Number of Units: {0}", NumberOfPanels))

            sb.AppendLine()
            sb.AppendLine(String.Format("Using Global Weather: {0}", Not UseUserDefinedWeather))
            sb.AppendLine(String.Format("Solar Irradiation: {0} kW/m2", SolarIrradiation_kW_m2.ToString(nf)))

            sb.AppendLine()
            sb.AppendLine(String.Format("Panel Area: {0} {1}", PanelArea.ConvertFromSI(su.area).ToString(nf), su.area))
            sb.AppendLine(String.Format("Efficiency: {0}", PanelEfficiency.ToString(nf)))
            sb.AppendLine()
            sb.AppendLine(String.Format("Generated Power: {0} {1}", GeneratedPower.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow))

            Return sb.ToString()

        End Function

        Public Overrides Function ReturnInstance(typename As String) As Object

            Return New SolarPanel

        End Function

        Public Overrides Function GetIconBitmap() As Object

            Return My.Resources.icons8_solar_panel

        End Function

        Public Overrides Function CloneXML() As Object

            Dim obj As ICustomXMLSerialization = New SolarPanel()
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

            Dim si As Double = 0.0

            If UseUserDefinedWeather Then

                si = SolarIrradiation_kW_m2

            Else

                si = FlowSheet.FlowsheetOptions.CurrentWeather.SolarIrradiation_kWh_m2

            End If

            ActualSolarIrradiation_kW_m2 = si

            GeneratedPower = si * PanelArea * NumberOfPanels * PanelEfficiency / 100.0

            esout.EnergyFlow = GeneratedPower

        End Sub

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_SolarPanel With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_SolarPanel With {.SimObject = Me}
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

        Public Overrides Function GetProperties(proptype As PropertyType) As String()

            Select Case proptype
                Case PropertyType.ALL, PropertyType.RW, PropertyType.RO
                    Return New String() {"Efficiency", "User-Defined Solar Irradiation", "Actual Solar Irradiation", "Panel Area", "Number of Panels", "Generated Power"}
                Case PropertyType.WR
                    Return New String() {"Efficiency", "User-Defined Solar Irradiation", "Panel Area", "Number of Panels"}
            End Select

        End Function

        Public Overrides Function GetPropertyValue(prop As String, Optional su As IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    Return PanelEfficiency
                Case "User-Defined Solar Irradiation"
                    Return SolarIrradiation_kW_m2
                Case "Actual Solar Irradiation"
                    Return ActualSolarIrradiation_kW_m2
                Case "Panel Area"
                    Return PanelArea.ConvertFromSI(su.area)
                Case "Number of Panels"
                    Return NumberOfPanels
                Case "Generated Power"
                    Return GeneratedPower.ConvertFromSI(su.heatflow)
            End Select

        End Function

        Public Overrides Function GetPropertyUnit(prop As String, Optional su As IUnitsOfMeasure = Nothing) As String

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    Return "%"
                Case "User-Defined Solar Irradiation", "Actual Solar Irradiation"
                    Return "kW/m2"
                Case "Panel Area"
                    Return (su.area)
                Case "Number of Panels"
                    Return ""
                Case "Generated Power"
                    Return (su.heatflow)
            End Select

        End Function

        Public Overrides Function SetPropertyValue(prop As String, propval As Object, Optional su As IUnitsOfMeasure = Nothing) As Boolean

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    PanelEfficiency = propval
                Case "User-Defined Solar Irradiation"
                    SolarIrradiation_kW_m2 = propval
                Case "Panel Area"
                    PanelArea = Convert.ToDouble(propval).ConvertToSI(su.area)
                Case "Number of Panels"
                    NumberOfPanels = propval
            End Select

            Return True

        End Function

    End Class

End Namespace