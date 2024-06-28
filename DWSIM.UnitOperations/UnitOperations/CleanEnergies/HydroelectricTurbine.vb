Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations
Imports Eto.Forms
Imports DWSIM.UI.Shared.Common
Imports SkiaSharp
Imports System.Globalization

Namespace UnitOperations

    Public Class HydroelectricTurbine

        Inherits CleanEnergyUnitOpBase

        Private ImagePath As String = ""

        Private Image As SKImage

        <Xml.Serialization.XmlIgnore> Public f As EditingForm_HydroelectricTurbine

        Public Overrides Property Prefix As String = "HT-"

        Public Property Efficiency As Double = 75

        Public Property StaticHead As Double = 1.0

        Public Property VelocityHead As Double = 1.0

        Public Property InletVelocity As Double = 1.0

        Public Property OutletVelocity As Double = 0.5

        Public Property TotalHead As Double = 0.0

        Public Property GeneratedPower As Double = 0.0

        Public Overrides Function GetDisplayName() As String
            Return "Hydroelectric Turbine"
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return "Hydroelectric Turbine"
        End Function

        Public Sub New()

            MyBase.New()

        End Sub


        Public Overrides Sub Draw(g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            If Image Is Nothing Then

                ImagePath = SharedClasses.Utility.GetTempFileName()
                My.Resources.icons8_hydroelectric.Save(ImagePath)

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

            Dim myIC1 As New ConnectionPoint

            myIC1.Position = New Point(x, y + h / 2)
            myIC1.Type = ConType.ConIn
            myIC1.Direction = ConDir.Right

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(x + w, y + h / 2)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(x + w / 2, y + h)
            myOC2.Type = ConType.ConOut
            myOC2.Direction = ConDir.Down
            myOC2.Type = ConType.ConEn

            With GraphicObject.InputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x, y + h / 2)
                Else
                    .Add(myIC1)
                End If
                .Item(0).ConnectorName = "Water Inlet"
            End With

            With GraphicObject.OutputConnectors
                If .Count = 2 Then
                    .Item(0).Position = New Point(x + w, y + h / 2)
                    .Item(1).Position = New Point(x + w / 2, y + h)
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                End If
                .Item(0).ConnectorName = "Water Outlet"
                .Item(1).ConnectorName = "Power Outlet"
            End With

            Me.GraphicObject.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub PopulateEditorPanel(ctner As Object)


            Dim container As DynamicLayout = ctner

            Dim su = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            container.CreateAndAddTextBoxRow(nf, String.Format("Static Head ({0})", su.distance),
                                             StaticHead, Sub(tb, e)
                                                             If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                                 StaticHead = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.distance)
                                                             End If
                                                         End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Inlet Velocity ({0})", su.velocity),
                                             InletVelocity, Sub(tb, e)
                                                                If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                                    InletVelocity = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.velocity)
                                                                End If
                                                            End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Outlet Velocity ({0})", su.velocity),
                                             OutletVelocity, Sub(tb, e)
                                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                                     OutletVelocity = tb.Text.ToDoubleFromInvariant().ConvertToSI(su.velocity)
                                                                 End If
                                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, String.Format("Efficiency ({0})", "%"),
                                             Efficiency, Sub(tb, e)
                                                             If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                                 Efficiency = tb.Text.ToDoubleFromInvariant()
                                                             End If
                                                         End Sub)

        End Sub

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Velocity Head",
                           Me.VelocityHead.ConvertFromSI(su.velocity).ToString(nf),
                           su.velocity}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Total Head",
                           Me.TotalHead.ConvertFromSI(su.velocity).ToString(nf),
                           su.velocity}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                           New String() {"Generated Power",
                           Me.GeneratedPower.ConvertFromSI(su.heatflow).ToString(nf),
                           su.heatflow}))

            Return list

        End Function

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As CultureInfo, numberformat As String) As String

            Dim sb As New Text.StringBuilder()

            sb.AppendLine(String.Format("Velocity Head: {0} {1}", VelocityHead.ConvertFromSI(su.velocity).ToString(numberformat), su.velocity))
            sb.AppendLine(String.Format("Total Head: {0} {1}", TotalHead.ConvertFromSI(su.velocity).ToString(numberformat), su.velocity))
            sb.AppendLine(String.Format("Generated Power: {0} {1}", GeneratedPower.ConvertFromSI(su.heatflow).ToString(numberformat), su.heatflow))

            Return sb.ToString()

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_HydroelectricTurbine With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_HydroelectricTurbine With {.SimObject = Me}
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

            Return New HydroelectricTurbine

        End Function

        Public Overrides Function GetIconBitmap() As Object

            Return My.Resources.icons8_hydroelectric

        End Function

        Public Overrides Function CloneXML() As Object

            Dim obj As ICustomXMLSerialization = New HydroelectricTurbine()
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

            Dim msin = GetInletMaterialStream(0)
            Dim msout = GetOutletMaterialStream(0)

            Dim esout = GetOutletEnergyStream(1)

            Dim eta = Efficiency / 100.0

            Dim q = msin.GetVolumetricFlow()

            Dim rho = msin.GetPhase("Liquid").Properties.density.GetValueOrDefault()

            Dim g = 9.8

            Dim hs = StaticHead

            Dim hv = (InletVelocity ^ 2 - OutletVelocity ^ 2) / (2 * g)

            VelocityHead = hv

            TotalHead = hs + hv

            Dim p = eta * rho * g * (hs + hv) * q

            GeneratedPower = p / 1000.0

            esout.EnergyFlow = GeneratedPower

            msout.Clear()
            msout.ClearAllProps()

            msout.AssignFromPhase(Enums.PhaseLabel.Mixture, msin, True)
            msout.SetPressure(msin.GetPressure)
            msout.SetMassEnthalpy(msin.GetMassEnthalpy() - GeneratedPower / msin.GetMassFlow())
            msout.SetFlashSpec("PH")

            msout.AtEquilibrium = False

        End Sub

        Public Overrides Function GetProperties(proptype As PropertyType) As String()

            Select Case proptype
                Case PropertyType.ALL, PropertyType.RW, PropertyType.RO
                    Return New String() {"Efficiency", "Static Head", "Velocity Head", "Total Head", "Inlet Velocity", "Outlet Velocity", "Generated Power"}
                Case PropertyType.WR
                    Return New String() {"Efficiency", "Static Head", "Inlet Velocity", "Outlet Velocity"}
            End Select

        End Function

        Public Overrides Function GetPropertyValue(prop As String, Optional su As IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    Return Efficiency
                Case "Static Head"
                    Return StaticHead.ConvertFromSI(su.distance)
                Case "Velocity Head"
                    Return VelocityHead.ConvertFromSI(su.distance)
                Case "Total Head"
                    Return TotalHead.ConvertFromSI(su.distance)
                Case "Inlet Velocity"
                    Return InletVelocity.ConvertFromSI(su.velocity)
                Case "Outlet Velocity"
                    Return OutletVelocity.ConvertFromSI(su.velocity)
                Case "Generated Power"
                    Return GeneratedPower.ConvertFromSI(su.heatflow)
            End Select

        End Function

        Public Overrides Function GetPropertyUnit(prop As String, Optional su As IUnitsOfMeasure = Nothing) As String

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    Return ""
                Case "Static Head"
                    Return (su.distance)
                Case "Velocity Head"
                    Return (su.distance)
                Case "Total Head"
                    Return (su.distance)
                Case "Inlet Velocity"
                    Return (su.velocity)
                Case "Outlet Velocity"
                    Return (su.velocity)
                Case "Generated Power"
                    Return (su.heatflow)
            End Select

        End Function

        Public Overrides Function SetPropertyValue(prop As String, propval As Object, Optional su As IUnitsOfMeasure = Nothing) As Boolean

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI

            Select Case prop
                Case "Efficiency"
                    Efficiency = propval
                Case "Static Head"
                    StaticHead = Convert.ToDouble(propval).ConvertToSI(su.distance)
                Case "Inlet Velocity"
                    InletVelocity = Convert.ToDouble(propval).ConvertToSI(su.velocity)
                Case "Outlet Velocity"
                    OutletVelocity = Convert.ToDouble(propval).ConvertToSI(su.velocity)
            End Select

            Return True

        End Function

    End Class

End Namespace