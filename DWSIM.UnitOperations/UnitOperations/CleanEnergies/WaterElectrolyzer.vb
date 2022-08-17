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

    Public Class WaterElectrolyzer

        Inherits CleanEnergyUnitOpBase

        Private ImagePath As String = ""

        Private Image As SKImage

        <Xml.Serialization.XmlIgnore> Public f As EditingForm_WaterElectrolyzer

        Public Overrides Function GetDisplayName() As String
            Return "Water Electrolyzer"
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return "Water Electrolyzer"
        End Function

        Public Overrides Property Prefix As String = "WE-"

        Public Property Voltage As Double

        Public Property NumberOfCells As Integer

        Public Property CellVoltage As Double

        Public Property WasteHeat As Double

        Public Property Current As Double

        Public Property ElectronTransfer As Double

        Public Overrides Function GetProperties(proptype As PropertyType) As String()

            Return New String() {"Voltage", "Number of Cells", "Cell Voltage", "Waste Heat", "Current", "Electron Transfer"}

        End Function

        Public Overrides Function GetPropertyValue(prop As String, Optional su As IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI()

            Select Case prop
                Case "Voltage"
                    Return Voltage
                Case "Number of Cells"
                    Return NumberOfCells
                Case "Cell Voltage"
                    Return CellVoltage
                Case "Waste Heat"
                    Return WasteHeat.ConvertFromSI(su.heatflow)
                Case "Current"
                    Return Current
                Case "Electron Transfer"
                    Return ElectronTransfer.ConvertFromSI(su.molarflow)
                Case Else
                    Return 0.0
            End Select

        End Function

        Public Overrides Function GetPropertyUnit(prop As String, Optional su As IUnitsOfMeasure = Nothing) As String

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI()

            Select Case prop
                Case "Voltage"
                    Return "V"
                Case "Number of Cells"
                    Return ""
                Case "Cell Voltage"
                    Return "V"
                Case "Waste Heat"
                    Return su.heatflow
                Case "Current"
                    Return "A"
                Case "Electron Transfer"
                    Return su.molarflow
                Case Else
                    Return 0.0
            End Select

        End Function

        Public Overrides Function SetPropertyValue(prop As String, propval As Object, Optional su As IUnitsOfMeasure = Nothing) As Boolean

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI()

            Select Case prop
                Case "Voltage"
                    Voltage = propval
                    Return True
                Case "Number of Cells"
                    NumberOfCells = propval
                    Return True
                Case Else
                    Return False
            End Select

        End Function

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub Draw(g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            If Image Is Nothing Then

                ImagePath = SharedClasses.Utility.GetTempFileName()
                My.Resources.electrolysis.Save(ImagePath)

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

            Dim myIC2 As New ConnectionPoint

            myIC2.Position = New Point(x + 0.5 * w, y + h)
            myIC2.Type = ConType.ConEn
            myIC2.Direction = ConDir.Up
            myIC2.Type = ConType.ConEn

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(x + w, y / 2)
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
                .Item(0).ConnectorName = "Products Outlet"
            End With

            Me.GraphicObject.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub PopulateEditorPanel(ctner As Object)


            Dim container As DynamicLayout = ctner

            Dim su = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            container.CreateAndAddTextBoxRow(nf, String.Format("Total Voltage ({0})", "V"), Voltage,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     Voltage = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

            container.CreateAndAddTextBoxRow(nf, "Number of Cells", NumberOfCells,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     NumberOfCells = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

        End Sub

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As CultureInfo, nf As String) As String

            Dim sb As New Text.StringBuilder()

            sb.AppendLine(String.Format("Number of Cells: {0}", NumberOfCells))

            sb.AppendLine()
            sb.AppendLine(String.Format("Cell Voltage: {0} V", CellVoltage.ToString(nf)))
            sb.AppendLine(String.Format("Current: {0} A", Current.ToString(nf)))
            sb.AppendLine(String.Format("Electron Transfer: {0} {1}", ElectronTransfer.ConvertFromSI(su.molarflow).ToString(nf), su.molarflow))
            sb.AppendLine()
            sb.AppendLine(String.Format("Waste Heat: {0} {1}", WasteHeat.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow))

            Return sb.ToString()

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_WaterElectrolyzer With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_WaterElectrolyzer With {.SimObject = Me}
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

        Public Overrides Sub CloseEditForm()

            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If

        End Sub

        Public Overrides Function ReturnInstance(typename As String) As Object

            Return New WaterElectrolyzer

        End Function

        Public Overrides Function GetIconBitmap() As Object

            Return My.Resources.electrolysis

        End Function

        Public Overrides Function CloneXML() As Object

            Dim obj As ICustomXMLSerialization = New WaterElectrolyzer()
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

            Dim esin = GetInletEnergyStream(1)

            Dim names = msin.Phases(0).Compounds.Keys.ToList()

            If Not names.Contains("Water") Then Throw New Exception("Needs Water compound")
            If Not names.Contains("Hydrogen") Then Throw New Exception("Needs Hydrogen compound")
            If Not names.Contains("Oxygen") Then Throw New Exception("Needs Oxygen compound")

            Current = esin.EnergyFlow.GetValueOrDefault() * 1000 / Voltage 'Ampere

            ElectronTransfer = Current / 96485.3365 * NumberOfCells 'mol/s

            Dim waterr = ElectronTransfer / 4 * 2 'mol/s
            Dim h2r = ElectronTransfer / 4 * 2 'mol/s
            Dim o2r = ElectronTransfer / 4 'mol/s

            Dim spO2 = 1.23 'V

            CellVoltage = Voltage / NumberOfCells

            If CellVoltage < spO2 Then Throw New Exception("Not enough power")

            Dim overV = CellVoltage - spO2

            WasteHeat = overV * Current * NumberOfCells / 1000.0 'kW

            Dim N0 = msin.Phases(0).Compounds.Values.Select(Function(c) c.MolarFlow.GetValueOrDefault()).ToList()

            Dim Nf = New List(Of Double)(N0)

            For i As Integer = 0 To N0.Count - 1
                If names(i) = "Water" Then
                    Nf(i) = N0(i) - waterr
                    If (Nf(i) < 0.0) Then Throw New Exception("Negative Water molar flow calculated")
                ElseIf names(i) = "Hydrogen" Then
                    Nf(i) = N0(i) + h2r
                ElseIf names(i) = "Oxygen" Then
                    Nf(i) = N0(i) + o2r
                End If
            Next

            msout.Clear()
            msout.ClearAllProps()

            msout.SetOverallComposition(Nf.ToArray().MultiplyConstY(1.0 / Nf.Sum))
            msout.SetMolarFlow(Nf.Sum)
            msout.SetPressure(msin.GetPressure)
            msout.SetMassEnthalpy(msin.GetMassEnthalpy() + WasteHeat / msin.GetMassFlow())
            msout.SetFlashSpec("PH")

            msout.AtEquilibrium = False

        End Sub


    End Class

End Namespace