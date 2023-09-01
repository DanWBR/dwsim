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

        Public Property ThermoNeutralVoltage As Double

        Public Property ReversibleVoltage As Double

        Public Property Efficiency As Double

        Public Property InputEfficiency As Double

        Public Overrides Function GetProperties(proptype As PropertyType) As String()

            Return New String() {"Voltage", "Thermoneutral Voltage", "Reversible Voltage", "Number of Cells", "Cell Voltage", "Waste Heat", "Current", "Electron Transfer", "Efficiency", "Input Efficiency"}

        End Function

        Public Overrides Function GetPropertyValue(prop As String, Optional su As IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI()

            Select Case prop
                Case "Voltage"
                    Return Voltage
                Case "Thermoneutral Voltage"
                    Return ThermoNeutralVoltage
                Case "Reversible Voltage"
                    Return ReversibleVoltage
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
                Case "Efficiency"
                    Return Efficiency
                Case "Input Efficiency"
                    Return InputEfficiency
                Case Else
                    Return 0.0
            End Select

        End Function

        Public Overrides Function GetPropertyUnit(prop As String, Optional su As IUnitsOfMeasure = Nothing) As String

            If su Is Nothing Then su = New SharedClasses.SystemsOfUnits.SI()

            Select Case prop
                Case "Voltage", "Thermoneutral Voltage", "Reversible Voltage", "Cell Voltage"
                    Return "V"
                Case "Number of Cells"
                    Return ""
                Case "Waste Heat"
                    Return su.heatflow
                Case "Current"
                    Return "A"
                Case "Electron Transfer"
                    Return su.molarflow
                Case "Efficiency"
                    Return ""
                Case "Input Efficiency"
                    Return ""
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
                Case "Input Efficiency"
                    InputEfficiency = propval
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
            myOC1.Position = New Point(x + w, y / 3)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(x + w, 2 * y / 3)
            myOC2.Type = ConType.ConOut
            myOC2.Direction = ConDir.Right

            With GraphicObject.InputConnectors
                If .Count = 2 Then
                    .Item(0).Position = New Point(x, y + h / 2)
                    .Item(1).Position = New Point(x + 0.5 * w, y + h)
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                End If
                .Item(0).ConnectorName = "Water Inlet"
                .Item(1).ConnectorName = "Power Inlet"
            End With

            With GraphicObject.OutputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x + w, y + h / 2)
                    .Add(myOC2)
                ElseIf .Count = 2 Then
                    .Item(0).Position = New Point(x + w, y + h / 3)
                    .Item(1).Position = New Point(x + w, y + 2 * h / 3)
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                End If
                .Item(0).ConnectorName = "Hydrogen-Rich Outlet"
                .Item(1).ConnectorName = "Oxygen-Rich Outlet"
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
            container.CreateAndAddTextBoxRow(nf, "Efficiency", InputEfficiency,
                                             Sub(tb, e)
                                                 If tb.Text.ToDoubleFromInvariant().IsValidDouble() Then
                                                     InputEfficiency = tb.Text.ToDoubleFromInvariant()
                                                 End If
                                             End Sub)

        End Sub

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As CultureInfo, nf As String) As String

            Dim sb As New Text.StringBuilder()

            sb.AppendLine(String.Format("Number of Cells: {0}", NumberOfCells))

            sb.AppendLine()
            sb.AppendLine(String.Format("Cell Voltage: {0} V", CellVoltage.ToString(nf)))
            sb.AppendLine(String.Format("Current: {0} A", Current.ToString(nf)))
            sb.AppendLine(String.Format("Efficiency: {0} A", Efficiency.ToString(nf)))
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
            Dim msout1 = GetOutletMaterialStream(0)
            Dim msout2 = GetOutletMaterialStream(1)

            If msout2 Is Nothing Then
                Throw New Exception("Please update your model and connect a second outlet stream to this electrolyzer.")
            End If

            Dim esin = GetInletEnergyStream(1)

            Dim names = msin.Phases(0).Compounds.Keys.ToList()

            Dim wid, hid As String

            If names.Contains("HeavyWater") Then
                If Not names.Contains("Deuterium") Then Throw New Exception("Needs Deuterium compound.")
                wid = "HeavyWater"
                hid = "Deuterium"
            Else
                If Not names.Contains("Water") Then Throw New Exception("Needs Water compound.")
                If Not names.Contains("Hydrogen") Then Throw New Exception("Needs Hydrogen compound.")
                wid = "Water"
                hid = "Hydrogen"
            End If

            If Not names.Contains("Oxygen") Then Throw New Exception("Needs Oxygen compound.")

            Dim pp = DirectCast(PropertyPackage, Thermodynamics.PropertyPackages.PropertyPackage)

            pp.CurrentMaterialStream = msin

            Dim T = msin.GetTemperature()

            'https://www.researchgate.net/publication/267979954_Integral_Characteristics_of_Hydrogen_Production_in_Alkaline_Electrolysers

            Dim DGf = pp.AUX_DELGig_RT(298.15, T, New String() {wid, hid, "Oxygen"}, New Double() {-1.0, 1.0, 0.5}, 0) * 8.314 * T / 1000
            Dim DHf = pp.AUX_DELHig_RT(298.15, T, New String() {wid, hid, "Oxygen"}, New Double() {-1.0, 1.0, 0.5}, 0) * 8.314 * T / 1000

            Dim mw = msin.Phases(0).Compounds(wid).ConstantProperties.Molar_Weight

            Dim DHvap As Double

            DHvap = pp.AUX_HVAPi(wid, T) * mw / 1000.0

            DHf += DHvap
            ' DGf for liquid water, DGf = DHf + T * DSf, S(water,liq) = , S(O2) = 205.15, S(H2) = 130.68
            ' Data from NIST Chemistry Webbook, https://webbook.nist.gov/
            Dim S_water = -203.606 * Math.Log(T / 1000) + 1523.29 * T / 1000 - 3196.413 * (T / 1000) ^ 2 / 2 + 2474.455 * (T / 1000) ^ 3 / 3 - 3.855326 / (2 * (T / 1000) ^ 2) - 488.7163
            Dim S_hydrogen = 33.066178 * Math.Log(T / 1000) - 11.363417 * T / 1000 + 11.432816 * (T / 1000) ^ 2 / 2 - 2.772874 * (T / 1000) ^ 3 / 3 + 0.158558 / (2 * (T / 1000) ^ 2) + 172.707974
            Dim S_oxygen = 31.32234 * Math.Log(T / 1000) - 20.23531 * T / 1000 + 57.86644 * (T / 1000) ^ 2 / 2 - 36.50624 * (T / 1000) ^ 3 / 3 + 0.007374 / (2 * (T / 1000) ^ 2) + 246.7945
            DGf = DHf + T * (S_water - (0.5 * S_oxygen + S_hydrogen)) / 1000

            Dim Vrev = DGf * 1000.0 / (2.0 * 96485.3365)
            Dim Vth = DHf * 1000.0 / (2.0 * 96485.3365)

            ThermoNeutralVoltage = Vth

            ReversibleVoltage = Vrev

            Dim waterr As Double
            Dim h2r As Double
            Dim o2r As Double

            If Voltage > 0 And NumberOfCells > 0 Then

                Current = esin.EnergyFlow.GetValueOrDefault() * 1000 / Voltage 'Ampere

                ElectronTransfer = Current / 96485.3365 * NumberOfCells 'mol/s

                waterr = ElectronTransfer / 4 * 2 'mol/s
                h2r = ElectronTransfer / 4 * 2 'mol/s
                o2r = ElectronTransfer / 4 'mol/s
                CellVoltage = Voltage / NumberOfCells
                If CellVoltage < Vrev Then Throw New Exception("Total Voltage too low.")

                Dim overV = CellVoltage - Vth

                WasteHeat = overV * Current * NumberOfCells / 1000.0 'kW


            ElseIf InputEfficiency > 0 And InputEfficiency <= 1.0 Then

                Dim reaction_heat As Double

                reaction_heat = InputEfficiency * esin.EnergyFlow.GetValueOrDefault()
                WasteHeat = (1 - InputEfficiency) * esin.EnergyFlow.GetValueOrDefault()

                waterr = reaction_heat / DHf
                h2r = reaction_heat / DHf
                o2r = 0.5 * reaction_heat / DHf

                CellVoltage = ThermoNeutralVoltage / InputEfficiency
                Voltage = 0
                ElectronTransfer = 2 * waterr
                Current = 0

            Else

                Throw New Exception(String.Format("Specify total voltage and number of cells or set both to zero and specify efficiency between 0 and 1"))

            End If

            Efficiency = (esin.EnergyFlow.GetValueOrDefault() - WasteHeat) / esin.EnergyFlow.GetValueOrDefault()

            Dim N0 = msin.Phases(0).Compounds.Values.Select(Function(c) c.MolarFlow.GetValueOrDefault()).ToList()

            Dim Nf = New List(Of Double)(N0)

            Dim widx, hidx, oidx As Integer

            For i As Integer = 0 To N0.Count - 1
                If names(i) = wid Then
                    widx = i
                    Nf(i) = N0(i) - waterr
                    If (Nf(i) < 0.0) Then Throw New Exception(String.Format("Negative {0} molar flow calculated. Increase water rate in inlet stream or reduce power.", wid))
                ElseIf names(i) = hid Then
                    hidx = i
                    Nf(i) = N0(i) + h2r
                ElseIf names(i) = "Oxygen" Then
                    oidx = i
                    Nf(i) = N0(i) + o2r
                End If
            Next

            Dim P = msin.GetPressure()

            Dim NH2 = Nf(hidx)
            Dim xH2Osat = pp.AUX_PVAPi(wid, T) / P
            Dim xH2 = 1 - xH2Osat
            Dim Ntot = NH2 / xH2
            Dim NH20sat = Ntot - NH2

            msout1.Clear()
            msout1.ClearAllProps()
            msout1.SetOverallCompoundMolarFlow(hid, NH2)
            msout1.SetOverallCompoundMolarFlow(wid, NH20sat)
            msout1.SetPressure(P)
            msout1.SetTemperature(T)
            msout1.SetFlashSpec("PT")
            msout1.Calculate()
            msout1.SetMassEnthalpy(msout1.GetMassEnthalpy())
            msout1.SetFlashSpec("PH")

            msout1.AtEquilibrium = False

            Nf(hidx) = 0.0
            Nf(widx) -= NH20sat

            If (Nf(widx) < 0.0) Then Throw New Exception("Negative Water molar flow calculated. Increase water rate in inlet stream or reduce power.")

            msout2.Clear()
            msout2.ClearAllProps()
            msout2.SetOverallComposition(Nf.ToArray().MultiplyConstY(1.0 / Nf.Sum))
            msout2.SetMolarFlow(Nf.Sum)
            msout2.SetPressure(P)
            msout2.SetTemperature(T)
            msout2.SetFlashSpec("PT")
            msout2.Calculate()
            msout2.SetMassEnthalpy(msout2.GetMassEnthalpy() + WasteHeat / msin.GetMassFlow())
            msout2.SetFlashSpec("PH")

            msout2.AtEquilibrium = False

        End Sub


    End Class

End Namespace