Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports DWSIM.ExtensionMethods
Imports DWSIM.Inspector
Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations

Public Class EditingForm_AirCooler

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As AirCooler2

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_AirCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

        ChangeDefaultFont()

    End Sub

    Sub UpdateInfo()

        Loaded = False

        UpdateGHGEditor(TabPage4, SimObject)

        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        If Host.Items.Where(Function(x) x.Name.Contains(SimObject.GraphicObject.Tag)).Count > 0 Then
            If InspReportBar Is Nothing Then
                InspReportBar = New SharedClasses.InspectorReportBar
                InspReportBar.Dock = DockStyle.Bottom
                AddHandler InspReportBar.Button1.Click, Sub()
                                                            Dim iwindow As New Inspector.Window2
                                                            iwindow.SelectedObject = SimObject
                                                            iwindow.Show(DockPanel)
                                                        End Sub
                Me.Controls.Add(InspReportBar)
                InspReportBar.BringToFront()
            End If
        Else
            If InspReportBar IsNot Nothing Then
                Me.Controls.Remove(InspReportBar)
                InspReportBar = Nothing
            End If
        End If

        cbCalcMode.SelectedIndex = SimObject.CalculationMode

        Select Case cbCalcMode.SelectedIndex
            Case 0
                TabPage2.Enabled = False
                TabPage3.Enabled = True
                tbPressureDrop.ReadOnly = False
                tbOverallUA.ReadOnly = True
                tbOutletTemperature.ReadOnly = False
            Case 1
                TabPage2.Enabled = True
                TabPage3.Enabled = True
                tbPressureDrop.ReadOnly = True
                tbOverallUA.ReadOnly = True
                tbOutletTemperature.ReadOnly = True
            Case 2
                TabPage2.Enabled = False
                TabPage3.Enabled = True
                tbPressureDrop.ReadOnly = False
                tbOverallUA.ReadOnly = False
                tbOutletTemperature.ReadOnly = True
        End Select

        Dim su = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        lbTubeDe.Text = su.diameter
        lbuTubeDi.Text = su.diameter
        lbTubeRoughness.Text = su.diameter
        lbuTubeFoulingFactor.Text = su.foulingfactor
        lbuTubeLength.Text = su.distance
        lbuTubePitch.Text = su.thickness
        lbuTubeThermalCond.Text = su.thermalConductivity

        lblActualAirFlow.Text = su.volumetricFlow
        lblElectricalLoad.Text = su.heatflow
        lblHeatExchanged.Text = su.heatflow
        lblInletAirPressure.Text = su.pressure
        lblInletAirTemperature.Text = su.temperature
        lblMaximumHeatExchange.Text = su.heatflow
        lblOutletAirTemperature.Text = su.temperature
        lblOutletTemperature.Text = su.temperature
        lblPressureDrop.Text = su.deltaP
        lblRefAirFlow.Text = su.volumetricFlow

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            Me.Text = .GraphicObject.Tag & " (" & .GetDisplayName() & ")"

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = System.Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = System.Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
                    lblStatus.ForeColor = System.Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = System.Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            'connections

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).OrderBy(Function(m) m).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).OrderBy(Function(m) m).ToArray()

            cbEnergy.Items.Clear()
            cbEnergy.Items.AddRange(eslist)

            If .GraphicObject.InputConnectors(1).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'parameters

            tbActualAF.Text = .ActualAirFlow.ConvertFromSI(su.volumetricFlow).ToString(nf)
            tbActualR.Text = .ActualFanSpeed.ToString(nf)
            tbEff.Text = .ExchangerEfficiency.ToString(nf)
            tbElecConv.Text = .ElectricalPowerConversionFactor.ToString(nf)
            tbElecLoad.Text = .ElectricalPowerLoad.ConvertFromSI(su.heatflow).ToString(nf)
            tbHeatingChange.Text = .HeatLoad.ConvertFromSI(su.heatflow).ToString(nf)
            tbInletAirPre.Text = .AirPressure.ConvertFromSI(su.pressure).ToString(nf)
            tbInletAirTemp.Text = .AirInletTemperature.ConvertFromSI(su.temperature).ToString(nf)
            tbMaxHeatEx.Text = .MaxHeatExchange.ConvertFromSI(su.heatflow).ToString(nf)
            tbOutletAirTemp.Text = .AirOutletTemperature.ConvertFromSI(su.temperature).ToString(nf)
            tbOutletTemperature.Text = .OutletTemperature.ConvertFromSI(su.temperature).ToString(nf)
            tbOverallUA.Text = .OverallUA.ToString(nf)
            tbPressureDrop.Text = .PressureDrop.ConvertFromSI(su.deltaP).ToString(nf)
            tbRefAF.Text = .ReferenceAirFlow.ConvertFromSI(su.volumetricFlow).ToString(nf)
            tbRefR.Text = .ReferenceFanSpeed.ToString(nf)

            tbNumberOfTubesPerShell.Text = .Tube_NumberPerShell
            tbTubeDe.Text = Format(cv.ConvertFromSI(su.diameter, .Tube_De / 1000.0), nf)
            tbTubeDi.Text = Format(cv.ConvertFromSI(su.diameter, .Tube_Di / 1000.0), nf)
            tbTubeFoulingFactor.Text = cv.ConvertFromSI(su.foulingfactor, .Tube_Fouling)
            tbTubeLength.Text = Format(cv.ConvertFromSI(su.distance, .Tube_Length), nf)
            tbTubePassesPerShell.Text = .Tube_PassesPerShell
            tbTubePitch.Text = Format(cv.ConvertFromSI(su.thickness, .Tube_Pitch / 1000.0), nf)
            tbTubeRoughness.Text = cv.ConvertFromSI(su.diameter, .Tube_Roughness / 1000.0)
            tbTubeThermalCond.Text = cv.ConvertFromSI(su.thermalConductivity, .Tube_ThermalConductivity)

            rbUseGlobal.Checked = SimObject.UseGlobalWeather

            tbInletAirPre.Enabled = Not SimObject.UseGlobalWeather
            tbInletAirTemp.Enabled = Not SimObject.UseGlobalWeather

        End With

        Loaded = True

    End Sub

    Sub UpdateProps()

        SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

        Dim su = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        With SimObject

            .ActualFanSpeed = tbActualR.Text.ParseExpressionToDouble
            .ElectricalPowerConversionFactor = tbElecConv.Text.ParseExpressionToDouble
            .HeatLoad = tbHeatingChange.Text.ParseExpressionToDouble.ConvertToSI(su.heatflow)
            .AirPressure = tbInletAirPre.Text.ParseExpressionToDouble.ConvertToSI(su.pressure)
            .AirInletTemperature = tbInletAirTemp.Text.ParseExpressionToDouble.ConvertToSI(su.temperature)
            .OutletTemperature = tbOutletTemperature.Text.ParseExpressionToDouble.ConvertToSI(su.temperature)
            .OverallUA = tbOverallUA.Text.ParseExpressionToDouble
            .PressureDrop = tbPressureDrop.Text.ParseExpressionToDouble.ConvertToSI(su.deltaP)
            .ReferenceAirFlow = tbRefAF.Text.ParseExpressionToDouble.ConvertToSI(su.volumetricFlow)
            .ReferenceFanSpeed = tbRefR.Text.ParseExpressionToDouble

            .Tube_NumberPerShell = tbNumberOfTubesPerShell.Text.ParseExpressionToDouble
            .Tube_De = cv.ConvertToSI(su.diameter, tbTubeDe.Text.ParseExpressionToDouble) * 1000.0
            .Tube_Di = cv.ConvertToSI(su.diameter, tbTubeDi.Text.ParseExpressionToDouble) * 1000.0
            .Tube_Roughness = cv.ConvertToSI(su.diameter, tbTubeRoughness.Text.ParseExpressionToDouble) * 1000.0
            .Tube_Fouling = cv.ConvertToSI(su.foulingfactor, tbTubeFoulingFactor.Text.ParseExpressionToDouble)
            .Tube_Length = cv.ConvertToSI(su.distance, tbTubeLength.Text.ParseExpressionToDouble)
            .Tube_PassesPerShell = tbTubePassesPerShell.Text.ParseExpressionToDouble
            .Tube_Pitch = cv.ConvertToSI(su.thickness, tbTubePitch.Text.ParseExpressionToDouble) * 1000.0
            .Tube_ThermalConductivity = cv.ConvertToSI(su.thermalConductivity, tbTubeThermalCond.Text.ParseExpressionToDouble)

        End With

    End Sub

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged

        If Loaded Then

            Select Case cbCalcMode.SelectedIndex
                Case 0
                    TabPage2.Enabled = False
                    TabPage3.Enabled = True
                    tbPressureDrop.ReadOnly = False
                    tbOverallUA.ReadOnly = True
                    tbOutletTemperature.ReadOnly = False
                Case 1
                    TabPage2.Enabled = True
                    TabPage3.Enabled = True
                    tbPressureDrop.ReadOnly = True
                    tbOverallUA.ReadOnly = True
                    tbOutletTemperature.ReadOnly = True
                Case 2
                    TabPage2.Enabled = False
                    TabPage3.Enabled = True
                    tbPressureDrop.ReadOnly = False
                    tbOverallUA.ReadOnly = False
                    tbOutletTemperature.ReadOnly = True
            End Select
            SimObject.CalculationMode = cbCalcMode.SelectedIndex

        End If

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click
        If cbInlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet1.Click
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectEnergy_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy.Click
        If cbEnergy.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbEnergy.SelectedItem = Nothing
        End If
    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbInlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

            End If

        End If

    End Sub

    Private Sub cbOutlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

            End If

        End If

    End Sub

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

            End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click, btnCreateAndConnectEnergy.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        If sender Is btnCreateAndConnectInlet1 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(0).Position.X - 50, sgobj.InputConnectors(0).Position.Y, "")

            If sgobj.InputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(0).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 0)

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(0).Position.X + 30, sgobj.OutputConnectors(0).Position.Y, "")

            If sgobj.OutputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(0).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

        ElseIf sender Is btnCreateAndConnectEnergy Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(1).Position.X - 30, sgobj.InputConnectors(1).Position.Y + 30, "")

            If sgobj.InputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(1).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 1)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout)

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation3(SimObject, False)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbActualR.TextChanged, tbElecConv.TextChanged,
        tbInletAirPre.TextChanged, tbInletAirTemp.TextChanged, tbNumberOfTubesPerShell.TextChanged,
        tbOutletTemperature.TextChanged, tbOverallUA.TextChanged, tbPressureDrop.TextChanged, tbRefAF.TextChanged,
        tbRefR.TextChanged, tbTubeDe.TextChanged, tbTubeDi.TextChanged, tbTubeFoulingFactor.TextChanged,
        tbTubeLength.TextChanged, tbTubePassesPerShell.TextChanged, tbTubePitch.TextChanged, tbTubeRoughness.TextChanged,
        tbTubeThermalCond.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbActualR.KeyDown, tbElecConv.KeyDown,
        tbInletAirPre.KeyDown, tbInletAirTemp.KeyDown, tbNumberOfTubesPerShell.KeyDown,
        tbOutletTemperature.KeyDown, tbOverallUA.KeyDown, tbPressureDrop.KeyDown, tbRefAF.KeyDown,
        tbRefR.KeyDown, tbTubeDe.KeyDown, tbTubeDi.KeyDown, tbTubeFoulingFactor.KeyDown,
        tbTubeLength.KeyDown, tbTubePassesPerShell.KeyDown, tbTubePitch.KeyDown, tbTubeRoughness.KeyDown,
        tbTubeThermalCond.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            Try
                UpdateProps()
                DirectCast(sender, TextBox).SelectAll()
                RequestCalc()
            Catch ex As Exception
                MessageBox.Show("Error updating property. TextBox ID: " & DirectCast(sender, TextBox).Name & ". Error message: " & ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try

        End If

    End Sub

    Private Sub rbUserDef_CheckedChanged(sender As Object, e As EventArgs) Handles rbUserDef.CheckedChanged, rbUseGlobal.CheckedChanged
        If Loaded Then
            SimObject.UseGlobalWeather = rbUseGlobal.Checked
            tbInletAirPre.Enabled = Not SimObject.UseGlobalWeather
            tbInletAirTemp.Enabled = Not SimObject.UseGlobalWeather
        End If
    End Sub

End Class