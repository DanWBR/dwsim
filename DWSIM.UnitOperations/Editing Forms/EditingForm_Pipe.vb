Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports System.Drawing

Public Class EditingForm_Pipe

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.Pipe

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

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

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).ToArray

            cbEnergy.Items.Clear()
            cbEnergy.Items.AddRange(eslist)

            If .GraphicObject.EnergyConnector.IsAttached Then cbEnergy.SelectedItem = .GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag

            'parameters

            cbTemp.Items.Clear()
            cbTemp.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbTemp.SelectedItem = units.temperature

            cbPressure.Items.Clear()
            cbPressure.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbPressure.SelectedItem = units.pressure

            cbTtol.Items.Clear()
            cbTtol.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbTtol.SelectedItem = units.deltaT

            cbPtol.Items.Clear()
            cbPtol.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbPtol.SelectedItem = units.deltaP

            tbOutletTemperature.Text = su.Converter.ConvertFromSI(units.temperature, .OutletTemperature).ToString(nf)
            tbOutletPressure.Text = su.Converter.ConvertFromSI(units.pressure, .OutletPressure).ToString(nf)

            tbTtol.Text = su.Converter.ConvertFromSI(units.deltaT, .TolT).ToString(nf)
            tbPtol.Text = su.Converter.ConvertFromSI(units.deltaP, .TolP).ToString(nf)

            chkIncludeJT.Checked = .IncludeJTEffect

            cbCalcMode.SelectedIndex = .Specification

            cbPDropModel.SelectedIndex = .SelectedFlowPackage

            'profiles

            TabPage5.Controls.Clear()
            Dim heditor As New PipeHydraulicProfileEditor With {.PipeOp = Me.SimObject}
            heditor.Dock = DockStyle.Fill
            TabPage5.Controls.Add(heditor)

            TabPage6.Controls.Clear()
            Dim teditor As New PipeThermalProfileEditor With {.PipeOp = Me.SimObject}
            teditor.Dock = DockStyle.Fill
            TabPage6.Controls.Add(teditor)

            'results

            gridResults.Rows.Clear()
            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DeltaP"), su.Converter.ConvertFromSI(units.deltaP, .DeltaP.GetValueOrDefault).ToString(nf), units.deltaP})
            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DeltaT"), su.Converter.ConvertFromSI(units.deltaT, .DeltaT.GetValueOrDefault).ToString(nf), units.deltaT})
            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RConvPGridItem3"), su.Converter.ConvertFromSI(units.heatflow, .DeltaQ.GetValueOrDefault).ToString(nf), units.heatflow})

            TabPage2.Controls.Clear()
            Dim tview As New EditingForm_Pipe_ResultsTable With {.PipeOp = Me.SimObject}
            tview.Dock = DockStyle.Fill
            TabPage2.Controls.Add(tview)

            TabPage3.Controls.Clear()
            Dim cview As New EditingForm_Pipe_ResultsChart With {.PipeOp = Me.SimObject}
            cview.Dock = DockStyle.Fill
            TabPage3.Controls.Add(cview)

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.Add("Default")
            cbFlashAlg.Items.AddRange(flashalgos)
            If .PreferredFlashAlgorithmTag <> "" Then cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag Else cbFlashAlg.SelectedIndex = 0

        End With

        Loaded = True

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Thermodynamics.Calculator.ConfigureFlashInstance(SimObject, cbFlashAlg.SelectedItem.ToString)

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
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo)
            cbEnergy.SelectedItem = Nothing
        End If
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            SimObject.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedItem.ToString
            RequestCalc()
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

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If gobj.EnergyConnector.IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.EnergyConnector.AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

            End If

        End If

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbOutletTemperature.TextChanged, tbOutletTemperature.TextChanged, tbOutletPressure.TextChanged, tbTtol.TextChanged, tbPtol.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbOutletTemperature.KeyDown, tbOutletPressure.KeyDown, tbTtol.KeyDown, tbPtol.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        If sender Is tbOutletTemperature Then SimObject.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text.ParseExpressionToDouble)
        If sender Is tbOutletPressure Then SimObject.OutletPressure = su.Converter.ConvertToSI(cbPressure.SelectedItem.ToString, tbOutletPressure.Text.ParseExpressionToDouble)
        If sender Is tbTtol Then SimObject.TolT = su.Converter.ConvertToSI(cbTtol.SelectedItem.ToString, tbTtol.Text.ParseExpressionToDouble)
        If sender Is tbPtol Then SimObject.TolP = su.Converter.ConvertToSI(cbPtol.SelectedItem.ToString, tbPtol.Text.ParseExpressionToDouble)

        RequestCalc()

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

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.EnergyConnector.Position.X + 30, sgobj.EnergyConnector.Position.Y + 30, "")

            If sgobj.EnergyConnector.IsAttached Then fs.DisconnectObjects(sgobj, sgobj.EnergyConnector.AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub cbTemp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTemp.SelectedIndexChanged, cbPressure.SelectedIndexChanged, cbTtol.SelectedIndexChanged, cbPtol.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbTemp Then
                    tbOutletTemperature.Text = su.Converter.Convert(cbTemp.SelectedItem.ToString, units.temperature, Double.Parse(tbOutletTemperature.Text)).ToString(nf)
                    cbTemp.SelectedItem = units.temperature
                    UpdateProps(tbOutletTemperature)
                ElseIf sender Is cbPressure Then
                    tbOutletPressure.Text = su.Converter.Convert(cbPressure.SelectedItem.ToString, units.pressure, Double.Parse(tbOutletPressure.Text)).ToString(nf)
                    cbPressure.SelectedItem = units.pressure
                    UpdateProps(tbOutletPressure)
                ElseIf sender Is cbTtol Then
                    tbTtol.Text = su.Converter.Convert(cbTtol.SelectedItem.ToString, units.deltaT, Double.Parse(tbTtol.Text)).ToString(nf)
                    cbTtol.SelectedItem = units.deltaT
                    UpdateProps(tbTtol)
                ElseIf sender Is cbPtol Then
                    tbPtol.Text = su.Converter.Convert(cbPtol.SelectedItem.ToString, units.deltaP, Double.Parse(tbPtol.Text)).ToString(nf)
                    cbPtol.SelectedItem = units.deltaP
                    UpdateProps(tbPtol)
                End If
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Private Sub chkIncludeJT_CheckedChanged(sender As Object, e As EventArgs) Handles chkIncludeJT.CheckedChanged
        SimObject.IncludeJTEffect = chkIncludeJT.Checked
    End Sub

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged
        SimObject.Specification = cbCalcMode.SelectedIndex
        Select Case cbCalcMode.SelectedIndex
            Case 0
                tbOutletTemperature.Enabled = False
                tbOutletPressure.Enabled = False
                cbTemp.Enabled = False
                cbPressure.Enabled = False
            Case 1
                tbOutletTemperature.Enabled = False
                tbOutletPressure.Enabled = True
                cbTemp.Enabled = False
                cbPressure.Enabled = True
            Case 2
                tbOutletTemperature.Enabled = True
                tbOutletPressure.Enabled = False
                cbTemp.Enabled = True
                cbPressure.Enabled = False
        End Select
    End Sub

    Private Sub cbPDropModel_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPDropModel.SelectedIndexChanged
        SimObject.SelectedFlowPackage = cbPDropModel.SelectedIndex
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Select Case cbPDropModel.SelectedIndex
            Case 0
                Process.Start("http://www.ihsenergy.ca/support/documentation_ca/Harmony/content/html_files/reference_material/calculations_and_correlations/pressure_loss_calculations.htm#Beggs_and_Brill_Correlation")
            Case 1
                Process.Start("http://thermopedia.com/content/37/")
            Case 2
                Process.Start("http://www.ihsenergy.ca/support/documentation_ca/Harmony/content/html_files/reference_material/calculations_and_correlations/pressure_loss_calculations.htm#Petalas_and_Aziz_Mechanistic_Model")
        End Select
    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class