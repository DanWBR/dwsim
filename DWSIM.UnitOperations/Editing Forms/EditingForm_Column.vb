Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports System.Drawing

Public Class EditingForm_Column

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As UnitOperations.Column

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

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            Me.Text = .GetDisplayName() & ": " & .GraphicObject.Tag

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
                    lblStatus.ForeColor = Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            'connections

            'parameters


            'profiles

            'TabPage5.Controls.Clear()
            'Dim heditor As New PipeHydraulicProfileEditor With {.PipeOp = Me.SimObject}
            'heditor.Dock = DockStyle.Fill
            'TabPage5.Controls.Add(heditor)

            'TabPage6.Controls.Clear()
            'Dim teditor As New PipeThermalProfileEditor With {.PipeOp = Me.SimObject}
            'teditor.Dock = DockStyle.Fill
            'TabPage6.Controls.Add(teditor)

            'results

            'gridResults.Rows.Clear()
            'gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DeltaP"), su.Converter.ConvertFromSI(units.deltaP, .DeltaP.GetValueOrDefault).ToString(nf), units.deltaP})
            'gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DeltaT"), su.Converter.ConvertFromSI(units.deltaT, .DeltaT.GetValueOrDefault).ToString(nf), units.deltaT})
            'gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("RConvPGridItem3"), su.Converter.ConvertFromSI(units.heatflow, .DeltaQ.GetValueOrDefault).ToString(nf), units.heatflow})

            'TabPage2.Controls.Clear()
            'Dim tview As New EditingForm_Pipe_ResultsTable With {.PipeOp = Me.SimObject}
            'tview.Dock = DockStyle.Fill
            'TabPage2.Controls.Add(tview)

            'TabPage3.Controls.Clear()
            'Dim cview As New EditingForm_Pipe_ResultsChart With {.PipeOp = Me.SimObject}
            'cview.Dock = DockStyle.Fill
            'TabPage3.Controls.Add(cview)

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.AddRange(flashalgos)
            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag

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
        If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
        Me.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
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

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs)

        Dim tbox = DirectCast(sender, TextBox)

        If Double.TryParse(tbox.Text, New Double()) Then
            tbox.ForeColor = Drawing.Color.Blue
        Else
            tbox.ForeColor = Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbOutletPressure.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        'If sender Is tbOutletTemperature Then SimObject.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text)
        'If sender Is tbOutletPressure Then SimObject.OutletPressure = su.Converter.ConvertToSI(cbPressure.SelectedItem.ToString, tbOutletPressure.Text)

        RequestCalc()

    End Sub

    Private Sub cbTemp_SelectedIndexChanged(sender As Object, e As EventArgs)

        'If Loaded Then
        '    Try
        '        If sender Is cbTemp Then
        '            tbOutletTemperature.Text = su.Converter.Convert(cbTemp.SelectedItem.ToString, units.temperature, Double.Parse(tbOutletTemperature.Text)).ToString(nf)
        '            cbTemp.SelectedItem = units.temperature
        '            UpdateProps(tbOutletTemperature)
        '        ElseIf sender Is cbPressure Then
        '            tbOutletPressure.Text = su.Converter.Convert(cbPressure.SelectedItem.ToString, units.pressure, Double.Parse(tbOutletPressure.Text)).ToString(nf)
        '            cbPressure.SelectedItem = units.pressure
        '            UpdateProps(tbOutletPressure)
        '        End If
        '    Catch ex As Exception
        '        SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
        '    End Try
        'End If

    End Sub

End Class