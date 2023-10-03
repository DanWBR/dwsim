Imports System.Drawing
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations

Public Class EditingForm_WindTurbine

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As WindTurbine

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_WindTurbine_Load(sender As Object, e As EventArgs) Handles Me.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

        ChangeDefaultFont()

    End Sub

    Sub UpdateInfo()

        Loaded = False

        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem

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

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).OrderBy(Function(m) m).ToArray()

            cbEnergy.Items.Clear()
            cbEnergy.Items.AddRange(eslist)

            If .GraphicObject.OutputConnectors(0).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            ' properties

            chkUseGlobalIrr.Checked = Not .UseUserDefinedWeather

            tbWindSpeed.Enabled = .UseUserDefinedWeather
            tbAtmTemp.Enabled = .UseUserDefinedWeather
            tbAtmPres.Enabled = .UseUserDefinedWeather
            tbRelHum.Enabled = .UseUserDefinedWeather

            lblDistanceUnits.Text = units.distance
            lblAirDensityUnits.Text = units.density
            lblPowerUnits.Text = units.heatflow
            lblAtmPres.Text = units.pressure
            lblAtmTemp.Text = units.temperature
            lblMaxPowerUnits.Text = units.heatflow
            lblWindSpeed.Text = units.velocity

            tbWindSpeed.Text = .UserDefinedWindSpeed.ConvertFromSI(units.velocity).ToString(nf)

            tbPanelEfficiency.Text = .Efficiency.ToString(nf)

            tbRotorDiameter.Text = .RotorDiameter.ConvertFromSI(units.distance).ToString(nf)

            tbNumberOfPanels.Text = .NumberOfTurbines

            tbGenPower.Text = .GeneratedPower.ConvertFromSI(units.heatflow).ToString(nf)

            tbMaxPower.Text = .MaximumTheoreticalPower.ConvertFromSI(units.heatflow).ToString(nf)

            tbAirDensity.Text = .AirDensity.ConvertFromSI(units.density).ToString(nf)

            tbAtmPres.Text = .UserDefinedAirPressure.ConvertFromSI(units.pressure).ToString(nf)

            tbAtmTemp.Text = .UserDefinedAirTemperature.ConvertFromSI(units.temperature).ToString(nf)

            tbRelHum.Text = .UserDefinedRelativeHumidity.ToString(nf)

        End With

        Loaded = True

    End Sub

    Private Sub chkUseGlobalIrr_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseGlobalIrr.CheckedChanged
        If Loaded Then
            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
            SimObject.UseUserDefinedWeather = Not chkUseGlobalIrr.Checked
            tbWindSpeed.Enabled = Not chkUseGlobalIrr.Checked
            tbRelHum.Enabled = Not chkUseGlobalIrr.Checked
            tbAtmPres.Enabled = Not chkUseGlobalIrr.Checked
            tbAtmTemp.Enabled = Not chkUseGlobalIrr.Checked
        End If
    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnectEnergy_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy.Click
        If cbEnergy.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbEnergy.SelectedItem = Nothing
            SimObject.FlowSheet.UpdateInterface()
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
                If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)
                SimObject.FlowSheet.UpdateInterface()

            End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectEnergy.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        If sender Is btnCreateAndConnectEnergy Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.OutputConnectors(0).Position.X - 30, sgobj.OutputConnectors(0).Position.Y + 30, "")

            If sgobj.OutputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(0).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

        End If

        UpdateInfo()

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

    Private Sub tbSolarIrr_TextChanged(sender As Object, e As EventArgs) Handles tbWindSpeed.TextChanged
        If Loaded Then
            Try
                SimObject.UserDefinedWindSpeed = tbWindSpeed.Text.ToDoubleFromCurrent().ConvertToSI(units.velocity)
                tbWindSpeed.ForeColor = Color.Blue
            Catch ex As Exception
                tbWindSpeed.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub tbPanelArea_TextChanged(sender As Object, e As EventArgs) Handles tbRotorDiameter.TextChanged
        If Loaded Then
            Try
                SimObject.RotorDiameter = tbRotorDiameter.Text.ToDoubleFromCurrent().ConvertToSI(units.distance)
                tbRotorDiameter.ForeColor = Color.Blue
            Catch ex As Exception
                tbRotorDiameter.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub tbPanelEfficiency_TextChanged(sender As Object, e As EventArgs) Handles tbPanelEfficiency.TextChanged
        If Loaded Then
            Try
                SimObject.Efficiency = tbPanelEfficiency.Text.ToDoubleFromCurrent()
                tbPanelEfficiency.ForeColor = Color.Blue
            Catch ex As Exception
                tbPanelEfficiency.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub tbNumberOfPanels_TextChanged(sender As Object, e As EventArgs) Handles tbNumberOfPanels.TextChanged
        If Loaded Then
            Try
                SimObject.NumberOfTurbines = tbNumberOfPanels.Text.ToDoubleFromCurrent()
                tbNumberOfPanels.ForeColor = Color.Blue
            Catch ex As Exception
                tbNumberOfPanels.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub tbAtmPres_TextChanged(sender As Object, e As EventArgs) Handles tbAtmPres.TextChanged
        If Loaded Then
            Try
                SimObject.UserDefinedAirPressure = tbAtmPres.Text.ToDoubleFromCurrent().ConvertToSI(units.pressure)
                tbAtmPres.ForeColor = Color.Blue
            Catch ex As Exception
                tbAtmPres.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub tbAtmTemp_TextChanged(sender As Object, e As EventArgs) Handles tbAtmTemp.TextChanged
        If Loaded Then
            Try
                SimObject.UserDefinedAirTemperature = tbAtmTemp.Text.ToDoubleFromCurrent().ConvertToSI(units.temperature)
                tbAtmTemp.ForeColor = Color.Blue
            Catch ex As Exception
                tbAtmTemp.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub tbRelHum_TextChanged(sender As Object, e As EventArgs) Handles tbRelHum.TextChanged
        If Loaded Then
            Try
                SimObject.UserDefinedRelativeHumidity = tbRelHum.Text.ToDoubleFromCurrent()
                tbRelHum.ForeColor = Color.Blue
            Catch ex As Exception
                tbRelHum.ForeColor = Color.Red
            End Try
        End If
    End Sub

End Class